# KneeLocator class and methods.

#' @importFrom methods setClass new
#' @include transformations.R
NULL


#' KneeLocator class
#' 
#' The KneeLocator class stores the input data, intermediate calculations and
#' results of all computations.
#' 
#' @slot x x coordinates
#' @slot y y coordinates
#' @slot S Sensitivity parameter. Smaller values detect knees quicker,
#'       while larger values are more conservative.
#' @slot smooth Method for smoothing the curve - either smooth.spline or loess 
#' @slot curve convex or concave
#' @slot direction decreasing or increasing
#' 
#' @name KneeLocator
#' @rdname kneer
#' @aliases KneeLocator,KneeLocator-class
#' @exportClass KneeLocator
#' 
KneeLocator <- setClass(
    Class = "KneeLocator",
    slots = list(
        x = "vector",
        y = "vector",
        y.smooth = "vector",
        x.normalized = "vector",
        y.normalized = "vector",
        x.difference = "vector",
        y.difference = "vector",
        x.difference.maxima = "vector",
        y.difference.maxima = "vector",
        x.difference.minima = "vector",
        y.difference.minima = "vector",
        S = "numeric",
        curve = "character",
        direction = "character",
        smooth = "character",
        N = "numeric",
        threshold = "numeric",
        all_knees = "vector",
        all_norm_knees = "vector",
        all_knees_y = "vector",
        all_norm_knees_y = "vector",
        maxima_indices = "vector",
        minima_indices = "vector",
        knee="numeric",
        y_at_knee="numeric"
    )
)


#' Initialization function for KneeLocator class
#' 
#' @param x x coordinates
#' @param y y coordinates
#' @param S Sensitivity parameter. Smaller values detect knees quicker,
#'          while larger values are more conservative.
#' @param smooth Method for smoothing the curve - either smooth.spline or loess 
#' @examples knee_locator <- create_knee_locator(
#' 			     x = 1:100, y=sapply(1:100, function (x) x**2 - 2*x), smooth="loess")
#' @export
create_knee_locator <- function(
    x, y, 
    S = 1.0, 
    smooth = "loess"
) {

    if (missing(x) | missing(y))
        stop("`x` and `y` must be provided.")
    if (length(x) != length(y))
        stop("Lengths of `x` and `y` do not match.")

    N <- length(x)

    direction = ifelse(y[1] > y[N], "decreasing", "increasing")
    curve = ifelse(y[floor(N/2)] > abs((y[N] + y[1]) / 2), "concave", "convex")

    knee_locator <- new(
        Class = "KneeLocator", 
        x = x, 
        y = y, 
        S = S, 
        curve = curve, 
        direction = direction,
        N = N,
        smooth = smooth)

    return(knee_locator)

}


#' Preprocess
#' 
#' The preprocess function calculates intermediate values for coordinates that are used
#' in downstream calculations of the knee. This includes 1) smoothing the y values; 
#' 2) normalizing the curve to be between 0 and 1; 3) transforms the curve to look like a concave
#' increasing curve that is easy to do computations on; 4) calculate a difference curve - the
#' extrema of which will be used to find the knees; 5) finds these extrema; 6) calculates a 
#' threshold for drop off in values - which is used in determining if a knee has been found.
#' 
#' @param object KneeLocator object.
#' @examples knee_locator <- create_knee_locator(x=1:10, y=sapply(1:10, function(x) x**3)); 
#' 			 knee_locator <- preprocess(knee_locator)
#' @export

preprocess <- function(object) {

    # 1. Smooth the curve
    object@y.smooth  <- smooth_y(object@x, object@y, object@smooth)

    # 2. Normalize 
    object@x.normalized <- normalize(object@x)
    object@y.normalized <- normalize(object@y.smooth)

    transformed <- transform_xy(object@x.normalized, object@y.normalized, 
        curve=object@curve, direction=object@direction)
    object@x.normalized <- transformed$x
    object@y.normalized <- transformed$y

    # 3. Calculate difference curve
    object@x.difference <- object@x.normalized
    object@y.difference <- object@y.normalized - object@x.normalized

    # 4. Find Extrema of difference curve
    ## 4.1 Maxima
    object@maxima_indices <- maxima(object@y.difference)

    object@x.difference.maxima <- object@x.difference[object@maxima_indices]
    object@y.difference.maxima <- object@y.difference[object@maxima_indices]
    
    ## 4.2 Minima
    object@minima_indices <- minima(object@y.difference)
    
    object@x.difference.minima <- object@x.difference[object@minima_indices]
    object@y.difference.minima <- object@y.difference[object@minima_indices]

    ## 5. Calculate threshold
    object@threshold <- object@y.difference.maxima - 
        object@S * abs(mean(diff(object@x.difference)))
    
    return(object)
}

#' Find Knee.
#' 
#' This function runs on the preprocessed KneeLocator object, and finds knees using the
#' Kneedle algorithm by Satopaa et al. In a nutshell, it works by finding points of maximum
#' curvature in a curve by finding extrema of the difference curve.
#' 
#' @param object KneeLocator object.
#' @examples kl <- create_knee_locator(x=1:10, y=sapply(1:10, function(x) x**3));
#' 			 kl <- preprocess(kl); kl <- find_knee(kl)
#' @export

find_knee <- function(object) {

    if (is.null(object@maxima_indices)) {
        warning("No local maxima found in difference curve. Check that preprocess has been run.")
    }

    maxima_threshold_index <- 1
    minima_threshold_index <- 1
    # traverse the difference curve
    for (i in seq_along(object@x.difference)) {
        # skip points on the curve before the the first local maxima
        if (i < object@maxima_indices[1])
            next
         
        # reached the end of the curve
        if (object@x.difference[i] == 1.0)
            break
        
        # if we're at local max, increment the maxima threshold index and continue
        if (i %in% object@maxima_indices) {
            threshold <- object@threshold[maxima_threshold_index]
            threshold_index <- i
            maxima_threshold_index <- maxima_threshold_index + 1
        }

        # values in difference curve are at or after a local minimum
        if (i %in% object@minima_indices) {
            threshold <- 0.0
            minima_threshold_index <- minima_threshold_index + 1
        }

        is_concave <- object@curve == "concave"
        is_decreasing <- object@direction == "decreasing"

        if (object@y.difference[i + 1] < threshold) {
            # xor(T, T) = F, xor(T, F) = F etc.
            if (xor(is_concave, is_decreasing)) {
                # triggered for {concave, increasing} and {convex, decreasing}
                knee <- object@x[threshold_index]
                norm_knee <- object@x.normalized[threshold_index]
            } else {
                # triggered for {convex, increasing} and {concave, decreasing}
                knee <- object@x[object@N - threshold_index + 1]
                norm_knee <- object@x.normalized[object@N - 
                                                 threshold_index + 1]
            }

            # add the y value at the knee
            y_at_knee <- object@y[object@x == knee][1]
            y_norm_at_knee <- object@y.normalized[object@x.normalized == norm_knee][1]
            if (!(knee %in% object@all_knees)){
                object@all_knees_y <- append(object@all_knees_y, y_at_knee)
                object@all_norm_knees_y <- append(object@all_norm_knees_y, y_norm_at_knee)
                object@all_knees <- append(object@all_knees, knee)
                object@all_norm_knees <- append(object@all_norm_knees, norm_knee)
        
            }
        }

    }

    if (length(object@all_knees) == 0)
        stop("No knees found.")

    object@knee <- knee
    object@y_at_knee <- y_at_knee

    return(object)
}


#' KneeLocator pipeline
#' 
#' This function runs the entire knee locator pipeline, from creation to preprocessing to
#' finding knees in data. It takes the same inputs as create_knee_locator.
#' @param x x coordinates
#' @param y y coordinates
#' @param S Sensitivity parameter. Smaller values detect knees quicker,
#'          while larger values are more conservative.
#' @param smooth Method for smoothing the curve - either smooth.spline or loess 
#' @examples kl <- knee_locator_pipeline(x=1:10, y=sapply(1:10, function(x) x**3))
#' @export

knee_locator_pipeline <- function(x, y, S = 1.0, smooth = "loess") {
    knee_locator <- create_knee_locator(x, y, S = S, smooth = smooth)
    knee_locator <- preprocess(knee_locator)
    knee_locator <- find_knee(knee_locator)
    return(knee_locator)
}
