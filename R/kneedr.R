# KneeLocator class and methods
#' @importFrom methods setClass
#' @include transformations.R
NULL


#' KneeLocator class
#' 
#' @slot x
#' @slot y
#' @slot S
#' @slot curve
#' @slot direction
#' 
#' @name KneeLocator
#' 
#' @export

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
        interp_method = "character",
        N = "numeric",
        threshold = "numeric",
        all_knees = "vector",
        all_norm_knees = "vector",
        all_knees_y = "vector",
        all_norm_knees_y = "vector",
        maxima_indices = "vector",
        minima_indices = "vector"
  )
)


#' Initialization function for KneeLocator class
#' 
#' @param x
#' @param y
#' @param S
#' @param curve
#' @param direction
#' @export
create_knee_locator <- function(
    x, y, S = 1.0, curve = "concave", direction = "increasing", interp_method = "loess"
) {

    if (missing(x) | missing(y))
        stop("`x` and `y` must be provided.")
    if (length(x) != length(y))
        stop("Lengths of `x` and `y` do not match.")
    if (!(curve %in% c("concave", "convex")))
        stop("`curve` must be one of `{concave, convex}`.")
    if (!(direction %in% c("increasing", "decreasing")))
        stop("`direction must be one of `{increasing, decreasing}`.")

    knee_locator <- new(
        Class = "KneeLocator", 
        x = x, 
        y = y, 
        S = S, 
        curve = curve, 
        direction = direction,
        interp_method = interp_method)

    return(knee_locator)

}



#' Preprocess
#' 
#' @param object
#' @export


preprocess <- function(object) {

    # 1. Smooth the curve
    object@y.smooth  <- smooth_y(object@x, object@y, object@interp_method)

    # 2. Normalize 
    object@x.normalized <- normalize(object@x)
    object@y.normalized <- normalize(object@y.smooth)
    object@N <- length(object@x)

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

#' Find Knee
#' 
#' @param object
#' @export
#' 
find_knee <- function(object) {

    if (is.null(object@maxima_indices)) {
        warn("No local maxima found in difference curve. 
              Check curve and direction params.")
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
            y_at_knee = object@y[object@x == knee][1]
            y_norm_at_knee = object@y.normalized[
                object@x.normalized == norm_knee][1]
            if (!(knee %in% object@all_knees)){
                object@all_knees_y = c(object@all_knees_y, y_at_knee)
                object@all_norm_knees_y = c(object@all_norm_knees_y, y_norm_at_knee)
            }
            # now add the knee
            object@all_knees = c(object@all_knees, knee)
            object@all_norm_knees = c(object@all_norm_knees, norm_knee)
        }

    }

    if (length(object@all_knees) == 0)
        stop("No knees found.")

    return(list(knee, norm_knee))
}
