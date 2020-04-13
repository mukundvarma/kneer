# Utility functions for transformations.
#' @importFrom stats loess smooth.spline
NULL

#' Transform x, y
#' 
#' Transform provided x and y coordinates into a concave increasing curve, to simplify
#' downstream calculations
#' @param x x coordinate
#' @param y y coordinate
#' @param direction direction of the curve
#' @param curve curvature - convex or concave
#' @examples transform_xy(x = 1:10, 
#'               y = sapply(1:10, function(x) x**2), direction = "increasing", curve = "convex")
#' @export 
transform_xy <- function(x, y, direction='increasing', curve='concave'){
    if (curve == "convex") {
        x <- max(x) - x
        y <- max(y) - y
    }

    if (direction == "decreasing"){
        y <- rev(y)
    }

    if (curve == "convex") {
        x <- rev(x)
        y <- rev(y)
    }
    list(x=x, y=y)
}

#' Normalize.
#' 
#' Min-max scale a 1-D vector
#' 
#' @param x 1-d vector
#' @examples normalize(x = 1:10)
#' @export 
normalize <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

#' Smooth a curve given x and y coordinates.
#' 
#' @param x x coordinate
#' @param y y coordinate
#' @param method Method to use for smoothing the curve. One of "loess" or "smooth.spline"
#' @examples smooth_y (x = 1:10, y = sapply(1:10, function(x) x**2) + runif(10, -1, 1) , method = "loess")
#' @export 
smooth_y <- function(x, y, method = "loess") {
    if (method == "loess") {
        fit <- loess(y ~ x)
        return(fit$fitted)
    }
    else if (method == "smooth.spline") {
        fit <- smooth.spline(x = x, y = y, df=3)
        return(fit$y)
    }
    else {
        return(y)
    }
}

#' Calculate maxima of a 1-D vector.
#' 
#' Adapted from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
#' 
#' @param x one dimensional vector of floats or ints
#' @examples maxima(x = c(0, 1, 2, 3, 1, 0))
#' @export 
maxima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(-Inf, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#' Calculate minima of a 1-D vector.
#' 
#' Adapted from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
#' 
#' @param x one dimensional vector of floats or ints
#' @examples minima(x = c(0, 1, 2, 3, 1, 0))
#' @export 
minima <- function(x) {
  # Use -Inf instead if x is numeric (non-integer)
  y <- diff(c(Inf, x)) < 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}
