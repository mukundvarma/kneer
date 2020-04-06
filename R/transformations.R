#' @importFrom stats loess smooth.spline
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

#' @export
normalize <- function(a) {
    (a - min(a)) / (max(a) - min(a))
}

#' @export
smooth_y <- function(x, y, method = "loess") {
    if (method == "loess") {
        fit <- loess(y ~ x)
        return(fit$fitted)
    }
    if (method == "smooth.spline") {
        fit <- smooth.spline(x = x, y = y)
        return(fit$y)
    }
}

# https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
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
