#' @importFrom stats loess smooth.spline

transform_xy <- function(x, y, direction='increasing', curve='concave'){
    if (curve == "convex") {
        x <- max(x) - x
        y <- max(y) - y
    }

    if (direction == "decreasing")
        y <- rev(y)

    if (curve == "convex") {
        x <- rev(x)
        y <- rev(y)
    }
    list(x, y)
}


normalize <- function(a) {
    a-min(a))/(max(a) - min(a))
}


smooth_y <- function(x, y, method = "loess") {
    if (method == "loess") {
        fit <- loess(y~x)
        fit$fitted
    }
    if (method == "smooth.spline") {
        fit <- smooth.spline(x=x, y=y)
        return(fitted$y)
    }
}

minima <- function(y) {

}

maxima <- function(y){

}
