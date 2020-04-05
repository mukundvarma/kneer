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
KneeLocator <- setClass(
    Class = "KneeLocator",
    slots = c(
        x = "vector",
        y = "vector",
        S = "numeric",
        curve = "character",
        direction = "character",
        N = "numeric",
        all_knees = "list",
        all_norm_knees = "list",
        all_knees_y = "list",
        all_norm_knees_y = "list",

    )
  )


#' Initialization function for KneeLocator class
#' 
#' @param x
#' @param y
#' @param S
#' @param curve
#' @param direction
create_knee_locator <- function(
    x, y, S = 1.0, curve = "concave", direction = "increasing"
) {

    if (missing(x=x) | missing(y=y))
        stop("`x` and `y` must be provided.")
    if (length(x) != length(y))
        stop("Lengths of `x` and `y` do not match.")
    if (!(curve %in% c("concave", "convex")))
        stop("`curve` must be one of `{concave, convex}`.")
    if (!(direction %in% c("increasing", "decreasing")))
        stop("`direction must be one of `{increasing, decreasing}`.")

    knee_locator <- new(
        class = "KneeLocator", 
        x = x, 
        y = y, 
        S = S, 
        curve = curve, 
        direction = direction)

    return(knee_locator)

}
