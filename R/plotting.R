# Plotting module for kneer.
#' 
#' @importFrom ggplot2 ggproto layer Stat GeomLine aes
NULL

#' Stat Knee X
#' 
#' Class for when the x coordinate of knee needs to be drawn.
#' @export 
StatKneeX <- ggproto("StatKnee", Stat,
	required_aes = c("x", "y"),
	default_aes = aes(colour="red", linetype=3, size=0.5),
    compute_group = function(data, scales, params, S=1.0, smooth="loess") {

        knee_locator <- knee_locator_pipeline(data$x, data$y, S=1.0, smooth=smooth)
    	knee <- knee_locator@knee
    	knee_line <- data.frame(x=rep(knee, length(data$x)), y=data$y)
        knee_line
    }
)


#' Stat Knee Y
#' 
#' Class for when the y coordinate of knee needs to be drawn.
#' @export 
StatKneeY <- ggproto("StatKnee", Stat,
	required_aes = c("x", "y"),
	default_aes = aes(colour="red", linetype=3, size=0.5),
    compute_group = function(data, scales, params, S=1.0, smooth="loess") {
        knee_locator <- knee_locator_pipeline(data$x, data$y, S=1.0, smooth=smooth)
    	y_at_knee <- knee_locator@y_at_knee
  	    knee_line <- data.frame(x=data$x, y=rep(y_at_knee, length(data$x)))
        knee_line
    }
)


#' stat_knee
#' 
#' ggplot layer for calculating the knee and overlaying it on data.
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}} or
#'   \code{\link[ggplot2]{aes_}}. If specified and \code{inherit.aes = TRUE} (the
#'   default), is combined with the default mapping at the top level of the
#'   plot. You only need to supply \code{mapping} if there isn't a mapping
#'   defined for the plot.
#' @param data A data frame. If specified, overrides the default data frame
#'   defined at the top level of the plot.
#' @param geom ggplot geom layer.
#' @param position Position adjustment, either as a string, or the result of
#'  a call to a position adjustment function.
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#'   \code{FALSE} never includes, and \code{TRUE} always includes.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. \code{\link[ggplot2]{borders}}.
#' @param S Sensitivity parameter. Smaller values detect knees quicker,
#'          while larger values are more conservative.
#' @param smooth Method for smoothing the curve - either smooth.spline or loess 
#' @param cutoff.dim Whether to show knee cutoff for x coordinate or y coordinate.
#' @param ... other arguments passed on to \code{\link[ggplot2]{layer}}. There are
#'   three types of arguments you can use here:
#'
#' @export
stat_knee <- function(mapping = NULL, data = NULL, geom = GeomLine,
                      position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                      S = 1.0, smooth = "loess", cutoff.dim = "y", ...) {
	if (cutoff.dim == "y") {
		stat = StatKneeY
	} else {
		stat = StatKneeX
	}
    layer(
        stat = stat, data = data, mapping = mapping, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, S = 1.0, smooth = "loess", ...)
    )
}