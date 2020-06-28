#' Binning scale constructor
#' @description non-exported function from ggplot2
#' @author Hadley Wickham
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param scale_name The name of the scale that should be used for error messages
#'   associated with this scale.
#' @param palette A palette function that when called with a numeric vector with
#'   values between 0 and 1 returns the corresponding output values
#'   (e.g., [scales::area_pal()]).
#' @param name The name of the scale. Used as the axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     [transformation object][scales::trans_new()]
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output (e.g., a function returned by [scales::extended_breaks()])
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (one minor break between
#'     each major break)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks.
#' @param n.breaks An integer guiding the number of major breaks. The algorithm
#'   may choose a slightly different number to ensure nice break labels. Will
#'   only have an effect if `breaks = waiver()`. Use `NULL` to use the default
#'   number of breaks given by the transformation.
#' @param labels One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels computed by the
#'     transformation object
#'   - A character vector giving labels (must be same length as `breaks`)
#'   - A function that takes the breaks as input and returns labels
#'     as output
#' @param limits One of:
#'   - `NULL` to use the default scale range
#'   - A numeric vector of length two providing limits of the scale.
#'     Use `NA` to refer to the existing minimum or maximum
#'   - A function that accepts the existing (automatic) limits and returns
#'     new limits
#'   Note that setting limits on positional scales will **remove** data outside of the limits.
#'   If the purpose is to zoom, use the limit argument in the coordinate system
#'   (see [coord_cartesian()]).
#' @param rescaler A function used to scale the input values to the
#'   range \[0, 1]. This is always [scales::rescale()], except for
#'   diverging and n colour gradients (i.e., [scale_colour_gradient2()],
#'   [scale_colour_gradientn()]). The `rescaler` is ignored by position
#'   scales, which always use [scales::rescale()].
#' @param oob One of:
#'   - Function that handles limits outside of the scale limits
#'   (out of bounds).
#'   - The default ([scales::censor()]) replaces out of
#'   bounds values with `NA`.
#'   - [scales::squish()] for squishing out of bounds values into range.
#'   - [scales::squish_infinite()] for squishing infinite values into range.
#' @param na.value Missing values will be replaced with this value.
#' @param trans For continuous scales, the name of a transformation object
#'   or the object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "date", "exp", "hms", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "modulus", "probability", "probit", "pseudo_log", "reciprocal",
#'   "reverse", "sqrt" and "time".
#'
#'   A transformation object bundles together a transform, its inverse,
#'   and methods for generating breaks and labels. Transformation objects
#'   are defined in the scales package, and are called `<name>_trans` (e.g.,
#'   [scales::boxcox_trans()]). You can create your own
#'   transformation with [scales::trans_new()].
#' @param guide A function used to create a guide or its name. See
#'   [guides()] for more information.
#' @param expand For position scales, a vector of range expansion constants used to add some
#'   padding around the data to ensure that they are placed some distance
#'   away from the axes. Use the convenience function [expansion()]
#'   to generate the values for the `expand` argument. The defaults are to
#'   expand the scale by 5% on each side for continuous variables, and by
#'   0.6 units on each side for discrete variables.
#' @param position For position scales, The position of the axis.
#' `left` or `right` for y axes, `top` or `bottom` for x axes.
#' @param super The super class to use for the constructed scale
#' @param n.breaks The number of break points to create if breaks are not given
#'   directly.
#' @param nice.breaks Logical. Should breaks be attempted placed at nice values
#'   instead of exactly evenly spaced between the limits. If `TRUE` (default)
#'   the scale will ask the transformation object to create breaks, and this
#'   may result in a different number of breaks than requested. Ignored if
#'   breaks are given explicetly.
#' @param right Should values on the border between bins be part of the right
#'   (upper) bin?
#' @param show.limits should the limits of the scale appear as ticks
#' @importFrom scales as.trans
#' @keywords internal
binned_scale <- function(aesthetics, scale_name, palette, name = waiver(),
                         breaks = waiver(), labels = waiver(), limits = NULL,
                         rescaler = rescale, oob = squish, expand = waiver(),
                         na.value = NA_real_, n.breaks = NULL, nice.breaks = TRUE,
                         right = TRUE, trans = "identity", show.limits = FALSE,
                         guide = "bins", position = "left", super = ScaleBinned) {

  aesthetics <- standardise_aes_names(aesthetics)

  check_breaks_labels(breaks, labels)

  position <- match.arg(position, c("left", "right", "top", "bottom"))

  if (is.null(breaks) && !is_position_aes(aesthetics) && guide != "none") {
    guide <- "none"
  }

  trans <- scales::as.trans(trans)
  if (!is.null(limits)) {
    limits <- trans$transform(limits)
  }

  ggproto(NULL, super,
          call = match.call(),

          aesthetics = aesthetics,
          scale_name = scale_name,
          palette = palette,

          range = continuous_range(),
          limits = limits,
          trans = trans,
          na.value = na.value,
          expand = expand,
          rescaler = rescaler,
          oob = oob,
          n.breaks = n.breaks,
          nice.breaks = nice.breaks,
          right = right,
          show.limits = show.limits,

          name = name,
          breaks = breaks,

          labels = labels,
          guide = guide,
          position = position
  )
}


#' Standardise aesthetic names
#'
#' This function standardises aesthetic names by converting `color` to `colour`
#' (also in substrings, e.g. `point_color` to `point_colour`) and translating old style
#' R names to ggplot names (eg. `pch` to `shape`, `cex` to `size`).
#' @param x Character vector of aesthetics names, such as `c("colour", "size", "shape")`.
#' @return Character vector of standardised names.
#' @keywords internal
#' @export
standardise_aes_names <- function(x) {
  # convert US to UK spelling of colour
  x <- sub("color", "colour", x, fixed = TRUE)

  # convert old-style aesthetics names to ggplot version
  revalue(x, .base_to_ggplot)
}

# Aesthetic aliases
# (In the future, .base_to_ggplot should be removed in favor
# of direct assignment to ggplot_global$base_to_ggplot, see below.)
.base_to_ggplot <- c(
  "col"   = "colour",
  "color" = "colour",
  "pch"   = "shape",
  "cex"   = "size",
  "lty"   = "linetype",
  "lwd"   = "size",
  "srt"   = "angle",
  "adj"   = "hjust",
  "bg"    = "fill",
  "fg"    = "colour",
  "min"   = "ymin",
  "max"   = "ymax"
)

#' Replace specified values with new values, in a factor or character vector
#'
#' An easy to use substitution of elements in a string-like vector (character or
#' factor). If `x` is a character vector the matching elements will be replaced
#' directly and if `x` is a factor the matching levels will be replaced
#'
#' @param x A character or factor vector
#' @param replace A named character vector with the names corresponding to the
#' elements to replace and the values giving the replacement.
#'
#' @return A vector of the same class as `x` with the given values replaced
#' @importFrom rlang abort
#' @keywords internal
#' @noRd
#'
revalue <- function(x, replace) {
  if (is.character(x)) {
    replace <- replace[names(replace) %in% x]
    if (length(replace) == 0) return(x)
    x[match(names(replace), x)] <- replace
  } else if (is.factor(x)) {
    lev <- levels(x)
    replace <- replace[names(replace) %in% lev]
    if (length(replace) == 0) return(x)
    lev[match(names(replace), lev)] <- replace
    levels(x) <- lev
  } else if (!is.null(x)) {
    abort("x is not a factor or character vector")
  }
  x
}

check_breaks_labels <- function(breaks, labels) {
  if (is.null(breaks)) {
    return(TRUE)
  }
  if (is.null(labels)) {
    return(TRUE)
  }

  bad_labels <- is.atomic(breaks) && is.atomic(labels) &&
    length(breaks) != length(labels)
  if (bad_labels) {
    abort("`breaks` and `labels` must have the same length")
  }

  TRUE
}

# Look up the scale that should be used for a given aesthetic
aes_to_scale <- function(var) {
  var[var %in% c("x", "xmin", "xmax", "xend", "xintercept")] <- "x"
  var[var %in% c("y", "ymin", "ymax", "yend", "yintercept")] <- "y"

  var
}

# Figure out if an aesthetic is a position aesthetic or not
is_position_aes <- function(vars) {
  aes_to_scale(vars) %in% c("x", "y")
}


