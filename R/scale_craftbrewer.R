#' Extended brewer palettes
#' @name scale_craftbrewer
#' @description extended brewer palettes
#' @param type one of "seq", "div" or "qual"
#' @param palette which brewer palette
#' @param direction reverses direction, 1 or -1
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(mpg, disp, fill = hp)) +
#'  geom_point(shape = 21) +
#'  scale_fill_craftfermenter(
#'    breaks = seq(0,520,40),
#'    limits = c(0,520),
#'    palette = "Spectral",
#'    guide=  guide_colorsteps(even.steps = FALSE, # workaround for issues #4019/#4100
#'                             barheight = 15)
#'  )
scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "craftfermenter", binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

#' @rdname scale_craftbrewer
#' @description extended brewer palettes
#' @export

scale_color_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "color") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  binned_scale(aesthetics, "craftfermenter", binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

#' Extended brewer palettes
#' @description extended brewer palettes
#' @param type one of "seq", "div" or "qual"
#' @param palette which brewer palette
#' @param direction reverses direction, 1 or -1
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang warn
#'
#'
craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) {
  pal <- pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- brewermax[names(brewermax) == palette]

    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette generated based on all colors of",
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

#' from ggplot2 non-exported function
binned_pal <- function(palette) {
  function(x) {
    palette(length(x))
  }
}
