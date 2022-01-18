#' geom_colorpath
#' @description lines with alternating color "just for the effect".
#' @name colorpath
#' @examples
#' air_df <- data.frame(x = 1: length(AirPassengers), y = c(AirPassengers))
#'
#' ggplot(air_df, aes(x, y)) +
#'   colorpath(cols = c("red", "blue", "green"))
#'
#' dat <- data.frame(x = seq(2,10, 2), y = seq(4,20, 4))
#'
#' p1 <- ggplot(dat, aes(x = x, y = y)) +
#'   geom_colorpath()
#' p2 <- ggplot(dat, aes(x, y)) +
#'   geom_colorpath(cols = c("red", "blue"))
#'
#' p3 <- ggplot(dat, aes(x, y)) +
#'   geom_colorpath(cols = c("red", "blue", "green"))
#'
#' p4 <- ggplot(dat, aes(x, y)) +
#'   geom_colorpath(cols = c("red", "blue", "green", "white"))
#'
#' patchwork::wrap_plots(mget(ls(pattern = "p[1-9]")))
#'
#' @export

StatColorPath <- ggproto("StatColorPath", Stat,
  compute_group = function(data, scales, params,
                           n_seg = 20, n = 100, cols = c("black", "white")) {
    # interpolate
    d <- approx(data$x, data$y, n = n)
    # create start and end points for segments
    d2 <- data.frame(
      x = head(d$x, -1), xend = d$x[-1],
      y = head(d$y, -1), yend = d$y[-1]
    )
    # create vector of segment colors
    d2$color <- rep(cols, each = ceiling((n - 1) / n_seg), length.out = n - 1)
    d2
  },
  required_aes = c("x", "y")
)

#' @rdname colorpath
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @param n_seg number of segments along line, according to taste
#' @param n number of points at which interpolation takes place
#'   increase if line takes sharp turns
#' @param cols vector of alternating colors
#' @export
geom_colorpath <- function(mapping = NULL, data = NULL, geom = "segment",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, cols = c("black", "white"),
                       n_seg = 20, n = 100, ...) {
  layer(
    stat = StatColorPath, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, cols = cols, n = n, n_seg = n_seg,...)
  )
}

#' @rdname colorpath
#' @export
#'
geom_colorpath <- function (mapping = NULL, data = NULL, stat = "ColorPath", position = "identity",
                            ..., arrow = NULL, arrow.fill = NULL, lineend = "butt", linejoin = "round",
                            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSegment,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(arrow = arrow, arrow.fill = arrow.fill,
                      lineend = lineend, linejoin = linejoin, na.rm = na.rm,
                      ...))
}

