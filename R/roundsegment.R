#' geom_roundsegment
#' @name roundsegment
#' @author Allan Cameron
#' @import ggplot2
#' @import grid
#' @import scales
#' @seealso Stackoverflow thread https://stackoverflow.com/questions/77494430/how-to-draw-segment-with-round-end-that-does-not-extend-beyond-values
#' @examples
#' df <- data.frame(x = 0, xend = 1, y = 0, yend = 1)
#' ggplot(df, aes(x, y)) +
#'   geom_roundseg(aes(xend = xend, yend = yend),
#'                 linewidth = 6, alpha = 0.5)  +
#'   geom_point() +
#'   geom_point(aes(xend, yend))
#'
#'ggplot(df, aes(x, y)) +
#' geom_vline(xintercept = c(0, 1), lty = 2) +
#' geom_roundseg(aes(xend = xend, yend = yend),
#'                 linewidth = 6, alpha = 0.5)
#'
#'ggplot(df, aes(x, y)) +
#'geom_roundseg(aes(xend = xend, yend = yend),
#'              linewidth = 1, alpha = 0.5)  +
#'  geom_point() +
#'  geom_point(aes(xend, yend))
#'
#'ggplot(df, aes(x, y)) +
#'  geom_roundseg(aes(xend = xend, yend = yend),
#'                linewidth = 30, alpha = 0.5)  +
#'  geom_point() +
#'  geom_point(aes(xend, yend))
#'
#'df_angle <- data.frame(x = 0, xend = 1, y = 0, yend = 1)
#'ggplot(df_angle, aes(x, y)) +
#'  geom_roundseg(aes(xend = xend, yend = yend),
#'                linewidth = 6, alpha = 0.5)  +
#'  geom_point() +
#'  geom_point(aes(xend, yend))
#'
#'
#' @export
#'
GeomRoundseg <- ggproto("GeomRoundseg", GeomSegment,
                        draw_panel = function (self, data, panel_params, coord, arrow = NULL,
                                               arrow.fill = NULL, linejoin = "round", na.rm = FALSE)
                        {
                          data <- check_linewidth(data, snake_class(self))
                          data <- remove_missing(data, na.rm = na.rm, c("x", "y", "xend",
                                                                                  "yend", "linetype", "linewidth", "shape"),
                                                           name = "geom_roundseg")
                          if (empty(data))
                            return(zeroGrob())
                          if (coord$is_linear()) {
                            coord <- coord$transform(data, panel_params)
                            arrow.fill <- `%||%`(arrow.fill, coord$colour)
                            sg <- grid::segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                                                     default.units = "native",
                                                     gp = grid::gpar(col = scales::alpha(coord$colour,
                                                                                         coord$alpha), fill = scales::alpha(arrow.fill, coord$alpha),
                                                                     lwd = coord$linewidth * .pt, lty = coord$linetype,
                                                                     lineend = "round", linejoin = linejoin), arrow = arrow)
                            class(sg) <- c("roundseg", class(sg))
                            return(sg)
                          }
                          data$group <- 1:nrow(data)
                          starts <- subset(data, select = c(-xend, -yend))
                          ends <- rename(subset(data, select = c(-x, -y)), c(xend = "x",
                                                                             yend = "y"))
                          pieces <- vec_rbind0(starts, ends)
                          pieces <- pieces[order(pieces$group), ]
                          GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
                                              lineend = lineend)
                        })

#' @rdname roundsegment
#' @description Round segments that end where they should.
#' @author Allan Cameron
#' @import ggplot2
#' @importFrom rlang list2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @export
#'
geom_roundseg <- function (mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", ..., arrow = NULL, arrow.fill = NULL,
                           linejoin = "round", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomRoundseg,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(arrow = arrow, arrow.fill = arrow.fill,
                              linejoin = linejoin, na.rm = na.rm, ...))
}

#' @rdname roundsegment
#' @importFrom grid convertX
#' @importFrom grid convertY
#' @author Allan Cameron
#' @export
#'
makeContent.roundseg <- function(x) {
## modified units cm to points from Allan's original code
  x$x0 <- grid::convertX(x$x0, "points")
  x$x1 <- grid::convertX(x$x1, "points")
  x$y0 <- grid::convertY(x$y0, "points")
  x$y1 <- grid::convertY(x$y1, "points")
  xmin <- pmin(as.numeric(x$x0), as.numeric(x$x1))
  xmax <- pmax(as.numeric(x$x0), as.numeric(x$x1))
  ymin <- pmin(as.numeric(x$y0), as.numeric(x$y1))
  ymax <- pmax(as.numeric(x$y0), as.numeric(x$y1))
  theta <- atan2(ymax - ymin, xmax - xmin)
  ## modified constant to sqrt(2) from Allan's code
  size <- sqrt(2) * x$gp$lwd / .stroke
  xmin <- xmin + cos(theta) * size
  xmax <- xmax - cos(theta) * size
  ymin <- ymin + sin(theta) * size
  ymax <- ymax - sin(theta) * size
  x$x0 <- unit(xmin, "points")
  x$x1 <- unit(xmax, "points")
  x$y0 <- unit(ymin, "points")
  x$y1 <- unit(ymax, "points")
  return(x)
}

