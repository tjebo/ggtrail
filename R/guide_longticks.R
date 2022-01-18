#' discrete gradient bar with separators, not ticks
#' @name guide_longticks
#' @param ... passed to guide_colorbar
#' @description from https://stackoverflow.com/a/62558606/7941188 teunbrand
#' @examples
#' ggplot(iris, aes(Sepal.Length, y = Sepal.Width, fill = Petal.Length))+
#'   geom_point(shape = 21) +
#'   scale_fill_fermenter(breaks = c(1:3,5,7), palette = "Reds") +
#'   guides(fill = guide_longticks(
#'     ticks = TRUE,
#'     even.steps = FALSE,
#'     frame.colour = "black",
#'     ticks.colour = "black")) +
#'   theme(legend.position = "bottom")
#' @export
guide_longticks <- function(...) {
  guide <- guide_colorbar(...)
  class(guide) <- c("guide_longticks", "guide", "colorbar")
  guide
}
#' @keywords internal
guide_gengrob.guide_longticks <- function(guide, theme) {
  dir <- guide$direction
  guide <- NextMethod()
  is_ticks <- grep("^ticks$", guide$layout$name)
  ticks <- guide$grobs[is_ticks][[1]]
  n <- length(ticks$x0)
  if (dir == "vertical") {
    ticks$x0 <- ticks$x0[1]
    ticks$x1 <- ticks$x1[length(ticks$x1)]
  } else {
    ticks$y0 <- ticks$y0[1]
    ticks$y1 <- ticks$y1[length(ticks$y1)]
  }

  guide$grobs[[is_ticks]] <- ticks
  guide
}
