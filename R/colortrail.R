library(ggplot2)
library(patchwork)
#' stat_colortrail
#' @description lines with alternating color "just for the effect".
#' @name stat_colortrail
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @param alt_cols vector of alternating colors
#' @examples
#' air_df <- data.frame(x = 1: length(AirPassengers), y = c(AirPassengers))
#'
#' ggplot(air_df, aes(x, y)) +
#'   stat_colortrail(alt_cols = c("red", "blue", "green"))
#'
#' dat <- data.frame(x = seq(2,10, 2), y = seq(4,20, 4))
#'
#' p1 <- ggplot(dat, aes(x, y)) +
#'   stat_colortrail(alt_cols = c("red", "blue"))
#'
#' p2 <- ggplot(dat, aes(x, y)) +
#'   stat_colortrail(alt_cols = c("red", "blue", "green"))
#'
#' p3 <- ggplot(dat, aes(x, y)) +
#'   stat_colortrail(alt_cols = c("red", "blue", "green", "white"))
#'
#' p1 + p2 + p3
#' @export

StatColorTrail <- ggproto("StatColorTrail", Stat,
  compute_group = function(data, scales, params, alt_cols = c("red", "green"),
                           rep_length = 1) {
    ## a function to create modifiable cuts in order to get segments.
    ## this looks convoluted - and it is! there are a few if/else statements.
    ## Why? The assigment of new y to x values depends on how many original values
    ## you have.
    ## There might be more direct ways to get there
      x <- data$x
      y <- data$y
      ## create new x for each tiny segment
      length_seg <- rep_length / length(alt_cols)
      new_x <- seq(min(x, na.rm = TRUE), x[length(x)], length_seg)
      ## now we need to interpolate y values for each new x
      ## This is different depending on how many x and new x you have
      if (length(new_x) < length(x)) {
        ind_int <- findInterval(new_x, x)
        new_y <- sapply(seq_along(ind_int), function(i) {
          if (y[ind_int[i]] == y[ind_int[length(ind_int)]]) {
            y[ind_int[i]]
          } else {
            seq_y <- seq(y[ind_int[i]], y[ind_int[i] + 1], length.out = length(alt_cols))
            head(seq_y, -1)
          }
        })
      } else {
        ind_int <- findInterval(new_x, x)
        rle_int <- rle(ind_int)
        new_y <- sapply(rle_int$values, function(i) {
          if (y[i] == y[max(rle_int$values)]) {
            y[i]
          } else {
            seq_y <- seq(y[i], y[i + 1], length.out = rle_int$lengths[i] + 1)
            head(seq_y, -1)
          }
        })
      }
      ## THis is also a bit painful and might cause other bugs that I haven't
      ## discovered yet.
      if (length(unlist(new_y)) < length(new_x)) {
        newdat <- data.frame(
          x = new_x,
          y = rep_len(unlist(new_y), length.out = length(new_x))
        )
      } else {
        newdat <- data.frame(x = new_x, y = unlist(new_y))
      }
      newdat <- dplyr::mutate(newdat, xend = dplyr::lead(x), yend = dplyr::lead(y))
      newdat <- tidyr::drop_na(newdat, xend)
      newdat$color <- alt_cols
      newdat
    },
  required_aes = c("x", "y")

)

stat_colortrail <- function(mapping = NULL, data = NULL, geom = "segment",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, alt_cols = c("red", "green"),
                       rep_length = 1, ...) {
  layer(
    stat = StatColorTrail, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, alt_cols = alt_cols, rep_length= rep_length,...)
  )
}


