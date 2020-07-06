
#' geom_trail
#' @description Mark a trail with a plot just like the base plot type = "b".
#' You can also leave the dots blank, thus allowing use of text instead of
#' points (see examples).
#' @name geom_trail
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::geom_text
#' @param gap between points and lines, default 0.7 for type = "point"
#'     and 0.4 for type = "text
#' @param type "point" (default) or "text".
#' @section Aesthetics:
#' `geom_trail` understands the following aesthetics (required aesthetics
#' are in bold):
#'
#'   - **`x`**
#'   - **`y`**
#'   - `alpha`
#'   - `color`
#'   - `linetype`
#'   - `size` when type = "text", default to font size of 10pt. Size for points
#'      uses ggplot somewhat weird point sizes. Size for text uses factor
#'      * 5/14 in order to result in font size (in pt)
#'   - `linesize`
#'   - **(`label`)** required when type = "text"
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(eye)
#'
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_ribbon(aes(ymin = pressure - 50, ymax = pressure + 50), alpha = 0.2) +
#'   geom_trail()
#'
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_ribbon(aes(ymin = pressure - 50, ymax = pressure + 50), alpha = 0.2) +
#'   geom_trail(type = "text", size = 8*5/14)
#'
#' amd_aggr <-
#' amd %>%
#'   group_by(
#'     age_cut10 = cut_width(BaselineAge, 10),
#'     days_cut90 = cut_width(FollowupDays, 90, labels = seq(0, 810, 90))
#'   ) %>%
#'   summarise(mean_va = mean(VA_ETDRS_Letters))
#'
#' p <- ggplot(amd_aggr, aes(days_cut90, mean_va, color = age_cut10)) +
#'        theme_classic() +
#'        labs(
#'          x = "Follow up time [Days]", y = "Mean VA [ETDRS letters]",
#'          color = "Age strata"
#'        )
#'
#' p + geom_trail(aes(group = age_cut10))
#'
#' p +
#'   geom_trail(aes(group = age_cut10, label = round(mean_va)),
#'              type = "text", size = 8*5/14, gap = .3)
#' @seealso
#' The geom was modified from the suggestion by user teunbrand on
#' Stackoverflow in
#' [this thread](https://stackoverflow.com/a/55857158/7941188)
#' @export

geom_trail <-
  function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
            na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, type = "point",
            parse = FALSE,  check_overlap = FALSE, ...) {
    layer(data = data, mapping = mapping, stat = stat, geom = GeomTrail,
          position = position, show.legend = show.legend,
           inherit.aes = inherit.aes,
          params = list(na.rm = na.rm,
                        parse = parse, type = type,
                        check_overlap = check_overlap, ...))
  }

#' @rdname geom_trail
#' @import grid
#' @import ggplot2
#' @importFrom rlang warn
#' @importFrom stats complete.cases
#' @importFrom stats ave
#'
#' @export

GeomTrail <- ggplot2::ggproto(
  "GeomTrail", ggplot2::GeomPoint,


  default_aes = ggplot2::aes(
    shape = 19, colour = "black", gap = .7, size = 1.5, fill = NA,
    alpha = NA, stroke = 0.5,
    linesize = 0.5, linetype = 1, label = NA, angle = 0,
    hjust = 0.5, vjust = 0.5, family = "", fontface = 1, lineheight = 1.2
  ),

  use_defaults = function(self, data, params = list(), modifiers = aes()) {

    new_data <- ggproto_parent(GeomPath, self)$use_defaults(
      data, params, modifiers
    )
    if (any(!is.na(new_data$label))) {
      if(is.null(params$size)){
        new_data$size <- 8* 1/.pt
      } else {
        new_data$size <- params$size * 1/.pt
      }
    }
    new_data
    # browser()
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, type,
                        parse, check_overlap,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    if (!anyDuplicated(data$group)) {
      ggplot2:::message_wrap("geom_path: Each group consists of only one observation. ",
                   "Do you need to adjust the group aesthetic?")
    }

    ##Default geom point behaviour
    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }
    coords <- coord$transform(data, panel_params)

    my_points <-
      switch(type, point = ggproto_parent(GeomPoint, self)$draw_panel(
      data, panel_params, coord, na.rm = na.rm
    ), text = ggproto_parent(GeomText, self)$draw_panel(
      data, panel_params, coord, na.rm = na.rm,
      parse = parse, check_overlap = check_overlap
    )
      )
    ##must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    ##Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]

    if (nrow(munched) < 2) {
      my_path <- zeroGrob()
    } else {

    munched <- transform(munched,
                         xend = c(tail(x, -1), NA),
                         yend = c(tail(y, -1), NA),
                         keep = c(group[-1] == head(group, -1), FALSE))
    munched <- munched[munched$keep, ]

    my_path <- switch(type,
      point = grid::grob(
      x0 = unit(munched$x, "npc"), x1 = unit(munched$xend, "npc"),
      y0 = unit(munched$y, "npc"), y1 = unit(munched$yend, "npc"),
      mult = (munched$size * .pt + munched$stroke * .stroke / 2) * munched$gap,
      name = "trail",
      gp = grid::gpar(
        col = alpha(munched$colour, munched$alpha),
        fill = alpha(munched$colour, munched$alpha),
        lwd = munched$linesize * .pt,
        lty = munched$linetype,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
      ),
      vp = NULL,
      cl = "trail"
    ),
    text = grid::grob(
      x0 = unit(munched$x, "npc"), x1 = unit(munched$xend, "npc"),
      y0 = unit(munched$y, "npc"), y1 = unit(munched$yend, "npc"),
      mult = munched$size * munched$gap * .pt,
      name = "trail",
      gp = grid::gpar(
        col = alpha(munched$colour, munched$alpha),
        fill = alpha(munched$colour, munched$alpha),
        lwd = munched$linesize * .pt,
        lty = munched$linetype,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
      ),
      vp = NULL,
      cl = "trail"
    )
    )
}
    ggplot2:::ggname(
      "geom_trail",
      grid::grobTree(my_path, my_points)
    )
  },
  non_missing_aes = c("size", "colour")
)

#' grid draw method geom trail
#' @description underlying drawing method for paths in geom_trail
#' @rdname makeContext.trail
#' @author Teun van den Brand
#' @import grid
#' @importFrom utils head
#' @param x grob object passed to method
#' @export

makeContext.trail <- function(x) {
  # Convert npcs to absolute units
  x0 <- grid::convertX(x$x0, "mm", TRUE)
  y0 <- grid::convertY(x$y0, "mm", TRUE)
  x1 <- grid::convertX(x$x1, "mm", TRUE)
  y1 <- grid::convertY(x$y1, "mm", TRUE)

  # Do trigonometry stuff
  dx <- x1 - x0
  dy <- y1 - y0
  hyp <- sqrt(dx ^ 2 + dy ^ 2)
  nudge_y <- (dy / hyp) * x$mult
  nudge_x <- (dx / hyp) * x$mult

  # Calculate new positions
  x0 <- x0 + nudge_x
  x1 <- x1 - nudge_x
  y0 <- y0 + nudge_y
  y1 <- y1 - nudge_y

  # Filter overshoot
  keep <- (sign(dx) == sign(x1 - x0)) & (sign(dy) == sign(y1 - y0))
  x$gp[] <- lapply(x$gp, function(x) {
    if (length(x) == 1L) return(x) else x[keep]
  })

  # Supply new xy coordinates
  x$x0 <- unit(x0[keep], "mm")
  x$x1 <- unit(x1[keep], "mm")
  x$y0 <- unit(y0[keep], "mm")
  x$y1 <- unit(y1[keep], "mm")

  # Set to segments class
  x$mult <- NULL
  x$id <- NULL
  class(x)[1] <- "segments"
  x
}

# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}

