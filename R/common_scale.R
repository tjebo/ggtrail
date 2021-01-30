#' Common scales for both x and y axis
#' @description Creates exactly the same scale for both x and y -
#'   just a massive convenience wrapper because I found myself
#'   calling the same parameters for both x and y way to often
#' @name scale_axis
#' @import ggplot2
#' @param ... passed to ggplot2::scale_x/y_discrete/continuous
#' @seealso [ggplot2::scale_x_continuous] and [ggplot2::scale_y_continuous]
#' @export
scale_axis_continuous <- function(...){
  list(ggplot2::scale_x_continuous(...),
       ggplot2::scale_y_continuous(...))
}

#' @rdname scale_axis
#' @export
scale_axis_discrete <- function(...){
  list(ggplot2::scale_x_discrete(...),
       ggplot2::scale_y_discrete(...))
}
