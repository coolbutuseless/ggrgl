
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Viridis colour scales from viridisLite
#'
#' The `viridis` scales provide colour maps that are perceptually uniform in both
#' colour and black-and-white. They are also designed to be perceived by viewers
#' with common forms of colour blindness. See also
#' <https://bids.github.io/colormap/>.
#'
#' @param begin,end,alpha,direction,option,values,space,na.value,guide See \code{ggplot2::scale_colour_viridis_d} for more information
#' @param ... Other arguments passed on to [discrete_scale()],
#' [continuous_scale()], or [binned_scale] to control name, limits, breaks,
#'   labels and so forth.
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_viridis_d <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", aesthetics = "extrude_face_fill") {
  discrete_scale(
    aesthetics,
    "viridis_d",
    viridis_pal(alpha, begin, end, direction, option),
    ...
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_extrude_face_fill_viridis_d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_viridis_c <- function(..., alpha = 1, begin = 0, end = 1,
                                         direction = 1, option = "D", values = NULL,
                                         space = "Lab", na.value = "grey50",
                                         guide = FALSE, aesthetics = "extrude_face_fill") {

  continuous_scale(
    aesthetics,
    "viridis_c",
    gradient_n_pal(
      viridis_pal(alpha, begin, end, direction, option)(6),
      values,
      space
    ),
    na.value = na.value,
    guide = guide,
    ...
  )
}

