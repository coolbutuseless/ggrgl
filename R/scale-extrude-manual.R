


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create your own discrete scale
#'
#' These functions allow you to specify your own set of mappings from levels
#' in the data to aesthetic values.
#'
#' @param ... arguments passed on to \code{discrete_scale}. See \code{ggplot2::scale_fill_manual}
#'        for more details.
#' @param values,aesthetics,breaks  See \code{ggplot2::scale_fill_manual}
#'        documentation for more details.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_manual <- function (..., values, aesthetics = "extrude_face_fill", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_face_fill_manual
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_alpha_manual <- function (..., values, aesthetics = "extrude_face_alpha", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_face_fill_manual
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_edge_colour_manual <- function (..., values, aesthetics = "extrude_edge_colour", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_face_fill_manual
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_edge_alpha_manual <- function (..., values, aesthetics = "extrude_edge_alpha", breaks = waiver()) {
  manual_scale(aesthetics, values, breaks, ...)
}
