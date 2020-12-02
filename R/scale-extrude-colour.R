

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Continuous and binned colour scales
#'
#' See \code{ggplot2::scale_colour_continuous()} for more information
#'
#' @param ... Additional parameters passed on to the scale type
#' @param type One of "gradient" (the default) or "viridis" indicating the
#'   colour scale to use
#' @export
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_continuous <- function(...,
                                          type = getOption("ggplot2.continuous.fill", default = "gradient")) {
  switch(
    type,
    gradient = scale_extrude_face_fill_gradient(...),
    viridis  = scale_extrude_face_fill_viridis_c(...),
    abort("Unknown scale type")
  )
}
