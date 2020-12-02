
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Evenly spaced colours for discrete data
#'
#' This is the default colour scale for categorical variables. It maps each
#' level to an evenly spaced hue on the colour wheel. It does not generate
#' colour-blind safe palettes.
#'
#' @param na.value Colour to use for missing values
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with. This can be useful, for
#'   example, to apply colour settings to the `colour` and `fill` aesthetics at the
#'   same time, via `aesthetics = c("colour", "fill")`.
#' @param h,c,l,h.start,direction,... See \code{ggplot2::scale_colour_hue}
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_edge_colour_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                     direction = 1, na.value = "grey50", aesthetics = "extrude_edge_colour") {
  discrete_scale(aesthetics, "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_edge_colour_hue
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_hue <- function(..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
                                   direction = 1, na.value = "grey50", aesthetics = "extrude_face_fill") {
  discrete_scale(aesthetics, "hue", hue_pal(h, c, l, h.start, direction),
                 na.value = na.value, ...)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_extrude_edge_colour_hue
#' @usage NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_discrete <- scale_extrude_face_fill_hue


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname scale_extrude_edge_colour_hue
#' @usage NULL
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_edge_colour_discrete <- scale_extrude_edge_colour_hue
