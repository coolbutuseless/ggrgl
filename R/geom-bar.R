
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_bar} into the third dimension.
#'
#' @section Z Offset Geom:
#' The \code{z} suffix indicates that this geom produces planar graphical elements,
#' parallel to the ground i.e. the drawn element is the same as that for
#' \code{ggplot2} except it has a vertical offset (constant across each graphical element).
#'
#' @section Geom Supports Extrusion:
#' This geometry supports extrusion.  Set \code{extrude = TRUE} and adjust
#' aesthetics:
#' \describe{
#' \item{\code{extrude_face_fill}}{The colour of the extruded faces. Default: 'grey20'}
#' \item{\code{extrude_face_alpha}}{Alpha for the extruded faces. Default: 1}
#' \item{\code{extrude_edge_colour}}{Colour of the edges of the extrusion. Default: NA (invisible)}
#' \item{\code{extrude_edge_alpha}}{Alpha for the extruded edges. Default: 1}
#' \item{\code{extrude_edge_size}}{Width of the line to draw the extruded edge. Default: 1}
#' \item{\code{extrude_z}}{The lower limit of the extrudsion. Default: 0.05}
#' }
#'
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,width,na.rm,orientation,show.legend,inherit.aes
#'        See documentation for \code{ggplot2::geom_bar}.
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_bar_z <- function(mapping = NULL, data = NULL,
                       stat = "count", position = "stack",
                       ...,
                       width       = NULL,
                       na.rm       = FALSE,
                       orientation = NA,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       extrude     = FALSE,
                       material    = list(),
                       keep2d      = FALSE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomBarZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width       = width,
      na.rm       = na.rm,
      orientation = orientation,
      extrude     = extrude,
      material    = material,
      keep2d      = keep2d,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomBarZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#'
#' @include geom-rect.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomBarZ <- ggproto(
  "GeomBarZ",
  GeomRectZ,
  required_aes = c("x", "y", "z"),

  # These aes columns are created by setup_data(). They need to be listed here so
  # that GeomRect$handle_na() properly removes any bars that fall outside the defined
  # limits, not just those for which x and y are outside the limits
  non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    data <- transform(data,
                      ymin = pmin(y, 0), ymax = pmax(y, 0),
                      xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
    flip_data(data, params$flipped_aes)
  },

  draw_panel = function(self, data, panel_params, coord, width = NULL, flipped_aes = FALSE,
                        extrude = FALSE, material = list(), keep2d = FALSE) {

    # Hack to ensure that width is detected as a parameter
    ggproto_parent(GeomRectZ, self)$draw_panel(data, panel_params, coord,
                                                extrude = extrude,
                                                material = material,
                                                keep2d = keep2d)
  }
)






