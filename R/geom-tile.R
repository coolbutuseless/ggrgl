
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_tile} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,linejoin,na.rm,show.legend,inherit.aes
#'        See documentation for \code{ggplot2::geom_segment}
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_tile_z <- function(mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      ...,
                      linejoin    = "mitre",
                      na.rm       = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      extrude     = FALSE,
                      material    = list(),
                      keep2d      = FALSE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTileZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      linejoin = linejoin,
      na.rm    = na.rm,
      extrude  = extrude,
      material = material,
      keep2d   = keep2d,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomTileZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomTileZ <- ggplot2::ggproto(
  "GeomTileZ",
  GeomRectZ,
  extra_params = c("na.rm"),

  setup_data = function(data, params) {
    data$width  <- data$width  %||% params$width  %||% resolution(data$x, FALSE)
    data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

    transform(data,
              xmin = x - width  / 2,  xmax = x + width / 2,  width = NULL,
              ymin = y - height / 2, ymax = y + height / 2, height = NULL
    )
  },

  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      fill            = "grey20",
      colour          = NA,
      size            = 0.1,
      linetype        = 1,
      alpha           = NA,
      width           = NA,
      height          = NA
    )
  ),


  required_aes = c("x", "y", "z"),

  draw_key = ggplot2::draw_key_polygon
)

