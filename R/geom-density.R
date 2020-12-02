

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_density} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes,orientation,outline.type
#'        See documentation for \code{ggplot2::geom_density}
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_density_z <- function(mapping = NULL, data = NULL,
                           stat = "density", position = "identity",
                           ...,
                           na.rm        = FALSE,
                           orientation  = NA,
                           show.legend  = NA,
                           inherit.aes  = TRUE,
                           outline.type = "upper",
                           extrude      = FALSE,
                           material     = list(),
                           keep2d       = FALSE) {

  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomDensityZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm        = na.rm,
      orientation  = orientation,
      outline.type = outline.type,
      extrude      = extrude,
      material     = material,
      keep2d       = keep2d,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomDensitZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @include geom-ribbon.R
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomDensityZ <- ggproto(
  "GeomDensityZ",
  GeomAreaZ,
  default_aes = augment_aes(
    extrusion_aesthetics,
    defaults(
      aes(fill = NA, weight = 1, colour = "black", alpha = NA),
      GeomAreaZ$default_aes
    )
  )
)


