
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_contour} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,bins,binwidth,breaks,lineend,linejoin,linemitre,na.rm,show.legend,inherit.aes
#'        See documentation for \code{ggplot2::geom_bar}.
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_contour_z <- function(mapping     = NULL,
                         data        = NULL,
                         stat        = "contour_z",
                         position    = "identity",
                         ...,
                         bins        = NULL,
                         binwidth    = NULL,
                         breaks      = NULL,
                         lineend     = "butt",
                         linejoin    = "round",
                         linemitre   = 10,
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
    geom        = GeomContourZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins      = bins,
      binwidth  = binwidth,
      breaks    = breaks,
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      na.rm     = na.rm,
      extrude   = extrude,
      material  = material,
      keep2d    = keep2d,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomContourZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @import grid
#' @export
#'
#' @include geom-path.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomContourZ <- ggproto(
  "GeomContourZ",
  GeomPath3d,

  required_aes = c("x", "y", "z"),

  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      weight   = 1,
      colour   = "#3366FF",
      size     = 0.5,
      linetype = 1,
      alpha    = NA,
      z        = 10
    )
  )
)











