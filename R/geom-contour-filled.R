

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom_contour_z
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_contour_filled_z <- function(
  mapping     = NULL,
  data        = NULL,
  stat        = "contour_filled_z",
  position    = "identity",
  ...,
  bins        = NULL,
  binwidth    = NULL,
  breaks      = NULL,
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
    geom        = GeomContourFilledZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins      = bins,
      binwidth  = binwidth,
      breaks    = breaks,
      na.rm     = na.rm,
      extrude   = extrude,
      material  = material,
      keep2d    = keep2d,
      ...
    )
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-polygon.R
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomContourFilledZ <- ggproto(
  "GeomContourFilledZ",
  GeomPolygonZ
)














