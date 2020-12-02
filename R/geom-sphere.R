

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A fully 3-dimensional analogue of \code{geom_point}
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes see
#'        documentation for \code{ggplot2::geom_point()}
#'
#' @inheritSection geom_path_3d Full 3d Positioning
#' @family 3d geoms
#'
#' @import rlang
#' @import glue
#' @importFrom stats setNames
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_sphere_3d <- function(mapping     = NULL,
                      data        = NULL,
                      stat        = "identity",
                      position    = "identity",
                      ...,
                      na.rm       = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      material    = list()) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomSphere3d,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm    = na.rm,
      material = material,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomSphere3d
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomSphere3d <- ggplot2::ggproto(
  "GeomSphere3d",
  ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour = "black",
      size   = 1.5,
      fill   = NA,
      alpha  = 1,
      stroke = 0.5
    )
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        extrude = FALSE, material = list(), npolys = 10, keep2d = FALSE) {

    coords <- coord$transform(data, panel_params)


    rgl_call <- cryogenic::capture_call(
      spheres3d(
        x      = coords$x,
        y      = coords$y,
        z      = coords$z,
        color  = coords$colour,
        radius = coords$size,
        alpha  = ifelse(is.na(coords$alpha), 1, coords$alpha)
      ),
      defaults = material
    )

    spheres_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)

    ggname(
      "geom_sphere_3d",
      grid::grobTree(
        spheres_grob
      )
    )
  },

  draw_key = draw_key_point
)






