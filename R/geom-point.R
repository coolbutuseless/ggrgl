

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_point} into the third dimension.
#'
#' Note that points in \code{ggplot2} are circles,  and this function still
#' plots circles, but with the option of raising or lowering the circle in the
#' \code{z} direction.
#'
#' For points with a fully 3-dimensional appearance see \code{geom_sphere_3d}.
#'
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param npolys Circles in ggrgl are approximated by polygons. \code{npolys}
#'        controls how many polygons are used to represent each cicle. Default: 10
#' @param mapping,data,stat,position,...,na.rm,show.legend,inherit.aes see
#'        documentation for \code{ggplot2::geom_point()}
#'
#' @import rlang
#' @import glue
#' @importFrom stats setNames
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_point_z <- function(mapping     = NULL,
                      data        = NULL,
                      stat        = "identity",
                      position    = "identity",
                      ...,
                      na.rm       = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      extrude     = FALSE,
                      material    = list(),
                      npolys      = 10,
                      keep2d      = FALSE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPointZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm    = na.rm,
      extrude  = extrude,
      material = material,
      npolys  = npolys,
      keep2d   = keep2d,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomPointZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomPointZ <- ggplot2::ggproto(
  "GeomPointZ",
  ggplot2::Geom,
  required_aes = c("x", "y", "z"),
  non_missing_aes = c("size", "shape", "colour"),
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      shape  = 19,
      colour = "black",
      size   = 1.5,
      fill   = NA,
      alpha  = 1,
      stroke = 0.5
    )
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE,
                        extrude = FALSE, material = list(), npolys = 10, keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomPoint$draw_panel(data, panel_params, coord, na.rm = na.rm)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }

    if (is.character(data$shape)) {
      data$shape <- translate_shape_string(data$shape)
    }

    coords <- coord$transform(data, panel_params)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Turn a single point (the centre) into coordinates around the circumference
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    npolys <- min(max(npolys, 2), 100)
    n_circle_segments <- npolys + 1
    theta <- seq(0, 2*pi, length.out = n_circle_segments)
    idx   <- rep(seq(nrow(coords)), each = n_circle_segments)

    coords_plus        <- coords
    coords_plus$pidx   <- seq_len(nrow(coords_plus))
    coords_plus        <- coords_plus[idx,]
    coords_plus$theta  <- theta
    coords_plus$radius <- coords_plus$size * 1/300
    coords_plus        <- transform(coords_plus,
                                    x = x + radius * cos(theta),
                                    y = y + radius * sin(theta))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Upper polygons
    # For each polygon, decompose into set of triangles
    # Avoid using 'rgl::polygon3d' as it is very slow in comparison to
    # decido::earcut()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polys <- split(coords_plus, coords_plus$pidx)
    polys <- lapply(polys, function(poly) {
      idx <- decido::earcut(poly[,c('x', 'y')])
      poly[idx,]
    })
    polys <- do.call(rbind, polys)

    rgl_call <- cryogenic::capture_call(
      triangles3d(
        x     = polys$x,
        y     = polys$y,
        z     = polys$z,
        color = polys$colour,
        alpha = ifelse(is.na(polys$alpha), 1, polys$alpha)
      ),
      defaults = material
    )

    upper_polygons_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create extrusion
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
    if (isTRUE(extrude)) {
      ex_data    <- coords_plus
      ex_data$id <- ex_data$pidx
      extrusion  <- create_extrusion(ex_data, closed = TRUE)
    }




    ggname(
      "geom_point_z",
      grid::grobTree(
        grob2d,
        upper_polygons_grob,
        extrusion$faces_grob,
        extrusion$edges_grob
      )
    )
  },

  draw_key = draw_key_point
)


