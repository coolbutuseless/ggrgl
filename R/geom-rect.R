


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_rect} into the third dimension.
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
geom_rect_z <- function(mapping = NULL, data = NULL,
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
    data       = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomRectZ,
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
#' GeomRectZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomRectZ <- ggplot2::ggproto(
  "GeomRectZ",
  ggplot2::Geom,

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # include extrusion aesthetics as part of the default aes()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour          = NA,
      fill            = "grey35",
      size            = 0.5,
      linetype        = 1,
      alpha           = NA
    )
  ),

  required_aes = c("xmin", "xmax", "ymin", "ymax", 'z'),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  draw_panel = function(self, data, panel_params, coord, linejoin = "mitre",
                        extrude = FALSE, material = list(), keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomRect$draw_panel(data, panel_params, coord)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }

    aesthetics <- setdiff(
      names(data), c("x", "y", "xmin", "xmax", "ymin", "ymax", "z")
    )

    if (!coord$is_linear()) {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # 3d Polygons
      # in ggplot, rects are promoted to polygons and then drawn using
      # geom_polygon. However, polygon drawing in 3d can be slow (as we have
      # to use triangular, so let's do something different in ggrgl).
      # However, when the coord is not linear, then we need to do
      # the slow expensive way e.g. for pie charts
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      polys3d <- lapply(split(data, seq_len(nrow(data))), function(row) {
        poly <- rect_to_poly(row$xmin, row$xmax, row$ymin, row$ymax)
        aes <- new_data_frame(row[aesthetics])[rep(1,5), ]
        aes$z <- rep(row$z, each = 5)

        GeomPolygonZ$draw_panel(cbind(poly, aes), panel_params, coord,
                                 extrude = extrude, material = material, keep2d = keep2d)
      })


      res <- ggname("rect", do.call(grid::grobTree, polys3d))
      return(res)
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Draw a quad3d for each rect
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    munched <- coord_munch(coord, data, panel_params)
    alpha   <- rep(munched$alpha, each = 4)


    imunched <- data.frame(
      x = with(munched, interleave(xmin, xmax, xmax, xmin)),
      y = with(munched, interleave(ymin, ymin, ymax, ymax)),
      z = rep(munched$z, each = 4),
      id = rep(seq(nrow(munched)), each = 4),
      extrude_face_fill   = rep(munched$extrude_face_fill  , each = 4),
      extrude_face_alpha  = rep(munched$extrude_face_alpha , each = 4),
      extrude_edge_colour = rep(munched$extrude_edge_colour, each = 4),
      extrude_edge_alpha  = rep(munched$extrude_edge_alpha , each = 4),
      extrude_edge_size   = rep(munched$extrude_edge_size  , each = 4),
      extrude_z           = rep(munched$extrude_z          , each = 4)
    )

    rects_call <- cryogenic::capture_call(
      quads3d(
        x = imunched$x,
        y = imunched$y,
        z = imunched$z,
        color = rep(munched$fill, each = 4),
        alpha = ifelse(is.na(alpha), 1, alpha)
      ),
      defaults = material
    )

    alpha  <- ifelse(is.na(alpha), 1, alpha)
    colour <- rep(munched$colour, each = 4)
    alpha  <- ifelse(is.na(colour), 0, alpha)
    outlines_call <- cryogenic::capture_call(
      quads3d(
        x = imunched$x,
        y = imunched$y,
        z = imunched$z,
        color = colour,
        alpha = alpha,
        front = 'lines',
        back  = 'lines'
      ),
      defaults = material
    )


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create extrusion
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
    if (isTRUE(extrude)) {
      extrusion  <- create_extrusion(imunched, closed = TRUE)
    }


    rects_grob    <- snowcrash::encode_robj_to_rasterGrob(rects_call)
    outlines_grob <- snowcrash::encode_robj_to_rasterGrob(outlines_call)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Put them all together
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ggname("rect", grid::grobTree(
      grob2d,
      extrusion$faces_grob, extrusion$edges_grob,
      rects_grob, outlines_grob
    ))

  },

  draw_key = draw_key_polygon
)


# Convert rectangle to polygon
# Useful for non-Cartesian coordinate systems where it's easy to work purely in
# terms of locations, rather than locations and dimensions. Note that, though
# `polygonGrob()` expects an open form, closed form is needed for correct
# munching (c.f. https://github.com/tidyverse/ggplot2/issues/3037#issuecomment-458406857).
#
# @keyword internal
rect_to_poly <- function(xmin, xmax, ymin, ymax) {
  new_data_frame(list(
    y = c(ymax, ymax, ymin, ymin, ymax),
    x = c(xmin, xmax, xmax, xmin, xmin)
  ))
}


