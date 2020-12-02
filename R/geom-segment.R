
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_segment} into the third dimension.
#'
#' @inheritSection geom_path_3d Full 3d Positioning
#' @family 3d geoms
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,arrow,arrow.fill,lineend,linejoin,na.rm,show.legend,inherit.aes
#'        See documentation for \code{ggplot2::geom_segment}
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_segment_3d <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           arrow       = NULL,
                           arrow.fill  = NULL,
                           lineend     = "butt",
                           linejoin    = "round",
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
    geom        = GeomSegment3d,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow      = arrow,
      arrow.fill = arrow.fill,
      lineend    = lineend,
      linejoin   = linejoin,
      na.rm      = na.rm,
      extrude    = extrude,
      material   = material,
      keep2d     = keep2d,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomSegment3d
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomSegment3d <- ggplot2::ggproto(
  "GeomSegment3d",
  ggplot2::Geom,
  required_aes = c("x", "y", "z", "xend", "yend", "zend"),
  non_missing_aes = c("linetype", "size", "shape"),

  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour   = "black",
      size     = 0.5,
      linetype = 1,
      alpha    = NA
    )
  ),

  draw_panel = function(data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE,
                        extrude = FALSE, material = list(), keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomSegment$draw_panel(data, panel_params, coord, arrow = arrow,
                                       arrow.fill = arrow.fill, lineend = lineend,
                                       linejoin = linejoin, na.rm = na.rm)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }


    data <- remove_missing(
      data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "shape"),
      name = "geom_segment"
    )

    if (empty(data)) return(ggplot2::zeroGrob())

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Linear coord only for now
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!coord$is_linear()) {
      abort("GeomSegment3d: Non-linear coord not handled. (yet!)")
    }

    coord      <- coord$transform(data, panel_params)
    arrow.fill <- arrow.fill %||% coord$colour

    x     <- with(coord, interleave(x, xend))
    y     <- with(coord, interleave(y, yend))
    z     <- with(coord, interleave(z, zend))
    color <- with(coord, rep(colour, each = 2))
    size  <- with(coord, rep(size  , each = 2))
    alpha <- with(coord, rep(alpha , each = 2))
    alpha <- ifelse(is.na(alpha), 1, alpha)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # TODO: do I need a separate segments3d call for each different size of lwd?
    # because I can only specify 1 lwd value, not a vector.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rgl_call <- cryogenic::capture_call(
      segments3d(
        x     = x,
        y     = y,
        z     = z,
        color = color,
        alpha = alpha,
        lwd   = coord$size[1] * ggplot2::.pt
      ),
      defaults = material
    )

    segments_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create extrusion
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
    if (isTRUE(extrude)) {
      ex_data      <- coord
      ex_data$id   <- seq_len(nrow(ex_data))

      ex_data2   <- ex_data
      ex_data2$x <- ex_data2$xend
      ex_data2$y <- ex_data2$yend
      ex_data2$z <- ex_data2$zend

      ex_data <- rbind(ex_data, ex_data2)
      ex_data <- ex_data[order(ex_data$id),]

      ex_data$id   <- ceiling(seq_len(nrow(ex_data))/2)

      extrusion    <- create_extrusion(ex_data, closed = FALSE)
    }


    grid::grobTree(
      grob2d,
      segments_grob,
      extrusion$faces_grob,
      extrusion$edges_grob
    )
  },

  draw_key = ggplot2::draw_key_path
)



