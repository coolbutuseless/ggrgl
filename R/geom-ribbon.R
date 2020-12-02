
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_ribbon} and \code{geom_area} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family zoffset geoms
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,na.rm,orientation,show.legend,inherit.aes,outline.type
#'        See documentation for \code{ggplot2::geom_ribbon}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_ribbon_z <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm        = FALSE,
                          orientation  = NA,
                          show.legend  = NA,
                          inherit.aes  = TRUE,
                          outline.type = "both",
                          extrude      = FALSE,
                          material     = list(),
                          keep2d       = FALSE) {

  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomRibbonZ,
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
#' GeomRibbonZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomRibbonZ <- ggproto(
  "GeomRibbonZ",
  Geom,
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour   = NA,
      fill     = "grey20",
      size     = 0.5,
      linetype = 1,
      alpha    = NA
    )
  ),

  required_aes = c("x|y", "ymin|xmin", "ymax|xmax", "z"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)

    if (is.null(data$ymin) && is.null(data$ymax)) {
      abort(glue("Either ", flipped_names(params$flipped_aes)$ymin, " or ",
                 flipped_names(params$flipped_aes)$ymax, " must be given as an aesthetic."))
    }
    data <- data[order(data$PANEL, data$group, data$x), , drop = FALSE]
    data$y <- data$ymin %||% data$ymax
    flip_data(data, params$flipped_aes)
  },

  draw_key = draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE,
                        flipped_aes = FALSE, outline.type = "both",
                        extrude = FALSE, material = list(), keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomRibbon$draw_group(data, panel_params, coord, na.rm = na.rm,
                                      flipped_aes = flipped_aes, outline.type = outline.type)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }


    data <- flip_data(data, flipped_aes)
    if (na.rm) data <- data[stats::complete.cases(data[c("x", "ymin", "ymax")]), ]
    data <- data[order(data$group), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "size", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      abort("Aesthetics can not vary with a ribbon")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("x", "ymin", "ymax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    data <- unclass(data) #for faster indexing
    positions <- new_data_frame(list(
      x = c(data$x, rev(data$x)),
      y = c(data$ymax, rev(data$ymin)),
      z = c(data$z   , rev(data$z)),
      id = c(ids, rev(ids))
    ))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Probably slower than ggplot2 original method, but I want to keep all
    # variables
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    positions <- as.data.frame(lapply(data, function(x) c(x, rev(x))))
    positions$y  <- c(data$ymax, rev(data$ymin))
    positions$id <- c(ids, rev(ids))


    positions <- flip_data(positions, flipped_aes)

    munched <- coord_munch(coord, positions, panel_params)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    is_full_outline <- identical(outline.type, "full")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Upper polygons
    # For each polygon, decompose into set of triangles
    # Avoid using 'rgl::polygon3d' as it is very slow in comparison to
    # decido::earcut()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polys <- split(munched, munched$id)
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
        color = aes$fill,
        alpha = ifelse(is.na(polys$alpha), 1, polys$alpha)
      ),
      defaults = material
    )

    upper_polygons_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Upper polylines
    # For each original polygon, draw the closed polyline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polys <- split(munched, munched$id)
    polys <- Filter(function(x) nrow(x) > 0, polys)
    rgl_call_list <- lapply(polys, function(poly) {
      if (is.na(poly$colour[1]) || poly$colour[1] == 'NA') {
        NULL
      } else {
        cryogenic::capture_call(
          lines3d(
            x     = c(poly$x, poly$x[1]),
            y     = c(poly$y, poly$y[1]),
            z     = c(poly$z, poly$z[1]),
            color = poly$colour,
            alpha = ifelse(is.na(poly$alpha), 1, poly$alpha)
          ),
          defaults = material
        )
      }
    })

    rgl_call_list <- Filter(Negate(is.null), rgl_call_list)
    upper_polylines_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call_list)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create extrusion
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    extrusion <- list(
      faces_grob = ggplot2::zeroGrob(),
      edges_grob = ggplot2::zeroGrob()
    )
    if (isTRUE(extrude)) {
      extrusion <- create_extrusion(munched, closed = TRUE)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (is_full_outline) {
      return(ggname("geom_ribbon", g_poly))
    }

    munched_lines <- munched
    # increment the IDs of the lower line
    munched_lines$id <- switch(outline.type,
                               both = munched_lines$id + rep(c(0, max(ids, na.rm = TRUE)), each = length(ids)),
                               upper = munched_lines$id + rep(c(0, NA), each = length(ids)),
                               lower = munched_lines$id + rep(c(NA, 0), each = length(ids)),
                               abort(glue("invalid outline.type: {outline.type}"))
    )

    ggname("geom_ribbon", grobTree(
      grob2d,
      extrusion$faces_grob,
      extrusion$edges_grob,
      upper_polygons_grob,
      upper_polylines_grob)
    )
  }

)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom_ribbon_z
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_area_z <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "stack", na.rm = FALSE, orientation = NA,
                        show.legend = NA, inherit.aes = TRUE, ...,
                        outline.type = "upper",
                        extrude      = FALSE,
                        material     = list(),
                        keep2d       = FALSE) {
  outline.type <- match.arg(outline.type, c("both", "upper", "lower", "full"))

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomAreaZ,
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
#' GeomRibbonZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomAreaZ <- ggproto(
  "GeomAreaZ",
  GeomRibbonZ,

  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour   = NA,
      fill     = "grey20",
      size     = 0.5,
      linetype = 1,
      alpha    = NA
    )
  ),

  required_aes = c("x", "y", "z"),

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- transform(data[order(data$PANEL, data$group, data$x), ], ymin = 0, ymax = y)
    flip_data(data, params$flipped_aes)
  }
)


