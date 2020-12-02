

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_polygon} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @family zoffset geoms
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family geoms supporting extrusion
#'
#' @param extrude whether or not to extrude the polygon
#' @param keep2d keep the original 2d representation? default: FALSE
#' @param material Arguments passed to \code{rgl::material3d()} to specify the
#'        material properties for this geom.   Any
#'        parameters specified in this argument override the defaults.  See \code{rgl}
#'        documentation for more info. Also see \code{ggrgl::standard_material} for
#'        the standard default material parameters used for all objects.
#' @param mapping,data,stat,position,rule,...,na.rm,show.legend,inherit.aes See
#'        documentation for \code{ggplot2::geom_polygon}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_polygon_z <- function(mapping = NULL, data = NULL,
                           stat        = "identity",
                           position    = "identity",
                           rule        = "evenodd",
                           ...,
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
    geom        = GeomPolygonZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params      = list(
      na.rm      = na.rm,
      rule       = rule,
      extrude    = extrude,
      material   = material,
      keep2d     = keep2d,
      ...
    )
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomPolygonZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @import grid
#' @import snowcrash
#' @import triangular
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomPolygonZ <- ggplot2::ggproto(
  "GeomPolygonZ",
  ggplot2::Geom,
  draw_panel = function(data, panel_params, coord, rule = "evenodd",
                        extrude = FALSE, material = list(), keep2d = FALSE,
                        use_triangular = TRUE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomPolygon$draw_panel(data, panel_params, coord)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }


    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)
    # munched <- munched[munched$group %in% unique(munched$group)[1:2], , drop = FALSE]

    if (is.null(munched$subgroup)) {
      # Assign all subgroup=1, so that processing with/without holes has
      # the same code
      munched$subgroup <- 1L
    }

    munched$group2 <- as.character(interaction(munched$group, munched$subgroup))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Sort by group to make sure that colors, fill, etc. come in same order
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # munched <- munched[order(munched$group2), ]
    if (use_triangular) {
      polys <- triangular::decompose(munched)
    } else {
      # Grouping for alternate subgroups being polygons then holes
      munched$pair_idx <- as.integer((munched$subgroup + 1)/2)
      munched$group3 <- as.character(interaction(munched$group, munched$pair_idx))
      polys <- split(munched, munched$group3)
      polys <- Filter(function(x) nrow(x) > 0, polys)

      polys <- lapply(polys, function(poly) {
        holes <- tail(which(!duplicated(poly$subgroup)), -1)
        if (length(holes) == 0) holes <- 0
        # message(poly$group[[1]], "  holes:", holes)
        idx <- decido::earcut(poly[,c('x', 'y')], holes = holes)
        poly[idx,]
      })
      polys <- do.call(rbind, polys)
    }


    rgl_call <- cryogenic::capture_call(
      triangles3d(
        x     = polys$x,
        y     = polys$y,
        z     = polys$z,
        color = polys$fill,
        alpha = ifelse(is.na(polys$alpha), 1, polys$alpha)
      ),
      defaults = material
    )

    upper_polygons_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Upper polylines
    # For each original polygon, draw the closed polyline
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    polys <- split(munched, munched$group2)
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
    extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
    if (isTRUE(extrude)) {
      ex_data    <- munched
      ex_data$id <- ex_data$group2
      extrusion  <- create_extrusion(ex_data, closed = TRUE)
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The final geometry
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ggname(
      "geom_polygon_z",
      grid::grobTree(
        grob2d,
        upper_polygons_grob,
        upper_polylines_grob,
        extrusion$faces_grob,
        extrusion$edges_grob
      )
    )

  },

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # User *must* supply these aes()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  required_aes = c("x", "y", "z"),

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Default values for other aes()
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour          = "NA",
      fill            = "grey20",
      size            = 0.5,
      linetype        = 1,
      alpha           = NA,
      subgroup        = NULL
    )
  ),

  handle_na = function(data, params) {
    data
  },


  draw_key = ggplot2::draw_key_polygon
)



if (FALSE) {
  library(ggplot2)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # polygon
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  polygons_df <- data.frame(
    x        = c(4, 8, 8, 4,   6, 7, 7, 6,  4.5,   5, 5, 4.5),
    y        = c(4, 4, 8, 8,   6, 6, 7, 7,  4.5, 4.5, 5, 5),
    group    = c(1, 1, 1, 1,   1, 1, 1, 1,    1,   1, 1, 1),
    subgroup = c(1, 1, 1, 1,   2, 2, 2, 2,    3,   3, 3, 3)
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Native" ggplot2 rendering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- ggplot(polygons_df) +
    geom_polygon_z(aes(x, y, subgroup = subgroup), z = 20, colour = 'red', size = 5) +
    theme_bw() +
    coord_equal() +
    labs(title = "ggplot2 rendering of original polygon(s)")

  devoutrgl::rgldev(fov = 30, view_preset = 1)
  p
  dev.off()
}




if (FALSE) {
  set.seed(1)
  polygons_df <- data.frame(
    x        = runif(10),
    y        = runif(10),
    group    = 1,
    subgroup = 1
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Native" ggplot2 rendering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p <- ggplot(polygons_df) +
    geom_polygon_z(aes(x, y), z = 100, fill = 'lightblue') +
    geom_polygon(aes(x, y)) +
    # geom_path(aes(x, y, group = interaction(group, subgroup))) +
    theme_bw() +
    coord_equal() +
    labs(title = "Overlay 2d + z offset", subtitle = "{ggrgl} + {devoutrgl}")

  devoutrgl::rgldev(fov = 30, view_preset = 1)
  p
  dev.off()
}













