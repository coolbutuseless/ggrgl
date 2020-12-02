

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_text} into the third dimension.
#'
#' @inheritSection geom_bar_z Z Offset Geom
#' @family zoffset geoms
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,parse,nudge_x,nudge_y,check_overlap,na.rm,show.legend,inherit.aes
#'        See documentation for \code{ggplot::geom_text}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_text_z <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        parse         = FALSE,
                        nudge_x       = 0,
                        nudge_y       = 0,
                        check_overlap = FALSE,
                        na.rm         = FALSE,
                        show.legend   = NA,
                        inherit.aes   = TRUE,
                        material      = list(),
                        keep2d        = FALSE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- position_nudge(nudge_x, nudge_y)
  }

  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextZ,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      parse         = parse,
      check_overlap = check_overlap,
      na.rm         = na.rm,
      material      = material,
      keep2d        = keep2d,
      ...
    )
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomTextZ
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomTextZ <- ggproto(
  "GeomTextZ",
  Geom,
  required_aes = c("x", "y", "z", "label"),

  default_aes = aes(
    colour = "black", size = 3.88, angle = 0, hjust = 0.5,
    vjust = 0.5, alpha = NA, family = "", fontface = 1, lineheight = 1.2
  ),

  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE, orientation = 'xy',
                        material = list(), keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomText$draw_panel(data, panel_params, coord, parse = parse,
                                    na.rm = na.rm, check_overlap = check_overlap)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }


    lab <- data$label
    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    data <- coord$transform(data, panel_params)
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Spheres grob
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    rgl_call <- cryogenic::capture_call(
      spheres3d(
        x     = data$x,
        y     = data$y,
        z     = 10,
        color = 'red',
        radius = 5,
        alpha  = 0.3
      ))

    spheres_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # text grob
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    text3d_rgl_calls <- lapply(seq(nrow(data)), function(i) {
      cryogenic::capture_call(
        rgl_text_helper(
          str         = lab[i],
          x           = data$x[i],
          y           = data$y[i],
          z           = data$z[i],
          rot         = 0,
          state       = NULL,
          manual      = TRUE,
          colour      = data$colour[i],
          orientation = orientation
        ),
        defaults = material
      )
    })

    text3d_grob <- snowcrash::encode_robj_to_rasterGrob(text3d_rgl_calls)

    grid::grobTree(
      grob2d,
      text3d_grob
    )
  },

  draw_key = draw_key_text
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# From ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
compute_just <- function(just, x) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(x[inward])]
  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(x[outward])]

  unname(c(left = 0, center = 0.5, right = 1,
           bottom = 0, middle = 0.5, top = 1)[just])
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# From ggplot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
just_dir <- function(x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}








