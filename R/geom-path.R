
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' An extension of \code{geom_path} and \code{geom_line} into the third dimension.
#'
#' @section Full 3d Positioning:
#' The \code{3d} suffix indicates that this geom allows for the full 3d
#' positioning of each node and/or edge which makes it up.  Thus the resulting
#' geometrical element rendered on the plot can appear in any orientiation, and
#' is not limited to planar representation.
#' @family 3d geoms
#' @inheritSection geom_bar_z Geom Supports Extrusion
#' @family geoms supporting extrusion
#'
#' @inheritParams geom_polygon_z
#' @param mapping,data,stat,position,...,lineend,linejoin,linemitre,arrow,na.rm,show.legend,inherit.aes,orientation
#'        See documentation for \code{ggplot2::geom_path}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_path_3d <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        lineend     = "butt",
                        linejoin    = "round",
                        linemitre   = 10,
                        arrow       = NULL,
                        na.rm       = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        extrude     = FALSE,
                        material    = list(),
                        keep2d      = FALSE) {
  layer(
    data = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomPath3d,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend   = lineend,
      linejoin  = linejoin,
      linemitre = linemitre,
      arrow     = arrow,
      na.rm     = na.rm,
      extrude   = extrude,
      material  = material,
      keep2d    = keep2d,
      ...
    )
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomPath3d
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomPath3d <- ggproto(
  "GeomPath3d",
  Geom,
  required_aes = c("x", "y", "z"),

  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      colour          = "black",
      size            = 0.5,
      linetype        = 1,
      alpha           = NA
    )
  ),

  handle_na = function(data, params) {
    # Drop missing values at the start or end of a line - can't drop in the
    # middle since you expect those to be shown by a break in the line
    complete <- stats::complete.cases(data[c("x", "y", "size", "colour", "linetype")])
    kept <- stats::ave(complete, data$group, FUN = keep_mid_true)
    data <- data[kept, ]

    if (!all(kept) && !params$na.rm) {
      warn(glue("Removed {sum(!kept)} row(s) containing missing values (geom_path)."))
    }

    data
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, extrude = FALSE, material = list(),
                        keep2d = FALSE) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Keep original 2d Polygons?
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (isTRUE(keep2d)) {
      grob2d <- GeomPath$draw_panel(data, panel_params, coord, arrow = arrow,
                                    lineend = lineend, linejoin = linejoin,
                                    linemitre = linemitre, na.rm = na.rm)
    } else {
      grob2d <- ggplot2::zeroGrob()
    }


    if (!anyDuplicated(data$group)) {
      message_wrap("geom_path3d: Each group consists of only one observation. ",
                   "Do you need to adjust the group aesthetic?")
    }

    # must be sorted on group
    data <- data[order(data$group), , drop = FALSE]
    munched <- coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(zeroGrob())

    # Work out whether we should use lines or segments
    attr <- dapply(munched, "group", function(df) {
      linetype <- unique(df$linetype)
      new_data_frame(list(
        solid = identical(linetype, 1) || identical(linetype, "solid"),
        constant = nrow(unique(df[, c("alpha", "colour","size", "linetype")])) == 1
      ), n = 1)
    })
    solid_lines <- all(attr$solid)
    constant <- all(attr$constant)
    if (!solid_lines && !constant) {
      abort("geom_path: If you are using dotted or dashed lines, colour, size and linetype must be constant over the line")
    }

    # Work out grouping variables for grobs
    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If the aesthetics aren't constant, then multiple segments must be
    # drawn - one segment for each set of aesthetics
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!constant) {

      x     <- interleave(munched$x[!end], munched$x[!start])
      y     <- interleave(munched$y[!end], munched$y[!start])
      z     <- interleave(munched$z[!end], munched$z[!start])
      color <- rep(munched$colour[!end], each = 2)
      size  <- rep(munched$size  [!end], each = 2) * ggplot2::.pt
      alpha <- rep(munched$alpha [!end], each = 2)
      alpha <- ifelse(is.na(alpha), 1, alpha)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # TODO: do I need a separate segments3d call for each different size of lwd?
      # because I can only specify 1 lwd value, not a vector, for each 'segments3d' call
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

      path_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)
      extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
    } else {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # All aesthetics are constant along the path, so we can draw a single
      # polyline.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      id <- match(munched$group, unique(munched$group))
      line_sets <- split(munched, munched$group)

      rgl_call_list <- lapply(line_sets, function(munched) {
        cryogenic::capture_call(
          lines3d(
            x     = munched$x,
            y     = munched$y,
            z     = munched$z,
            color = munched$colour,
            alpha = ifelse(is.na(munched$alpha), 1, munched$alpha)
          ),
          defaults = material
        )
      })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create extrusion
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      extrusion <- list(faces_grob = ggplot2::zeroGrob(), edges_grob = ggplot2::zeroGrob())
      if (isTRUE(extrude)) {
        ex_data    <- munched
        ex_data$id <- ex_data$group
        extrusion  <- create_extrusion(ex_data, closed = FALSE)
      }



      path_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call_list)
    }

    grid::grobTree(
      grob2d,
      path_grob,
      extrusion$faces_grob,
      extrusion$edges_grob
    )
  },

  draw_key = draw_key_path
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Trim false values from left and right: keep all values from
# first TRUE to last TRUE
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
keep_mid_true <- function(x) {
  first <- match(TRUE, x) - 1
  if (is.na(first)) {
    return(rep(FALSE, length(x)))
  }

  last <- length(x) - match(TRUE, rev(x)) + 1
  c(
    rep(FALSE, first),
    rep(TRUE, last - first),
    rep(FALSE, length(x) - last)
  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @export
#' @rdname geom_path_3d
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_line_3d <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", na.rm = FALSE, orientation = NA,
                        show.legend = NA, inherit.aes = TRUE, extrude = FALSE,
                        material = list(), keep2d = FALSE, ...) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomLine3d,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm       = na.rm,
      orientation = orientation,
      extrude     = extrude,
      material    = material,
      keep2d      = keep2d,
      ...
    )
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomLine3d
#'
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @import ggplot2
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomLine3d <- ggproto(
  "GeomLine3d",
  GeomPath3d,

  setup_params = function(data, params) {
    params$flipped_aes <- has_flipped_aes(data, params, ambiguous = TRUE)
    params
  },

  extra_params = c("na.rm", "orientation"),

  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    flip_data(data, params$flipped_aes)
  }
)



