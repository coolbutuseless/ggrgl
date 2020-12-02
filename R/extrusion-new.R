

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an extrusion for an object
#'
#' @param data data.frame of polylines. one point per row. polylines
#'        grouped by the \code{id} value
#' @param closed whether or not the polyline should be closed. e.g. for
#'        extrusions of polygons and rects set \code{closed = TRUE} for
#'        line and segment objects set \code{closed = FALSE}.
#'
#' @importFrom rgl segments3d quads3d
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_extrusion <- function(data, closed) {

  expected_cols <- c('x', 'y', 'z', 'id', names(extrusion_aesthetics))

  if (any(!expected_cols %in% names(data))) {
    warning("create_extrusion: missing cols ", deparse(setdiff(expected_cols, names(data))))
    return(
      list(
        edges_grob = ggplot2::zeroGrob(),
        faces_grob = ggplot2::zeroGrob()
      )
    )
  }

  polylines <- split(data, data$id)

  lens <-rle(data$id)$lengths

  first_row_idx <- !duplicated(data$id)
  first_rows <- data[first_row_idx, , drop = FALSE]

  if (isTRUE(closed)) {
    res <- lapply(polylines, function(pl) {
      data.frame(
        x = interleave(pl$x, c(pl$x[-1], pl$x[1]), c(pl$x[-1], pl$x[1]), pl$x),
        y = interleave(pl$y, c(pl$y[-1], pl$y[1]), c(pl$y[-1], pl$y[1]), pl$y),
        z = interleave(pl$z, c(pl$z[-1], pl$z[1]), pl$extrude_z, pl$extrude_z)
      )
    })
  } else {
    res <- lapply(polylines, function(pl) {
      N <- nrow(pl)
      data.frame(
        x = interleave(pl$x[-N], pl$x[-1], pl$x[-1], pl$x[-N]),
        y = interleave(pl$y[-N], pl$y[-1], pl$y[-1], pl$y[-N]),
        z = interleave(pl$z[-N], pl$z[-1], pl$extrude_z[-1], pl$extrude_z[-N])
      )
    })
  }



  res <- do.call(rbind, res)

  face_colour  <- rep.int(first_rows$extrude_face_fill , lens * 4)
  face_alpha   <- rep.int(first_rows$extrude_face_alpha, lens * 4)

  face_alpha <- ifelse(is.na(face_alpha ), 1, face_alpha)
  face_alpha <- ifelse(is.na(face_colour), 0, face_alpha)

  # print(nrow(res))
  # print(length(face_colour))

  faces_call <- cryogenic::capture_call(
    quads3d(
      x = res$x,
      y = res$y,
      z = res$z,
      color = face_colour,
      alpha = face_alpha
    )
  )


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all rects for faces to a rastergrob
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  faces_grob <- snowcrash::encode_robj_to_rasterGrob(faces_call)



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prep for extruded edges
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data$extrude_edge_alpha <- ifelse(is.na(data$extrude_edge_alpha) , 1, data$extrude_edge_alpha)
  data$extrude_edge_alpha <- ifelse(is.na(data$extrude_edge_colour), 0, data$extrude_edge_alpha)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Edges of extruded faces
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  rgl_call <- cryogenic::capture_call(
    segments3d(
      x     = rep(data$x, each = 2),
      y     = rep(data$y, each = 2),
      z     = interleave(data$z, data$extrude_z),
      color = rep(data$extrude_edge_colour, each = 2),
      alpha = rep(data$extrude_edge_alpha , each = 2),
      lwd   = data$extrude_edge_size[1] %||% 1
    )
  )

  edges_grob <- snowcrash::encode_robj_to_rasterGrob(rgl_call)


  list(
    edges_grob = edges_grob,
    faces_grob = faces_grob
  )

}
