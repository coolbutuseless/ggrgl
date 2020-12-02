
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Gradient colour scales
#'
#' See \code{ggplot2::scale_colour_gradient()} for more information
#'
#' `scale_*_gradient` creates a two colour gradient (low-high),
#' `scale_*_gradient2` creates a diverging colour gradient (low-mid-high),
#' `scale_*_gradientn` creates a n-colour gradient.
#'
#' @param space,...,na.value,aesthetics See \code{scales::seq_gradient_pal}, \code{scale_colour_hue}, \code{ggplot2::continuous_scale}
#' @param low,mid,high,midpoint Colours for low and high ends of the gradient.
#' @param guide Type of legend. Use `"colourbar"` for continuous
#'   colour bar, or `"legend"` for discrete colour legend.
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_gradient <- function(..., low = "#132B43", high = "#56B1F7", space = "Lab",
                                        na.value = "grey50", guide = FALSE, aesthetics = "extrude_face_fill") {

  # ToDo: Why can't I have guide = 'colourbar'?
  continuous_scale(aesthetics, "gradient", seq_gradient_pal(low, high, space),
                   na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_face_fill_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_gradient2 <- function(..., low = muted("red"), mid = "white", high = muted("blue"),
                                         midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar",
                                         aesthetics = "extrude_face_fill") {
  continuous_scale(aesthetics, "gradient2",
                   div_gradient_pal(low, mid, high, space), na.value = na.value, guide = guide, ...,
                   rescaler = mid_rescaler(mid = midpoint))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# from ggplot2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mid_rescaler <- function(mid) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    rescale_mid(x, to, from, mid)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @inheritParams scales::gradient_n_pal
#' @param colours,colors Vector of colours to use for n-colour gradient.
#' @rdname scale_extrude_face_fill_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                           guide = "colourbar", aesthetics = "extrude_face_fill", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname scale_extrude_face_fill_gradient
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_extrude_face_fill_gradientn <- function(..., colours, values = NULL, space = "Lab", na.value = "grey50",
                                         guide = "colourbar", aesthetics = "extrude_face_fill", colors) {
  colours <- if (missing(colours)) colors else colours

  continuous_scale(aesthetics, "gradientn",
                   gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

