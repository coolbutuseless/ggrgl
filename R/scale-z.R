
#-----------------------------------------------------------------------------
#' Scales for z
#'
#' @param name,breaks,labels,limits,trans,guide See \code{ggplot2::scale_size} for more information
#' @param range a numeric vector of length 2 that specifies the minimum and
#'   maximum size of the plotting symbol after transformation.
#'
#' @import scales
#' @export
#-----------------------------------------------------------------------------
scale_z_continuous <- function(name = waiver(), breaks = waiver(), labels = waiver(),
                                          limits = NULL, range = c(1, 100),
                                          trans = "identity", guide = FALSE) {

  continuous_scale("z", "z", rescale_pal(range), name = name,
                   breaks = breaks, labels = labels, limits = limits, trans = trans,
                   guide = guide)
}


# #-----------------------------------------------------------------------------
# #' @rdname scale_z_continuous
# #' @export
# #-----------------------------------------------------------------------------
# scale_size_binned <- function(name = waiver(), breaks = waiver(), labels = waiver(),
#                               limits = NULL, range = c(1, 6), n.breaks = NULL,
#                               nice.breaks = TRUE, trans = "identity", guide = "bins") {
#   binned_scale("size", "area_b", area_pal(range), name = name,
#                breaks = breaks, labels = labels, limits = limits, trans = trans,
#                n.breaks = n.breaks, nice.breaks = nice.breaks, guide = guide)
# }


#-----------------------------------------------------------------------------
#' @rdname scale_z_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_z_discrete <- function(..., guide='none') {
  # warn("Using 'z' for a discrete variable is not advised.")
  scale_z_ordinal(..., guide=guide)
}


#-----------------------------------------------------------------------------
#' @rdname scale_z_continuous
#' @export
#' @usage NULL
#-----------------------------------------------------------------------------
scale_z_ordinal <- function(..., range = c(1, 100)) {
  force(range)

  discrete_scale(
    "z",
    "z_d",
    function(n) {
      seq(range[1], range[2], length.out = n)
    },
    ...
  )
}
