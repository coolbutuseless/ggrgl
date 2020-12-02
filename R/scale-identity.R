

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Use z values without scaling
#'
#' @param ...,guide See \code{ggplot2} for documentation on identity scales.
#'        e.g. \code{ggplot2::scale_alpha_identity()}
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
scale_z_identity <- function(..., guide = 'none') {
  continuous_scale(
    aesthetics = 'z',
    scale_name = 'identity',
    palette    = identity_pal(),
    ...,
    guide      = guide,
    super      = ScaleContinuousIdentity
  )
}
