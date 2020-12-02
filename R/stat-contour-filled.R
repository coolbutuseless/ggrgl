

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname geom_contour_z
#' @param geom geom for stat
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stat_contour_filled_z <- function(
  mapping     = NULL,
  data        = NULL,
  geom        = "contour_filled_z",
  position    = "identity",
  ...,
  bins        = NULL,
  binwidth    = NULL,
  breaks      = NULL,
  na.rm       = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  extrude     = FALSE,
  material    = list(),
  keep2d      = FALSE) {
  layer(
    data        = data,
    mapping     = mapping,
    stat        = StatContourFilledZ,
    geom        = geom,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins     = bins,
      binwidth = binwidth,
      breaks   = breaks,
      na.rm    = na.rm,
      extrude   = extrude,
      material  = material,
      keep2d    = keep2d,
      ...
    )
  )
}





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StatContourFilledZ <- ggproto(
  "StatContourFilledZ",
  Stat,

  required_aes = c("x", "y", "z"),
  default_aes = augment_aes(
    extrusion_aesthetics,
    aes(
      order = after_stat(level),
      fill  = after_stat(level)
    )
  ),

  setup_params = function(data, params) {
    params$z.range <- range(data$z, na.rm = TRUE, finite = TRUE)
    params
  },

  compute_group = function(data, scales, z.range, bins = NULL, binwidth = NULL, breaks = NULL, na.rm = FALSE) {
    breaks <- contour_breaks(z.range, bins, binwidth, breaks)

    isobands <- xyz_to_isobands(data, breaks)
    names(isobands) <- pretty_isoband_levels(names(isobands))
    path_df <- iso_to_polygon(isobands, data$group[1])

    path_df$level      <- ordered(path_df$level, levels = names(isobands))
    path_df$level_low  <- breaks[as.numeric(path_df$level)]
    path_df$level_high <- breaks[as.numeric(path_df$level) + 1]
    path_df$level_mid  <- 0.5*(path_df$level_low + path_df$level_high)
    path_df$nlevel     <- rescale_max(path_df$level_high)
    path_df$z          <- path_df$level_low

    path_df
  }
)
