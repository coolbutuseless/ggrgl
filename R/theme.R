
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' A theme based upon \code{theme_bw()} which sets some good defaults for \code{ggrgl} plots
#'
#' @param base_size,base_family,base_line_size,base_rect_size See docs for
#'        \code{ggplot2::theme_bw()}
#'
#' @importFrom ggplot2 theme_bw theme
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
theme_ggrgl <- function(base_size = 11, base_family = '', base_line_size = base_size/22,
                        base_rect_size = base_size/22) {
  ggplot2::theme_bw(
    base_size      = base_size,
    base_family    = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) +
    ggplot2::theme(rect = element_blank())
    # legend.background = ggplot2::element_blank(),
    # legend.key        = ggplot2::element_blank(),
    # plot.background   = ggplot2::element_blank(),
    # panel.background  = ggplot2::element_blank(),
}
