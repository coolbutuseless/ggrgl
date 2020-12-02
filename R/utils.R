#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Darken/lighten a hex colour by the given amount
#' Stolen from \url{https://benjaminlmoore.wordpress.com/2014/03/18/guardian-data-blog-uk-elections/}
#'
#' @param hex_colour strings e.g. "#345678"
#' @param amount fraction to darken by. default 0.15
#'
#' @return darkened hex colours
#'
#' @importFrom grDevices col2rgb rgb
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
darken_colour <- function(hex_colour, amount = 0.15) {
  rgb(t(col2rgb(hex_colour) * (1 - amount)), maxColorValue = 255)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname darken_colour
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lighten_colour <- function(hex_colour, amount = 0.15) {
  lighter <- t(col2rgb(hex_colour) * (1 + amount))
  lighter[lighter > 255] <- 255 # clamp
  rgb(lighter, maxColorValue = 255)
}
