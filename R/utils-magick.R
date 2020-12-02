
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' abind clone for adding a matrix to an array
#'
#' A very cut-down version of \code{abind::abind()} that will only stack a
#' matrix onto an array.
#'
#' @param arr,mat array and matrix to be stacked
#'
#' @return new array with matrix added as a new plane at the end of the array
#'
#' @importFrom utils head
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
my_abind <- function(arr, mat) {

  stopifnot(is.array(arr))
  stopifnot(is.matrix(mat))
  if (!identical(head(dim(arr), -1), dim(mat))) {
    stop("Dimension missmatch. Array: ", deparse(dim(arr)), "  Matrix: ", deparse(dim(mat)))
  }

  new_dim    <- dim(arr)
  new_dim[3] <- new_dim[3] + 1

  array(c(arr, mat), dim = new_dim)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a magick image to an RGBA array.
#'
#' This will promote gray or RGB images to RGBA arrays.
#'
#' @param img magick image
#'
#' @return RGBA array with all values in range [0, 1]
#'
#' @import magick
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_img_to_array <- function(img) {

  stopifnot(inherits(img, 'magick-image'))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # extract the RGB array from that image
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  arr <- as.numeric(magick::image_data(img))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this is a grey image (i.e. a 2d matrix), then promote it
  # to a 3d array by copying the grey into R,G and B planes
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(dim(arr)) == 2) {
    arr <- array(c(arr, arr, arr), dim = c(dim(arr), 3))
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add an alpha channel if there isn't one already
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (dim(arr)[3] == 3) {
    alpha_matrix <- matrix(1, nrow=dim(arr)[1], ncol = dim(arr)[2])
    arr          <- my_abind(arr, alpha_matrix)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check: Assert everything image is RGBA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(dim(arr)[3] == 4)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transpose the image if requested.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if (transpose) {
  #   arr <- aperm(arr, c(2, 1, 3))
  # }

  arr
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an image to a raster object
#'
#' @param img image
#'
#' @importFrom grDevices as.raster
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
convert_img_to_raster <- function(img) {
  as.raster(convert_img_to_array(img))
}



if (FALSE) {
  library(magick)
  image_file <- system.file("img", "Rlogo.jpg", package="jpeg")

  im <- image_read(image_file)

  arr <- convert_img_to_array(im)

}

