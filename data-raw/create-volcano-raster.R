

library(viridis)

mat <- (volcano - min(volcano))/(max(volcano) - min(volcano))
ras <- viridis::viridis(256)[as.integer(mat*254) + 1]

dim(ras) <- rev(dim(mat))

cols <- t(col2rgb(ras)/255)

arr <- array(cols, dim = c(dim(mat), 3))
arr <- aperm(arr, c(2, 1, 3))

# plot(as.raster(arr))

volcano_arr <- arr

usethis::use_data(volcano_arr, internal = FALSE, overwrite = TRUE)
