library(raster)

r <- raster("louisiana_SAR.tif")
plot(r)
rv <- values(r)
hist(rv)
