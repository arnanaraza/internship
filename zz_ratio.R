intr.files <- list.files(paste0(mydir, '/temp/allmask'),pattern='.tif')
pacman::p_load(raster, rgdal, parallel)

intr.files
setwd(paste0(mydir, '/temp/allmask'))
palst <- stack(intr.files[[4]])
            
pCalc <- function(x,y) {
  ratio <- (x / y)
  return(ratio)
}

palst$hh.hv1 <- overlay(x=palst[[1]],y=palst[[3]], fun=pCalc)
plot(palst$hh.hv1)
axis(1, at = seq(-1, 1, by = 0.1), las=2)
