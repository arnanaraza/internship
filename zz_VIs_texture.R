#### Script to compute VIs, VV-VH ratio, and texture ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, glcm, rgdal)
mydir <- setwd('/home/sarvision/Desktop/BiomassPhilippines')

# Function proper - argument should be filenames 'palsar, s1, and landsat'
RasProd <- function(satellite, sat.str) {
  if (sat.str == 'landsat') {
    
    #VIs
    satellite$ndvi <- (satellite[[2]] - satellite[[1]]) / (satellite[[2]] + satellite[[1]])
    satellite$savi <- (1 + 0.5) * ((satellite[[2]] - satellite[[1]])) / (satellite[[2]] + (2.4* satellite[[1]]) + 1)
    satellite$evi <- 2.5* (satellite[[2]] - satellite[[1]]) /(satellite[[2]] + (2.4* satellite[[1]]) + 1)
    satellite$rvi <- satellite[[2]] / satellite[[1]]
    satellite$wdvi <- (satellite[[2]] - 2 * satellite[[1]] )/ 100
    satellite$svi <- satellite[[1]] / satellite[[2]]
    
    #texture
    tex.red <- glcm(raster(satellite, layer=1),window = c(3,3),
                             shift=list(c(1,1), c(-1,-1)))
    tex.nir <- glcm(raster(satellite, layer=2),window = c(3,3),
                    shift=list(c(1,1), c(-1,-1)))
    satellite <- addLayer(satellite, tex.red[[1]])
    satellite <- addLayer(satellite, tex.nir[[1]])
    names(satellite) <- c("red" ,  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",   'rvi', "wdvi", 'svi', 'tex.red', 'tex.nir')
    return(satellite)}
    
  
  if (sat.str == 'palsar'){
    names(satellite) <- names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2')
    satellite$hv.hh1 <- satellite$hv1 / satellite$hh1 # ask what hv hh to use!!!!       
    satellite$hv.hh2 <- satellite$hv2 / satellite$hh2
    tex.hv1 <- glcm(raster(satellite, layer=2),window = c(3,3),
                    shift=list(c(1,1), c(-1,-1)))
    tex.hv2 <- glcm(raster(satellite, layer=4),window = c(3,3),
                   shift=list(c(1,1), c(-1,-1)))
    satellite <- addLayer(satellite, tex.hv1[[1]])
    satellite <- addLayer(satellite, tex.hv2[[1]])
    names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2', 'hv/hh1', 'hv/hh2', 'hv.tex1', 'hv.tex2')
    return (satellite)}
}

Landsat <- RasProd(landsat,'landsat')
nem <- c("red" ,  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",   'rvi', "wdvi", 'svi')
nemski <- c(nem, nem1[11:26])
names(Landsat) <- nemski
setwd('G:/PhilippinesLandsat/')
writeRaster(Landsat, 'optical.tif')


tsek <- RasProd(landsat, 'landsat')
tsek1 <- RasProd(palsar, 'palsar')
