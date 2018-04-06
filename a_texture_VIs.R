#### Script to compute VIs, VV-VH ratio, and texture ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, glcm, rgdal)

# Function proper - argument should be filenames 'palsar, s1, and landsat'
RasProd <- function(satellite, sat.str) {
  subdir <- 'mid-results/with_derived_covs'
  dir.create(file.path(mydir, subdir))
  setwd(file.path(mydir, subdir))
  
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
    names(satellite) <- c("red" ,  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                          'rvi', "wdvi", 'svi', 'tex.red', 'tex.nir')
    return(satellite)
    writeRaster(satellite, 'all_optical.tif')
    setwd(mydir)}
    
  
  if (sat.str == 'palsar'){
    names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2')
    satellite$hv.hh1 <- satellite$hv1 / satellite$hh1 # ask what hv hh to use!!!!       
    satellite$hv.hh2 <- satellite$hv2 / satellite$hh2
    tex.hv1 <- glcm(raster(satellite, layer=2),window = c(3,3),
                    shift=list(c(1,1), c(-1,-1)))
    tex.hv2 <- glcm(raster(satellite, layer=4),window = c(3,3),
                   shift=list(c(1,1), c(-1,-1)))
    satellite <- addLayer(satellite, tex.hv1[[1]])
    satellite <- addLayer(satellite, tex.hv2[[1]])
    names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2', 'hv/hh1', 'hv/hh2', 'hv.tex1', 'hv.tex2')
    return (satellite)
    writeRaster(satellite, 'all_palsar.tif')
    setwd(mydir)}
  
  if (sat.str == 's1'){
    names(satellite) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                          'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')
    satellite$vh.vv <- satellite$mean.vh / satellite$mean.vv        
    tex.vh <- glcm(raster(satellite, layer=3),window = c(3,3),
                    shift=list(c(1,1), c(-1,-1)))
    satellite <- addLayer(satellite, tex.vh[[1]])
    names(satellite) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                          'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv','vh/vv', 'vh.tex')
    return (satellite)
    writeRaster(satellite, 'all_s1.tif')
    setwd(mydir)}
  
  if (sat.str == 'dem'){
    slope <- terrain(satellite, c("slope"), neighbors=4, unit='degrees')
    satellite <- addLayer(satellite, slope)
    names(satellite) <- c('dem', 'slope')
    return (satellite)
    writeRaster(satellite, 'dem_slope.tif')
    setwd(mydir)}
  
  if (sat.str == 'lcov'){
    satellite <- satellite[satellite > 19] = NA #removing  NA labels (using 0-19 classifications)
    writeRaster(satellite, 'lcov.tif')
    setwd(mydir)} 
}


