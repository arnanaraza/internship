#### Script to compute VIs, VV-VH ratio, and texture ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal, parallel)

# Function proper - argument should be filenames 'palsar, s1, and landsat'
RasProd <- function(satellite, sat.str) {
  
  
  if (sat.str == 'landsat') {
   # beginCluster(n=cores)
    ndvCalc <- function(x) {
      ndvi <- (satellite[[2]] - satellite[[1]]) / (satellite[[2]] + satellite[[1]])
      return(ndvi)
    }
    satellite$ndvi <- calc(x=satellite, fun=ndvCalc)
    
    
    
    
    
    
    
    satellite$ndvi <- (satellite[[2]] - satellite[[1]]) / (satellite[[2]] + satellite[[1]])
    satellite$savi <- (1 + 0.5) * ((satellite[[2]] - satellite[[1]])) / (satellite[[2]] + (2.4* satellite[[1]]) + 1)
    satellite$evi <- 2.5* (satellite[[2]] - satellite[[1]]) /(satellite[[2]] + (2.4* satellite[[1]]) + 1)
    satellite$rvi <- satellite[[2]] / satellite[[1]]
    satellite$wdvi <- (satellite[[2]] - 2 * satellite[[1]] )/ 100
    satellite$svi <- satellite[[1]] / satellite[[2]]
    names(satellite) <- c("red" ,  "nir",   "swir1", "swir2", "ndvi",  "savi"  ,"evi",  
                          'rvi', "wdvi", 'svi')
    return(satellite)
   # endCluster()
    }
    
  
  if (sat.str == 'palsar'){
   # beginCluster(n=cores)
    names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2')
    satellite$hv.hh1 <- satellite$hv1 / satellite$hh1 # ask what hv hh to use!!!!       
    satellite$hv.hh2 <- satellite$hv2 / satellite$hh2
    names(satellite) <- c('hh1', 'hv1', 'hh2', 'hv2', 'hv/hh1', 'hv/hh2')
    return (satellite)
   # endCluster()
    }
  
  if (sat.str == 's1'){
    #beginCluster(n=cores)
    names(satellite) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                          'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')
    satellite$vh.vv <- satellite$mean.vh / satellite$mean.vv        
    names(satellite) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 
                          'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv','vh/vv')
    return (satellite)
  #  endCluster()
    }
  
  if (sat.str == 'dem'){
  #  beginCluster(n=cores)
    slope <- terrain(satellite, c("slope"), neighbors=4, unit='degrees')
    satellite <- addLayer(satellite, slope)
    names(satellite) <- c('dem', 'slope')
    return (satellite)
  #  endCluster()
    }
  
  if (sat.str == 'lcov'){
  #  beginCluster(n=cores)
    satellite[satellite > 19] = NA #removing  NA labels (using 0-19 classifications)
    return (satellite)
  #  endCluster()
    } 
#closeAllConnections()
}


  
