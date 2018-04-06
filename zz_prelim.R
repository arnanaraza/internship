#### Script to open rasters ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster,rgdal)
mydir <- setwd('D:/Biomass')


# Open files
FolderFiles <- function(folder, ext) {
  allfiles <- list.files(paste0(mydir,'/data/', folder),  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  return (allfiles)
}

RasFiles <- FolderFiles ('radar', 'ovr')

# Name bandsm compute hv-hh ratio also
palsar <- stack(paste0(mydir,'/data/radar/', RasFiles[[1]]))
proj4string(palsar) <- crs('+init=epsg:4326')
names(palsar) <- c('hh', 'hv', 'hh1', 'hv1')
palsar$hv.hh <- palsar$hv / palsar$hh
palsar$hv.hh1 <- palsar$hv1 / palsar$hh1

s1 <- stack(paste0(mydir,'/data/radar/', RasFiles[[3]]))
proj4string(s1) <-  crs('+init=epsg:4326')
names(s1) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')

landsat <- stack(paste0(mydir,'/data/radar/', RasFiles[[5]]))
proj4string(landsat) <-  crs('+init=epsg:4326')
names(landsat) <- c('red', 'nir', 'swir1', 'swir2')

setwd(paste0(mydir, '/scripts'))
source('preprocess_rasters.R')
setwd(mydir)

ls <- PP.Rasters (landsat, s1)
sp <- PP.Rasters (s1, palsar)
spl <- PP.Rasters(landsat, sp)

