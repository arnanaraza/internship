### Function to mask and mosaic main raster inputs ###
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal)

# Open and mask Sentinel 1 images 
S1Files <- function(ext, orbit, s1dir) { #reads bin files (should be per orbit folder i.e. 'orbit_163')
  setwd(s1dir)
  allfiles <- list.files(paste0(s1dir,'/Orbit_',orbit),  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  setwd(paste0(s1dir,'/Orbit_',orbit))
  allfiles1 <- stack(allfiles[[1]])
  proj4string(allfiles1) <-  crs('+init=epsg:4326')
  ph.bounds <- readOGR(dsn = paste0(mydir,'/data/general/Country'), layer = "Country")
  allfiles.c <- crop(allfiles1, extent(ph.bounds))
  allfiles.m <- mask(allfiles.c, ph.bounds)
  writeRaster(allfiles.m, paste0('orb',orbit,'.tif'))
  setwd(mydir)
  return (allfiles.m)
}


# Open and mask Palsar images
#print(list.files('E:/Projects/Phillippines_All/PhilippinesTotalPalsar', pattern='.bin')) #palsar  dir
PalsarFiles <- function(ext, palsardir) {
  setwd(palsardir)
  allfiles <- list.files(palsardir,  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  allfiles1 <- stack(allfiles[[length(allfiles)]]) #pre-defined
  proj4string(allfiles1) <-  crs('+init=epsg:4326')
  ph.bounds <- readOGR(dsn = paste0(mydir,'/data/general/Country'), layer = "Country")
  allfiles.c <- crop(allfiles1, extent(ph.bounds))
  allfiles.m <- mask(allfiles.c, ph.bounds)
  writeRaster(allfiles.m, 'palsar.tif')
  setwd(mydir)
  return (allfiles.m)
}


# Open and mask Landsat images
LandsatFiles <- function(ext, year, landsatdir) {
  setwd(landsatdir)
  allfiles <- list.files(landsatdir,  pattern = paste0('*.', ext, '$'))
  allfiles <- files[grep(year, allfiles, fixed=T)]
  print (paste('loaded:', allfiles))
  allfilelandsat <- lapply(1:length(allfiles), function(x) stack(allfiles[[x]]))
  allfilelandsat$fun <- mean
  allfile.mo <- do.call(mosaic,allfilelandsat)
  proj4string(allfile.mo) <-  crs('+init=epsg:4326')
  ph.bounds <- readOGR(dsn = paste0(mydir,'/data/general/Country'), layer = "Country")
  allfiles.c <- crop(allfile.mo, extent(ph.bounds))
  allfiles.m <- mask(allfiles.c, ph.bounds)
  writeRaster(allfiles.m, paste0(' ',year,' ','.tif'))
  setwd(mydir)
  return (allfiles.m)
}


# Open and mask DEM (srtm 30m)
DEMFiles <- function(ext, demdir) {
  setwd(demdir)
  allfiles <- list.files(demdir,  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  allfiles <- raster(allfiles[[1]])
  proj4string(allfiles) <-  crs('+init=epsg:4326')
  ph.bounds <- readOGR(dsn = paste0(mydir,'/data/general/Country'), layer = "Country")
  allfiles.c <- crop(allfiles, extent(ph.bounds))
  allfiles.m <- mask(allfiles.c, ph.bounds)
  writeRaster(allfiles.m, 'dem.tif', overwrite=T)
  setwd(mydir)
  return (allfiles.m)
}


# Open and mask Lcover2014 
LcovFiles <- function(ext, lcovdir) {
  setwd(lcovdir)
  allfiles <- list.files(lcovdir,  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  allfiles <- raster(allfiles[[1]])
  proj4string(allfiles) <-  crs('+init=epsg:4326')
  ph.bounds <- readOGR(dsn = paste0(mydir,'/data/general/Country'), layer = "Country")
  allfiles.c <- crop(allfiles, extent(ph.bounds))
  allfiles.m <- mask(allfiles.c, ph.bounds)
  writeRaster(allfiles.m, 'lcov.tif', overwrite=T)
  setwd(mydir)
  return (allfiles.m)
}
