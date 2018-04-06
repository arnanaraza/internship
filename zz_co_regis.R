#### Script to co-register raster images ####

if (!require("pacman")) install.packages("pacman")
pacman::p_load(RStoolbox, raster, reshape2, ggplot2)
mydir <- setwd('D:/Biomass')


# Open files
FolderFiles <- function(folder, ext) {
  allfiles <- list.files(paste0(mydir,'/data/', folder),  pattern = paste0('*.', ext, '$'))
  print (paste('loaded:', allfiles))
  return (allfiles)
}

RasFiles <- FolderFiles ('radar', 'ovr')
palsar <- stack(paste0(mydir,'/data/radar/', RasFiles[[1]]))
names(palsar) <- c('hh', 'hv', 'hh1', 'hv1')
palsar$hv.hh <- palsar$hv / palsar$hh
palsar$hv.hh1 <- palsar$hv1 / palsar$hh1

s1 <- stack(paste0(mydir,'/data/radar/', RasFiles[[3]]))
names(s1) <- c('min.vh', 'min.vv', 'mean.vh', 'mean.vv', 'max.vh', 'max.vv', 'sd.vh', 'sd.vv', 'range.vh', 'range.vv', 'variab.vh', 'variab.vv')

landsat <- stack(paste0(mydir,'/data/radar/', RasFiles[[5]]))
names(landsat) <- c('red', 'nir', 'swir1', 'swir2')


# Coregister images (and report statistics)
reference <- landsat
missreg <- shift(reference, x = 2, y = 3)

coreg <- coregisterImages(palsar, s1, reportStats = TRUE)

ggplot(coreg$MI) + geom_raster(aes(x,y,fill=mi))
df <- melt(coreg$jointHist)   
df$L1 <- factor(df$L1, levels = names(coreg$jointHist))
df[df$value == 0, "value"] <- NA ## don't display p = 0
ggplot(df) + geom_raster(aes(x = Var2, y = Var1,fill=value)) + facet_wrap(~L1) + 
  scale_fill_gradientn(name = "p", colours =  heat.colors(10), na.value = NA)

ggR(reference) +
  ggR(coreg$coregImg, ggLayer=TRUE) 

writeRaster(coreg$coregImg, 'coreg.tif')

