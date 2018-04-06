### Script to preprocess national forest inventory data
### Writes biomass value per plot (SPDF format) using desired allometric equation

# Preliminaries - packages, directory
pacman::p_load(rgdal, raster)

# Function proper
NFI <- function(equation, year, stat) {
  # Load nfi data in csv format 
  nfi03 <- read.csv(paste0(mydir,'/data/NFI/PP_2003NFI_2018.csv'))
  nfi14<- read.csv(paste0(mydir,'/data/NFI/PP_2014NFI_2018.csv'))
  
  if (stat == 'yes'){
    # Summary statistics assessment
    par(mfrow=c(1,2))
    hist(nfi03$X61.TotalHeight)
    hist(nfi14$Total.Height)
    hist(nfi03$X58.Dbh)
    hist(nfi14$Diameter)
    summary(nfi03)
    summary(nfi14)}
    
  # Specify coordinates
  coord <- crs('+init=epsg:4326')
  
  # Get columns for biomass computation only
  nfi03.fin <- nfi03[c(1,2,9,11,14)]
  nfi14.fin <- nfi14[c(1,2,7,8,9)]
  
  # Compute biomass
  if (equation == 'Chave14'){
    nfi03.fin$biomass <- 0.0673*((nfi03.fin$zz.Wood.Density*(nfi03.fin$X58.Dbh^2)*nfi03.fin$X61.TotalHeight)^0.976)
    nfi14.fin$biomass <- 0.0673*((nfi14.fin$Mean.Wood.Density*(nfi14.fin$Diameter^2)*nfi14.fin$Total.Height)^0.976)
  }
  # Create biomass per pixel and integrate to points
  
  #pivot per plot per tract
  bio.sum.03 <- aggregate(nfi03.fin$biomass ~ nfi03.fin$tract + nfi03.fin$PlotNo,
                          data=nfi03.fin,FUN=sum)
  colnames(bio.sum.03)<- c('tract', 'plot', 'biomass')
  
  bio.sum.14 <- aggregate(nfi14.fin$biomass ~ nfi14.fin$Tract.No. + nfi14.fin$Plot.No.,
                          data=nfi14.fin,FUN=sum)
  colnames(bio.sum.14)<- c('tract', 'plot', 'biomass')
  
  #plot the coordinates per plot
  p1 <- read.csv(paste0(mydir,'/data/NFI/plot1.csv'))
  p2 <- read.csv(paste0(mydir,'/data/NFI/plot2.csv'))
  p3 <-read.csv(paste0(mydir,'/data/NFI/plot3.csv'))
  p4 <- read.csv(paste0(mydir,'/data/NFI/plot4.csv'))
  
  #assign biomass values per plot
  #do it for 2003
  cummu1 <- 1:length(bio.sum.03$plot[bio.sum.03$plot==1])
  cummu2 <- (1+length(cummu1)):
    (length(cummu1) + length(bio.sum.03$plot[bio.sum.03$plot==2]))
  cummu3 <- (1+length(cummu2)+length(cummu1)):
    (length(cummu1) + length(cummu2) + length(bio.sum.03$plot[bio.sum.03$plot==3]))
  cummu4 <- (1 + length(cummu3)+length(cummu2)+length(cummu1)):
    (length(cummu1) + length(cummu2) + length(cummu3) +length(bio.sum.03$plot[bio.sum.03$plot==4]))
  
  plots1.03 <- merge(bio.sum.03[cummu1,], p1, by='tract')
  plots2.03 <- merge(bio.sum.03[cummu2,], p2, by='tract')
  plots3.03 <- merge(bio.sum.03[cummu3,], p3, by='tract')
  plots4.03 <- merge(bio.sum.03[cummu4,], p4, by='tract')
  plots.fin.03 <- rbind(plots1.03,plots2.03,plots3.03,plots4.03)
  
  all.plots.03 <- plots.fin.03[c('long', 'lat', 'tract', 'plot', 'biomass')]
  all.plots.03 <- SpatialPointsDataFrame(all.plots.03, all.plots.03[,1:5], proj4string=crs(coord))
  writeOGR(obj=all.plots.03, dsn=paste0(mydir,'/mid-results'), driver="ESRI Shapefile", layer='all.plots.03', overwrite_layer = T)
  
  #do it for 2015
  cummu1 <- 1:length(bio.sum.14$plot[bio.sum.14$plot==1])
  cummu2 <- (1+length(cummu1)):
    (length(cummu1) + length(bio.sum.14$plot[bio.sum.14$plot==2]))
  cummu3 <- (1+length(cummu2)+length(cummu1)):
    (length(cummu1) + length(cummu2) + length(bio.sum.14$plot[bio.sum.14$plot==3]))
  cummu4 <- (1 + length(cummu3)+length(cummu2)+length(cummu1)):
    (length(cummu1) + length(cummu2) + length(cummu3) +length(bio.sum.14$plot[bio.sum.14$plot==4]))
  
  
  plots1.14 <- merge(bio.sum.14[cummu1,], p1, by='tract')
  plots2.14 <- merge(bio.sum.14[cummu2,], p2, by='tract')
  plots3.14 <- merge(bio.sum.14[cummu3,], p3, by='tract')
  plots4.14 <- merge(bio.sum.14[cummu4,], p4, by='tract')
  plots.fin.14 <- rbind(plots1.14,plots2.14,plots3.14,plots4.14)
  
  all.plots.14 <- plots.fin.14[c('long', 'lat', 'tract', 'plot', 'biomass')]
  all.plots.14 <- SpatialPointsDataFrame(all.plots.14, all.plots.14[,1:5], proj4string=crs(coord))
  writeOGR(obj=all.plots.14, dsn=paste0(mydir,'/mid-results'), driver="ESRI Shapefile", layer='all.plots.14', overwrite_layer = T)
  
  if (year == 03) {return (all.plots.03)}
  if (year == 14) {return (all.plots.14)}
}


# Get per plot ratio of small (<10cm DBH)and large trees (>20cm DBH)
#plot.sum.03 <- aggregate(nfi03.fin$X58.Dbh ~ nfi03.fin$tract + nfi03.fin$PlotNo + nfi03.fin$X58.Dbh,
 #                       data=nfi03.fin, FUN=sum)

# Blow up small trees at plot


