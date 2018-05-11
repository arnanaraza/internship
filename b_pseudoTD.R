## Function to get per class statistics and create random poitns (pseudo TD)
pacman::p_load(raster, rgeos, rgdal)

PTD <- function (spdf, nclass, for_mask, n){
  #open forest classes - GIS-made (protected areas, key biodiversity areas, closed forests, sloping and high altitude)
  fmask <- readOGR(dsn=paste0(mydir,'/data/'), layer = for_mask)
  
  #clip with forest mask 
  crs(fmask) <- crs(spdf)
  class5 <- fmask[fmask$OBJECTID == 5,]
  class4 <- fmask[fmask$OBJECTID == 4,]
  class3 <- fmask[fmask$OBJECTID == 3,]
  mask5 <- raster::intersect(spdf, class5)
  mask4 <- raster::intersect(spdf, class4)
  mask3 <- raster::intersect(spdf, class3)
  print(summary(mask5$biomass.ton))
  print(summary(mask4$biomass.ton))
  print(summary(mask3$biomass.ton))
  
  #get number of points per plot
  ptd5 <- spsample(class5,n=n,type="random")
  ptd5 <- SpatialPointsDataFrame(ptd5, spdf@data[c(1:n),])
  ptd5@proj4string <- spdf@proj4string
  ptd5$long <- ptd5@coords[,1]
  ptd5$lat <- ptd5@coords[,2]
  ptd5@bbox <- spdf@bbox
  sc <- spdf@coords[,c(1,2)]
  spdf@coords <- ptd5@coords
  spdf@coords <- sc
  if (nclass == 6){
    ptd5$bioclass <- 6
    with5 <- rbind(spdf,ptd5)}
  if (nclass == 5){
    ptd5$bioclass <- 5
    with5 <- rbind(spdf,ptd5)}
  
  ptd4 <- spsample(class4,n=n,type="random")
  ptd4 <- SpatialPointsDataFrame(ptd4, spdf@data[c(1:n),])
  ptd4@proj4string <- spdf@proj4string
  ptd4$long <- ptd4@coords[,1]
  ptd4$lat <- ptd4@coords[,2]
  ptd4@bbox <- spdf@bbox
  sc <- with5@coords[,c(1,2)]
  with5@coords <- ptd4@coords
  with5@coords <- sc
  if (nclass == 6){
    ptd4$bioclass <- 5
    with45 <- rbind(with5,ptd4)}
  if (nclass == 5){
    ptd4$bioclass <- 4
    with45 <- rbind(with5,ptd4)}
  
  ptd3 <- spsample(class3,n=n,type="random")
  ptd3 <- SpatialPointsDataFrame(ptd3, spdf@data[c(1:n),])
  ptd3@proj4string <- spdf@proj4string
  ptd3$long <- ptd3@coords[,1]
  ptd3$lat <- ptd3@coords[,2]
  ptd3@bbox <- spdf@bbox
  sc <- with45@coords[,c(1,2)]
  with45@coords <- ptd3@coords
  with45@coords <- sc
  if (nclass == 6){
    ptd3$bioclass <- 4
    with345 <- rbind(with45,ptd3)}
  if (nclass == 5){
    ptd3$bioclass <- 3
    with345 <- rbind(with45,ptd3)}
  
  return(with345)
}












