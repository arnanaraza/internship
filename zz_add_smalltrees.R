library(data.table)
library(dplyr)

small.trees <- nfi14.fin[which( nfi14.fin$Diameter < 20 ),]
summary(small.trees)
small.trees


bio.sum.14.small <- aggregate(small.trees$biomass ~ small.trees$Tract.No. + small.trees$Plot.No.,
                        data=small.trees,FUN=sum)
colnames(bio.sum.14.small)<- c('tract', 'plot', 'biomass')

bio.sum.14.small$biomas.small <- bio.sum.14.small$biomass * 8.3 # 100/12, 12% = three 20x10 plots for small trees!
mean(bio.sum.14.small$biomas.small)

nrow (bio.sum.14)
nrow(bio.sum.14.small)

bio.sum.14 <- aggregate(nfi14.fin$biomass ~ nfi14.fin$Tract.No. + nfi14.fin$Plot.No.,
                        data=nfi14.fin,FUN=sum)
colnames(bio.sum.14)<- c('tract', 'plot', 'biomass')

## not tallying because there are plots without small trees
d1 <- setDT(bio.sum.14)
d2 <- setDT(bio.sum.14.small)
names(d1) <- c('tract', 'plot', 'biomass')
names(d2) <- c('tract2', 'plot2', 'biomass2', 'biomass.small')


asdf <- as.data.frame(d1 %>% 
  left_join(d2, by=c('tract' = 'tract2', 'plot'='plot2')))

asdf$biomass.total <- asdf$biomass+asdf$biomass.small
