### Script to open files at designated folders ###
if (!require("pacman")) install.packages("pacman")
pacman::p_load(raster, rgdal)

# Open files
FolderFiles <- function(type, ext) {
  setwd(mydir)
  if (type == 'raw') {allfiles <- list.files(paste0(mydir,'/data/'),  pattern = paste0('*.', ext, '$'))
  setwd(paste0(mydir,'/data/'))
    if (length(allfiles) == 0 )
    {allfiles <- list.dirs(paste0(mydir,'/data/')) 
      print('sub-folders existing:')
      print(list.dirs(paste0(mydir,'/data/')))}
      print('returning list of directories instead, open files by indexing sub-folders :)')} 
  if (type == 'intermediate'){allfiles <- list.files(paste0(mydir,'/mid-results/'),  pattern = paste0('*.', ext, '$'))
  setwd(paste0(mydir,'/mid-results/'))
  print (paste('loaded:', allfiles))}
  if (type == 'final'){allfiles <- list.files(paste0(mydir,'/final/'),  pattern = paste0('*.', ext, '$'))
  setwd(paste0(mydir,'/final/'))
  print (paste('loaded:', allfiles))}
  if (type == 'temp'){allfiles <- list.files(paste0(mydir,'/temp/'),  pattern = paste0('*.', ext, '$'))
  setwd(paste0(mydir,'/temp/'))
  print (paste('loaded:', allfiles))}
  
  
  
  return (allfiles)
  
}

