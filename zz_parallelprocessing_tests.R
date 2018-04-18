
### computer cores registration
UseCores <- detectCores() -1
cl<- makeCluster(UseCores)
registerDoParallel(cl)

### usual process
start.time <- Sys.time()
satellite$savi <- (1 + 0.5) * ((satellite[[2]] - satellite[[1]])) / (satellite[[2]] + (2.4* satellite[[1]]) + 1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


### glcm tests
# 9x9 window, 1 core, landsat
start.time <- Sys.time()
tex.red <- glcm(raster(l.mask, layer=1),window = c(9,9), shift=list(c(1,1), c(-1,-1)))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 9x9 window, 1 core, sentinel
start.time <- Sys.time()
tex.red <- glcm(raster(s.mask, layer=1),window = c(9,9), shift=list(c(1,1), c(-1,-1)))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# parallel per raster 
r.list <- list(l.mask, s.mask)
foreach (i=1:length(r.list), .packages=c('raster', 'glcm')) %dopar% {
  tex.red <- glcm(raster(r.list[[i]], layer=1),window = c(9,9), shift=list(c(1,1), c(-1,-1)))
  }

stopCluster(cl)

beginCluster(n=2)
# 3x3 window, 4 cores
start.time <- Sys.time()
tex.red <- glcm(raster(satellite, layer=1),window = c(9,9), shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
endCluster()

# 3x3 window, 1 core
start.time <- Sys.time()
tex.red <- glcm(raster(satellite, layer=1),window = c(9,9), shift=c(1,1))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


### resampling tests
beginCluster(n=2)
l.mask <- resample(landsat, s.mask[[1]])
endCluster()


satellite$evi <- 2.5* (satellite[[2]] - satellite[[1]]) /(satellite[[2]] + (2.4* satellite[[1]]) + 1)
satellite$rvi <- satellite[[2]] / satellite[[1]]
satellite$wdvi <- (satellite[[2]] - 2 * satellite[[1]] )/ 100
satellite$svi <- satellite[[1]] / satellite[[2]]



### VI_ratio tests
l.int <- RasProd(l.mask, 'landsat')
p.int <-RasProd(p.mask, 'palsar')
s.int <-RasProd(s.mask, 's1')
s.int <- subset(s.int, order(c(1,3,2,4,5,6,7,8,9,10,11,12)))
d.int <-RasProd(d.mask, 'dem')
lc.int <-RasProd(lc.mask, 'lcov')

l.ftex <- calc(l.int[[1:2]], fun=mean)