setwd('/media/sarvision/InternshipFilesAraza/BiomassPhilippines/mid-results/test_predictions')
writeRaster(predRF, 'pred4_all_RF_50.tif')

writeRaster(predRF, 'pred5_lsat_RF_60_buff.tif')

writeRaster(predRF1, 'pred5_all_RF_50.tif')
writeRaster(predRF1, 'pred6_lsat03_RF_70.tif')
writeRaster(rf.loocv, 'pred5_loo_all_RF_50.tif')

setwd(mydir)

