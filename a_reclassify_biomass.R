pacman::p_load(classInt, data.table)

RecPlot <- function(spdf, class_number, style) {
  class <- classIntervals(spdf$biomass.ton, n=class_number, style=style)
  brk <- class$brks
  spdf$bioclass <- spdf$biomass.ton
  if (class_number == 3) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
              spdf$bioclass > brk[3], 3, 0))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    return(spdf)}
  
  if (class_number == 4) {
  #  class <- classIntervals(spdf$biomass.ton, n=class_number, style='fixed', fixedBreaks = c(10,50,100,150,250,650))
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
              spdf$bioclass > brk[4], 4, 0)))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    return(spdf)}
  
  if (class_number == 5) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5], 5, 0))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    return(spdf)}
  
  if (class_number == 6) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                    spdf$bioclass > brk[6], 6, 0)))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
  
  if (class_number == 7) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                spdf$bioclass > brk[6] & spdf$bioclass <= brk[7], 6, ifelse(
                    spdf$bioclass > brk[7], 7, 0))))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
  
  
  if (class_number == 8) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                spdf$bioclass > brk[6] & spdf$bioclass <= brk[7], 6, ifelse(
                  spdf$bioclass > brk[7] & spdf$bioclass <= brk[8], 7, ifelse(
                    spdf$bioclass > brk[8], 8, 0)))))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
  
  if (class_number == 12) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                spdf$bioclass > brk[6] & spdf$bioclass <= brk[7], 6, ifelse(
                  spdf$bioclass > brk[7] & spdf$bioclass <= brk[8], 7, ifelse(
                    spdf$bioclass > brk[8] & spdf$bioclass <= brk[9], 8, ifelse(
                      spdf$bioclass > brk[9] & spdf$bioclass <= brk[10], 9, ifelse(
                        spdf$bioclass > brk[10] & spdf$bioclass <= brk[11], 10, ifelse(
                          spdf$bioclass > brk[11] & spdf$bioclass <= brk[12], 11, ifelse(
                            spdf$bioclass > brk[12], 12, 0)))))))))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
  
  if (class_number == 15) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                spdf$bioclass > brk[6] & spdf$bioclass <= brk[7], 6, ifelse(
                  spdf$bioclass > brk[7] & spdf$bioclass <= brk[8], 7, ifelse(
                    spdf$bioclass > brk[8] & spdf$bioclass <= brk[9], 8, ifelse(
                      spdf$bioclass > brk[9] & spdf$bioclass <= brk[10], 9, ifelse(
                        spdf$bioclass > brk[10] & spdf$bioclass <= brk[11], 10, ifelse(
                          spdf$bioclass > brk[11] & spdf$bioclass <= brk[12], 11, ifelse(
                            spdf$bioclass > brk[12] & spdf$bioclass <= brk[13], 12, ifelse(
                              spdf$bioclass > brk[13] & spdf$bioclass <= brk[14], 13, ifelse(
                                spdf$bioclass > brk[14] & spdf$bioclass <= brk[15], 14, ifelse(
                                  spdf$bioclass > brk[15], 15, 0))))))))))))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
  
  if (class_number == 20) {
    spdf$bioclass <- with(spdf, ifelse(
      spdf$bioclass >= brk[1] & spdf$bioclass <= brk[2], 1, ifelse(
        spdf$bioclass > brk[2] & spdf$bioclass <= brk[3], 2, ifelse(
          spdf$bioclass > brk[3] & spdf$bioclass <= brk[4], 3, ifelse(
            spdf$bioclass > brk[4] & spdf$bioclass <= brk[5], 4, ifelse(
              spdf$bioclass > brk[5] & spdf$bioclass <= brk[6], 5, ifelse(
                spdf$bioclass > brk[6] & spdf$bioclass <= brk[7], 6, ifelse(
                  spdf$bioclass > brk[7] & spdf$bioclass <= brk[8], 7, ifelse(
                    spdf$bioclass > brk[8] & spdf$bioclass <= brk[9], 8, ifelse(
                      spdf$bioclass > brk[9] & spdf$bioclass <= brk[10], 9, ifelse(
                        spdf$bioclass > brk[10] & spdf$bioclass <= brk[11], 10, ifelse(
                          spdf$bioclass > brk[11] & spdf$bioclass <= brk[12], 11, ifelse(
                            spdf$bioclass > brk[12] & spdf$bioclass <= brk[13], 12, ifelse(
                              spdf$bioclass > brk[13] & spdf$bioclass <= brk[14], 13, ifelse(
                                spdf$bioclass > brk[14] & spdf$bioclass <= brk[15], 14, ifelse(
                                  spdf$bioclass > brk[15] & spdf$bioclass <= brk[16], 15, ifelse(
                                    spdf$bioclass > brk[16] & spdf$bioclass <= brk[17], 16, ifelse(
                                      spdf$bioclass > brk[17] & spdf$bioclass <= brk[18], 17, ifelse(
                                        spdf$bioclass > brk[18] & spdf$bioclass <= brk[19], 18, ifelse(
                                          spdf$bioclass > brk[19] & spdf$bioclass <= brk[20], 19, ifelse(
                                            spdf$bioclass > brk[20], 20, 0)))))))))))))))))))))
    spdf$bioclass
    unique(spdf$bioclass)
    print(brk)
    
    return(spdf)}
}




