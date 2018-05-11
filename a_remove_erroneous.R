## Function to remove manually visualized erroneous plots (PRE-DEFINED) ##

NoErr <- function(spdf){
  t <- c(1,	3,	3,	4,	21,	25,	25,	32,	33,	33,	33,	34,	34,	34,	34,	35,	40,
         40,	41,	43,	43,	48,	49,	78,	78,	79,	112,	145,	149,	161,	161,	161,	
         172,	187,	211,	211,	218,	244,	250,	250,	253,	257,	257,	257,	
         257,	262,	262,	267,	267,	274,	275,	286,	286,	299,	354)
  p <- c(1,3,	4,	3,	1,	1,	3,	4,	2,	3,	4,	1,	2,	3,	4,	2,	3,	4,	3,	1,	2,
         3,	3,	3,	4,	3,	4,	4,	3,	2,	3,	4,	4,	4,	2,	4,	1,	1,	1,
         2,	4,	1,	2,	3,	4,	1,	2,	1,	2,	4,	2,	1,	4,	2,	3)
  tp <- as.data.frame(cbind(t,p))
  tp <- do.call(paste, c(tp[c("t", "p")], sep = "")) 
  
  spdf$ID <- do.call(paste, c(as.data.frame(spdf)[c("tract", "plot")], sep = "")) 
  
  '%ni%' <- Negate('%in%')
  no.err <-  subset(spdf, ID %ni% tp)
  return(no.err)
}