######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 26.11.2021, version: 1.0
# 
# nPVI script
######################################################################################

# references:
# Burchardt, L. S., & Knörnschild, M. (2020). Comparison of methods for rhythm analysis of complex animals’ acoustic signals. PLOS Computational Biology, 16(4), e1007755. doi:https://doi.org/10.1371/journal.pcbi.1007755
# Grabe E, Low E. Durational Variability in Speech and the Rhythm Class Hypothesis. Laboratory Phonology VII. 7. Berlin: Mouton de Gruyter; 2002. p. 515–46
# Toussaint GT. The Geometry of Musical Rhythm: What Makes a "Good" Rhythm Good?. Boca Raton, FL: CRC Press; 2013.
# Ravignani A, Norton P. Measuring Ryhtmic Complexity: A Primer to Quantify and Compare Temporal Structure in Speec, Movement and Animal Vocalizations. Journal of Language Evolution. 2017; 2(1):4– 19, https://doi.org/10.1093/jole/lzx002


npvi <- function(ioi){

  
  ioi_seq <- drop_na(ioi_seq)
  z <- c()
  b <- c()
  
  for (l in 1: nrow(ioi_seq)){
  
z[l] <- (ioi_seq[l,1] - ioi_seq[l+1,1])
b[l] <- (ioi_seq[l,1] + ioi_seq[l+1,1])/2

  } #end of for loop
  
  z <- na.omit(z)
  b <- na.omit(b)
  c <- sum(abs(z/b))
  
  npvi <- c*(100/(length(z)-1))
  
  results_rhythm[a,9] <<- npvi
  
  }