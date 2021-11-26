######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 26.11.2021, version: 1.0
# 
# recurrence plots
######################################################################################

recurrence <- function(data){

## 00a: load packages-----------

library(tidyverse)
library(crqa) #package with recurrence plot option, check if we really use it
library(vegan) #to calculate euclidean distance
library(corrplot)
  
## 00b: load example data ------

# ioi_seq
  
## 01: recurrence matrix

  #euclidian distance matrix (when using phillips way)
  
  eucl_dist <- (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
  eucl_dist <- eucl_dist[1:(nrow(eucl_dist)-1),1:(nrow(eucl_dist)-1) ]
  
  threshold <- mean(ioi_seq$X1, na.rm = TRUE)*0.1
  
  eucl_dist[eucl_dist < threshold] <- 0 
  
  # transform matrix as to be able to plot it with ggplot as tile plot
  #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
  
  eucl_dist_2 <- eucl_dist %>%
    tibble::as_tibble() %>%
    rownames_to_column('Var1') %>%
    gather(Var2, value, -Var1) %>%
    mutate(
      Var1 = factor(Var1, levels=1:11),              # levels has to be changed to dynamically change according to plot
      Var2 = factor(gsub("V", "", Var2), levels=1:1) # same issue for levels
    )
  
  ggplot(eucl_dist_2, aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    #geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "black") 
 

# matlab code from philipp
#threshold = mean(ioi) * 0.1;
# distanceMatrix = abs(bsxfun(@minus, ioi, transpose(ioi)) );  
#  % set values < threshold to 0
#  distanceMatrix(distanceMatrix<threshold) = 0; 
# binaryDistanceMatrix = distanceMatrix < threshold;
# imagesc(-distanceMatrix);  
  
#https://dk81.github.io/dkmathstats_site/rvisual-corrplots.html
  dat <- matrix(rnorm(100, 3, 1), ncol=10)
  
  ## reshape data (tidy/tall form)
  dat2 <- dat %>%
    tibble::as_tibble() %>%
    rownames_to_column('Var1') %>%
    gather(Var2, value, -Var1) %>%
    mutate(
      Var1 = factor(Var1, levels=1:10),
      Var2 = factor(gsub("V", "", Var2), levels=1:10)
    )
  
  ## plot data
  ggplot(dat2, aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    #geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "black") 
}