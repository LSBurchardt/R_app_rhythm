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
library(vegan) #to calculate euclidean distance
library(corrplot)
  
## 00b: load example data ------

#data input from function
  
  
## 01: data to ioi sequence ------------------------
  ioi_seq <- data.frame()                 # set up empty dataframe to store ioi values in
  
  #for (a in filenumber){             #start of loop for number of files, needs to be added, maybe better add in main! 
  
  for (x in  1:nrow(data)) {          # start of loop through rows of data to calculate iois
    
    z = x+1 
    ioi_seq[x,1] <- data[z,1]-data[x,1]
    
  }
  
## 02: recurrence matrix ---------------------

  #euclidian distance matrix (when using phillips way)
  
  eucl_dist <- (as.matrix(vegdist(ioi_seq, "euclidean", na.rm = TRUE)))
  eucl_dist <- eucl_dist[1:(nrow(eucl_dist)-1),1:(nrow(eucl_dist)-1) ]
  
  threshold <- mean(ioi_seq$X1, na.rm = TRUE)*0.1
  
  eucl_dist[eucl_dist < threshold] <- 0 
  
  # transform matrix as to be able to plot it with ggplot as tile plot
  #https://stackoverflow.com/questions/14290364/heatmap-with-values-ggplot2
  
  levels <- 1:(nrow(eucl_dist))
  
  eucl_dist_2 <- eucl_dist %>%
    tibble::as_tibble() %>%
    rownames_to_column('Var1') %>%
    gather(Var2, value, -Var1) %>%
    mutate(
      Var1 = factor(Var1, levels = levels),              
      Var2 = factor(gsub("V", "", Var2), levels = levels) 
    )
## 03: recurrence plot -----------------
    
  ggplot(eucl_dist_2, aes(Var1, Var2)) +
    geom_tile(aes(fill = value)) + 
    #geom_text(aes(label = round(value, 1))) +
    scale_fill_gradient(low = "white", high = "black") 

  
  ggsave(paste('plots/recurrence_',a,'.jpg', sep = ""),
         dpi =300 , 
         device = "jpg")  
   
}
