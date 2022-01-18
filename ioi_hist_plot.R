######################################################################################
# Rhythm Analysis Methods in R 
# Transfering script from Matlab to R
# author: Dr. Lara S. Burchardt
# 
# date: 08.11.2021, version: 1.0
# 
# IOI plot script for whole dataset
######################################################################################

ioi_hist_plot <- function(data){
  
  
  gather(ioi_all, cols, value) %>% 
    ggplot(aes(x= value))+
    geom_histogram(color = "white", fill = "darkblue", na.rm = TRUE)+               #change binwidth here
    aes(y=stat(count)/sum(stat(count))*100) +     # y is shown in percentages
    xlab("IOI [sec]")+                            
    ylab("Percentage [%]")+
    theme_minimal()
  
  
  ggsave(paste('plots//histogram_iois_', savename, '_whole_dataset.jpg', sep = ""),
         dpi =300 , 
         device = "jpg")
   
}