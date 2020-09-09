## Installing required packges
package_list <- c(
  "reshape2",
  "ggplot2",
  "ggpubr",
  "dplyr",
  "tidyverse",
  "kableExtra",
  "gridExtra",
  "RColorBrewer",
  "cowplot",
  "cellranger"
)
if(F){
  install.packages(package_list)
}
lapply(package_list, function(x) library(x, character.only = T))
rm(package_list)
