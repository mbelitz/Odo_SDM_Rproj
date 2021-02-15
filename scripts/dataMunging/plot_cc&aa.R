library(png)
library(ggplot2)
library(cowplot)
library(dplyr)

# path to accessible area pngs
path_aa <- list.files(path = "g:/UF3/Ode_SDMs/ModelResultsNA/ModelResults/", 
                      pattern = "*occs.png",
                      recursive = T, full.names = T)

# path to response curves
path_rc <- list.files(path = "g:/UF3/Ode_SDMs/ModelResultsNA/ModelResults/", 
                      pattern = "*Curves.png",
                      recursive = T,full.names = T)


# path to all pngs
path_all <- list.files(path = "g:/UF3/Ode_SDMs/ModelResultsNA/ModelResults/", 
                      pattern = "*.png",
                      recursive = T,full.names = T)

# paths to only cc
path_notrc <- path_all[!path_all %in% path_rc]
path_cc <- path_notrc[!path_notrc %in% path_aa]


# sppecies list
test_spp <- read.csv("NA_SDM_testSpp.csv")

# function to plot pngs together

for(i in 1:length(test_spp$binomial)){
  
  png1 <- readPNG(path_aa[i])
  png2 <- readPNG(path_cc[i])
  
  p1 <- ggdraw() + draw_image(png1)
  p2 <- ggdraw() + draw_image(png2)
  
  cp <- cowplot::plot_grid(p2,p1, ncol = 1, nrow =2, rel_heights = c(1,1.5))

  ggsave(filename = paste("g:/UF3/Ode_SDMs/Occurrrence_PNGs", "/", test_spp$binomial[i], "_cc.png",
                sep = ""), plot = cp,
         width = 5, height = 7)

}
