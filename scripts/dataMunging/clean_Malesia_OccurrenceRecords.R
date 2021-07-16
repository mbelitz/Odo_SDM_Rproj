library(rnaturalearth)
library(tidyverse)

source("scripts/pipelineFunctions/02-5_clean_occurrences_noIQR.R")
source("scripts/pipelineFunctions/03_define_accessibleArea.R")
source('scripts/pipelineFunctions/03-5_define_accessibleArea_alphaHullFail.R')

nco <- data.table::fread("Occ_Data/AAAMapsMalesia.csv")

nco <- nco %>% 
  dplyr::rename("status" = 1,
                "scientific_name" = 2, 
                "decimalLatitude" = 7, "decimalLongitude" = 8) %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude, status) %>% 
  mutate(validName = scientific_name)

nco <- nco %>% 
  mutate(decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude))

world <- rnaturalearth::ne_countries(returnclass = "sf")

makemaps <- function(binomial){
  
  spdf <- nco %>% 
    filter(scientific_name == binomial)
  
  cc <- coord_clean_noOutliers(species = binomial, df = nco)
  
  removed_pts <- spdf %>% 
    dplyr::filter(!decimalLatitude %in% cc$decimalLatitude &
                    !decimalLongitude %in% cc$decimalLongitude) 
  
  p1 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(spdf, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "deeppink") + 
    coord_sf(xlim = c(min(spdf$decimalLongitude) - 10, max(spdf$decimalLongitude) + 10),
             ylim = c(min(spdf$decimalLatitude) - 10, max(spdf$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") + 
    theme_bw()
  
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(removed_pts, mapping = aes(x = decimalLongitude, y = decimalLatitude,
                                          color = "Removed Pts")) +
    geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude,
                                 color = "Cleaned"))  +
    coord_sf(xlim = c(min(spdf$decimalLongitude) - 10, max(spdf$decimalLongitude) + 10),
             ylim = c(min(spdf$decimalLatitude) - 10, max(spdf$decimalLatitude) + 10)) +
    scale_color_manual(values = c("Black", "deeppink")) +
    labs(color = "") +
    ggtitle("Cleaned") +
    theme_bw() +
    theme(legend.position = "bottom")
  
  aa_shp <- define_accessibleArea(species_df = cc, minBuff = 75000,
                                  saveImage = FALSE,
                                  saveShapefile = FALSE)
  
  if(range(cc$decimalLongitude)[2] - range(cc$decimalLongitude)[1] > 1.5 &
     range(cc$decimalLatitude)[2] - range(cc$decimalLatitude)[1] > 1.5){
    
    p3 <- ggplot() + 
      geom_sf(world, mapping = aes(), fill = NA) +
      geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "Orange", alpha = 0.85) +
      geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
                 color = "black")  +
      coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
               ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
      ggtitle(binomial) +
      theme_classic()
  } else if(range(cc$decimalLongitude)[2] - range(cc$decimalLongitude)[1] > 1.5 |
            range(cc$decimalLatitude)[2] - range(cc$decimalLatitude)[1] > 1.5){
    
    p3 <- ggplot() + 
      geom_sf(world, mapping = aes(), fill = NA) +
      geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "purple", alpha = 0.85) +
      geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
                 color = "black")  +
      coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
               ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
      ggtitle(binomial) +
      theme_classic()  
    
  } else {
    p3 <- ggplot() + 
      geom_sf(world, mapping = aes(), fill = NA) +
      geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "green", alpha = 0.85) +
      geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
                 color = "black")  +
      coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
               ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
      ggtitle(binomial) +
      theme_classic()
  }
  
  cp <- egg::ggarrange(p1, p2, p3, ncol = 2)
  
  dir.create(paste("Outputs/Malesia/", binomial, sep = ""))
  
  ggsave(filename = paste("Outputs/Malesia/", 
                          binomial, "/CoordClean.png", 
                          sep = ""),
         plot = cp, width = 10, height = 6)
  
  write.csv(x = cc, file = paste("Outputs/Malesia/", 
                                 binomial, "/occurrenceRecords.csv", 
                                 sep = ""),
            row.names = F)
  
}



## read in species list
taxo <- unique(nco$scientific_name)

spp_list <- taxo[1:5] 
spp_list <- taxo[6:329] 

for(i in seq_along(spp_list)){
  tryCatch(makemaps(binomial = spp_list[i]),
           error = function(e) print(paste(spp_list[i], "Error in Code Skipping for now!")))
}
