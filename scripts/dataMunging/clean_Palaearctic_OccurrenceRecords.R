library(rnaturalearth)
library(tidyverse)

source("scripts/pipelineFunctions/02_clean_occurrences.R")
source("scripts/pipelineFunctions/03_define_accessibleArea.R")
source('scripts/pipelineFunctions/03-5_define_accessibleArea_alphaHullFail.R')

nco <- data.table::fread("Occ_Data/AAA Map Palaearctic.txt")

nco <- nco %>% 
  dplyr::rename("decimalLatitude" = 1, "decimalLongitude" = 2, "scientific_name" = 3,
                "Country" = 4, "QM" = 5, "status" = 6) %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude, status) %>% 
  mutate(validName = scientific_name)

nco <- nco %>% 
  mutate(decimalLatitude = as.numeric(decimalLatitude),
         decimalLongitude = as.numeric(decimalLongitude))

world <- rnaturalearth::ne_countries(returnclass = "sf")

makemaps <- function(binomial){
  
  spdf <- nco %>% 
    filter(scientific_name == binomial)
  
  cc <- coord_clean(species = binomial, df = nco)
  
  p1 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(spdf, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(spdf$decimalLongitude) - 10, max(spdf$decimalLongitude) + 10),
             ylim = c(min(spdf$decimalLatitude) - 10, max(spdf$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") +
    theme_bw()
  
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
             ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
    ggtitle("Cleaned") +
    theme_bw()
  
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
  
  cp <- cowplot::plot_grid(p1, p2, p3, rel_heights = c(1,1,3), rel_widths = c(1,1,3))
  
  dir.create(paste("Outputs/Palaearctic/", binomial, sep = ""))
  
  ggsave(filename = paste("Outputs/Palaearctic/", 
                          binomial, "/CoordClean.png", 
                          sep = ""),
         plot = cp)
  
  write.csv(x = cc, file = paste("Outputs/Palaearctic/", 
                                 binomial, "/occurrenceRecords.csv", 
                                 sep = ""),
            row.names = F)
  
}


makemaps_failedAccessibleArea <- function(binomial){
  
  spdf <- nco %>% 
    filter(scientific_name == binomial)
  
  cc <- coord_clean(species = binomial, df = nco)
  
  p1 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(spdf, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(spdf$decimalLongitude) - 10, max(spdf$decimalLongitude) + 10),
             ylim = c(min(spdf$decimalLatitude) - 10, max(spdf$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") +
    theme_bw()
  
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
             ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
    ggtitle("Cleaned") +
    theme_bw()
  
    aa_shp <- define_FailedaccessibleArea(species_df = cc, minBuff = 75000,
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
  
  
  cp <- cowplot::plot_grid(p1, p2, p3, rel_heights = c(1,1,3), rel_widths = c(1,1,3))
  
  dir.create(paste("Outputs/Palaearctic/", binomial, sep = ""))
  
  ggsave(filename = paste("Outputs/Palaearctic/", 
                          binomial, "/CoordClean.png", 
                          sep = ""),
         plot = cp)
  
  write.csv(x = cc, file = paste("Outputs/Palaearctic/", 
                                 binomial, "/occurrenceRecords.csv", 
                                 sep = ""),
            row.names = F)
  
}

# make maps for sigleton species
makemaps_singletons <- function(binomial){
  
  spdf <- nco %>% 
    filter(scientific_name == binomial)
  
  cc <- coord_clean(species = binomial, df = nco)
  
  p1 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(spdf, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(spdf$decimalLongitude) - 10, max(spdf$decimalLongitude) + 10),
             ylim = c(min(spdf$decimalLatitude) - 10, max(spdf$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") +
    theme_bw()
  
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
             ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
    ggtitle("Cleaned") +
    theme_bw()
    
    temp <- cc
    #Calculate buffer in meters
    coordinates(temp) <- ~ decimalLongitude + decimalLatitude
    proj4string(temp) <- CRS("+proj=longlat +datum=WGS84")
    aa_shp <- buffer(temp, 75000)
    
    p3 <- ggplot() + 
      geom_sf(world, mapping = aes(), fill = NA) +
      geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "green", alpha = 0.85) +
      geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
                 color = "black")  +
      coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
               ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
      ggtitle(binomial) +
      theme_classic()
  
  cp <- cowplot::plot_grid(p1, p2, p3, rel_heights = c(1,1,3), rel_widths = c(1,1,3))
  
  dir.create(paste("Outputs/Palaearctic/", binomial, sep = ""))
  
  ggsave(filename = paste("Outputs/Palaearctic/", 
                          binomial, "/CoordClean.png", 
                          sep = ""),
         plot = cp)
  
  write.csv(x = cc, file = paste("Outputs/Palaearctic/", 
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

a <- list.files("Outputs/Palaearctic/",
                pattern = "*.png", recursive = T)
spp_with_map <- word(a, start = 1, end = 1, sep = "/")

spp_without_map <- nco %>% 
  filter(!scientific_name %in% spp_with_map)

spp_without_map_names <- unique(spp_without_map$scientific_name)

singletons <- spp_without_map %>% 
  group_by(scientific_name) %>% 
  summarise(numObs = n())

singletons_names <- filter(singletons, numObs == 1)$scientific_name

## run singleton dot mapps
for(i in seq_along(singletons_names)){
  tryCatch(makemaps_singletons(binomial = singletons_names[i]),
           error = function(e) print(paste(singletons_names[i], "Error in Code Skipping for now!")))
}


# run dot maps for the rest of the species without dot maps but not singletons
spp_without_map_names <- filter(singletons, numObs > 1)$scientific_name

for(i in seq_along(spp_without_map_names)){
  tryCatch(makemaps_failedAccessibleArea(binomial = spp_without_map_names[i]),
           error = function(e) print(paste(spp_without_map_names[i], "Error in Code Skipping for now!")))
}


for(i in seq_along(spp_without_map_names)){
  tryCatch(makemaps_singletons(binomial = spp_without_map_names[i]),
           error = function(e) print(paste(spp_without_map_names[i], "Error in Code Skipping for now!")))
}
