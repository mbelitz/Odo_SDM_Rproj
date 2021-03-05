library(rnaturalearth)

source("scripts/pipelineFunctions/02_clean_occurrences.R")
source("scripts/pipelineFunctions/03_define_accessibleArea.R")

dir.create("CoordinateCleaningResults")
dir.create("CoordinateCleaningResults/NA")

nonCleanedOccs <- data.table::fread("Occ_Data/OC_All_Records_2020-10-16.csv", 
                                    select = c("scientific_name", "decimalLongitude", "decimalLatitude")) %>% 
  mutate(validName = scientific_name)

world <- rnaturalearth::ne_countries(returnclass = "sf")

makemaps <- function(binomial){
  
  spdf <- filter(nonCleanedOccs, scientific_name == binomial)
  
  cc <- coord_clean(species = binomial, df = nonCleanedOccs)
  
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
  
  p3 <- ggplot() + 
    geom_sf(world, mapping = aes(), fill = NA) +
    geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "Orange") +
    geom_point(spdf, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "black")  +
    coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
             ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
    ggtitle(binomial) +
    theme_classic()
  
  cp <- cowplot::plot_grid(p1, p2, p3, rel_heights = c(1,1,3), rel_widths = c(1,1,3))
  
  return(cp)
  
}

test <- makemaps("Aeshna septentrionalis")
test

spp_list <- read.csv("NA_SDM_testSpp.csv") %>% 
  dplyr::rename(scientific_name = 1) %>% 
  dplyr::filter(!is.na(scientific_name))

# Error at 16. Why?

for(i in 16:length(spp_list$scientific_name)){
  
  a <- makemaps(binomial = spp_list$scientific_name[i])
  
  ggsave(filename = paste("CoordinateCleaningResults/NA/", 
                          spp_list$scientific_name[i], ".png", sep = ""),
         plot = a)
         
}

