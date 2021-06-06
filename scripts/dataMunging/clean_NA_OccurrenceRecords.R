library(rnaturalearth)

source("scripts/pipelineFunctions/02_clean_occurrences.R")
source("scripts/pipelineFunctions/03_define_accessibleArea.R")

nco <- read_csv("Occ_Data/NA_OC_All_Records_2020-10-16.csv")

nco <- nco %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude, status) %>% 
  mutate(validName = scientific_name) %>% 
  filter(status == "Accepted")

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
  
  aa_shp <- define_accessibleArea(species_df = cc, minBuff = 100000,
                                  saveImage = FALSE,
                                  saveShapefile = FALSE)
  
  p3 <- ggplot() + 
    geom_sf(world, mapping = aes(), fill = NA) +
    geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "Orange") +
    geom_point(cc, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "black")  +
    coord_sf(xlim = c(min(cc$decimalLongitude) - 10, max(cc$decimalLongitude) + 10),
             ylim = c(min(cc$decimalLatitude) - 10, max(cc$decimalLatitude) + 10)) +
    ggtitle(binomial) +
    theme_classic()
  
  cp <- cowplot::plot_grid(p1, p2, p3, rel_heights = c(1,1,3), rel_widths = c(1,1,3))
  
  dir.create(paste("Outputs/CleanedOccs/AllNorthAmericaSpp/", binomial, sep = ""))
  
  ggsave(filename = paste("Outputs/CleanedOccs/AllNorthAmericaSpp/", 
                          binomial, "/CoordClean.png", 
                          sep = ""),
        plot = cp)
  
  write.csv(x = cc, file = paste("Outputs/CleanedOccs/AllNorthAmericaSpp/", 
                                      binomial, "/occurrenceRecords.csv", 
                                      sep = ""),
            row.names = F)
  
}


## read in species list
library(googlesheets4)

taxo <- read_sheet("https://docs.google.com/spreadsheets/d/1nzNGZtYoNzpv_i9_yKdBVnHia1fuM9a1vtwM7hvcXN8/edit?ts=60a58f25#gid=345138096")

taxo <- taxo %>% 
  mutate(validName = stringr::word(`Full name`, 1 , 2))

taxo2 <- taxo %>% 
  replace_na(list(`North America` = "Not in North America"))

taxo2$`North America` <- unlist(taxo2$`North America`)

spp_list <- filter(taxo2, `North America` == "1")$validName

lapply(spp_list, makemaps)

for(i in seq_along(spp_list)){
  tryCatch(makemaps(binomial = spp_list[i]),
           error = function(e) print(paste(spp_list[i], "Error in Code Skipping for now!")))
}
