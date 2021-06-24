library(raster)
library(tidyverse)
library(ggplot2)
library(rnaturalearth)

tifs <- list.files(path = "Outputs/NorthAmericaEndemics_2021_06_22/",
                   pattern = "PA.tif", recursive = T, full.names = T)

test_tifs <- tifs[1:25]

na <- ne_countries(continent = "North America", returnclass = 'sp')
r <- raster(test_tifs[1])
re <- extend(r, na)
r2 <- raster(test_tifs[2])
r2e <- extend(r2, na)
r22 <- raster(vals=values(r2e),ext=extent(re),crs=crs(re),
              nrows=dim(re)[1],ncols=dim(re)[2])
rs <- stack(re, r22)

all_tifs <- tifs[3:251]

ECM <- function(rasterLayer){
  rasterLayer %>%
    raster::extend( ., extent(rs) ) %>%
    raster::crop(   ., extent(rs) ) %>%
    raster::resample(., rs) 
}

for(i in seq_along(all_tifs)){
  
  rr <- raster(all_tifs[i])
  rr_ecm <- ECM(rr)
    
  rs <- raster::stack(rs, rr_ecm)
}

rich <- sum(rs, na.rm = T)
plot(rich)
