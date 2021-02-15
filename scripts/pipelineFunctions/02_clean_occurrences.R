library(dplyr)
library(ggplot2)
library(CoordinateCleaner)
library(rnaturalearth)
library(sf)
library(stringr)


#' Function to clean coordinates from occurrence records

coord_clean <- function(species, df,
                        imagedir = "NotNeeded", 
                        occdir = "NotNeeded", 
                        saveimage = FALSE, 
                        saveocc = FALSE){
  
  cs <- filter(df, scientific_name == species)
  
  cs1 <- cs %>% 
    filter(!is.na(decimalLatitude)) %>% 
    filter(!is.na(decimalLongitude)) %>% 
    mutate(decimalLatitude = as.numeric(decimalLatitude),
           decimalLongitude = as.numeric(decimalLongitude)) %>% 
    cc_val(lon = "decimalLongitude", lat = "decimalLatitude", value = "clean")
  
  cs2 <- cs1 %>% 
    
    cc_equ(lon = "decimalLongitude", lat = "decimalLatitude",
           value = "clean") %>% 
    
    cc_cen(lon = "decimalLongitude", lat = "decimalLatitude",
           species = "validname", buffer = 500, value = "clean") %>% 
    
    cc_dupl(lon = "decimalLongitude", lat = "decimalLatitude",
            value = "clean", species = "validName") %>% 
    
    cc_gbif(lon = "decimalLongitude", lat = "decimalLatitude",
            value = "clean", species = "validName", buffer = 500) %>% 
    
    cc_inst(lon = "decimalLongitude", lat = "decimalLatitude",
            value = "clean", species = "validName", buffer = 100) %>% 
    
    
    cc_zero(lon = "decimalLongitude", lat = "decimalLatitude",
            buffer = 0.05, value = "clean") %>% 
    
    cc_outl(lon = "decimalLongitude", lat = "decimalLatitude",
            value = "clean", species = "validName", method = "distance",
            tdi = 1000) 
  cs2 <- 
    tryCatch(cc_sea(x = cs2, lon = "decimalLongitude", lat = "decimalLatitude", 
                    value = "clean"),
             error = function(e) 
               cc_sea(x = cs2, lon = "decimalLongitude", lat = "decimalLatitude", 
                      value = "clean", scale = 50))
  
  if(IQR(cs2$decimalLatitude) < 0.5 | IQR(cs2$decimalLongitude) < 0.5){
    cs3 <- cc_outl(x = cs2, lon = "decimalLongitude", lat = "decimalLatitude",
                   value = "clean", species = "validName", method = "quantile",
                   mltpl = 50)} else if(IQR(cs2$decimalLatitude) >= 0.5 &  IQR(cs2$decimalLatitude) < 1 |
                                        IQR(cs2$decimalLongitude) >= 0.5 & IQR(cs2$decimalLongitude) < 1){
                     cs3 <- cc_outl(x = cs2, lon = "decimalLongitude", lat = "decimalLatitude",
                                    value = "clean", species = "validName", method = "quantile",
                                    mltpl = 25)} else{
                                      cs3 <-  cc_outl(x = cs2, lon = "decimalLongitude", lat = "decimalLatitude",
                                                      value = "clean", species = "validName", method = "quantile",
                                                      mltpl = 10)}
  
  if(saveocc == TRUE){
    if(nrow(cs3) > 10){
      write.csv(x = cs3, file = paste(occdir, species, ".csv", sep = ""), 
                row.names = FALSE)
    } else{
      print(paste(species, "does not have enough data points, no csv produced"))
    } } else{
      print("Decision made to not save occurrence points to directory")
    }
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  us <- ne_countries(continent = "North America", returnclass = "sf")
  
  a <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cs1, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple") + 
    coord_sf(xlim = c(min(cs1$decimalLongitude) - 10, max(cs1$decimalLongitude) + 10),
             ylim = c(min(cs1$decimalLatitude) - 10, max(cs1$decimalLatitude) + 10)) +
    ggtitle("Uncleaned") + 
    theme_bw()
  
  b <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_point(cs3, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               color = "purple")  +
    coord_sf(xlim = c(min(cs1$decimalLongitude) - 10, max(cs1$decimalLongitude) + 10),
             ylim = c(min(cs1$decimalLatitude) - 10, max(cs1$decimalLatitude) + 10)) +
    ggtitle("Cleaned") +
    theme_bw()
  
  cp <- cowplot::plot_grid(a, b)
  
  
  if(saveimage == TRUE){  
    if(nrow(cs3) > 10){
      ggsave(filename = paste(imagedir, species, "_occs", ".png", sep = ""),
             plot = cp)
    } else{
      print(paste(species, "does not have enough data points, no image produced"))
    } } else {
      print("Decision made not to save image of occurrence points to image directory")
    }
  
  return(cs3)
}
