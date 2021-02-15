library(sp)
library(rangeBuilder)
library(raster)
library(alphahull)
library(sf)
library(tidyverse)

# Read in past spp specific workflows


#' Function to determine accessible area for SDMs. This is done using an alpha
#' hull approach, where the buffer to the alpha hull is either the 80th 
#' percentile distance between points or a user defined distance in meters

define_accessibleArea <- function(species_df, minBuff = 75000, 
                                 imagedir = "NotSaving", 
                                 shapefiledir = "NotSaving",
                                 saveImage = FALSE, 
                                 saveShapefile = FALSE){

  temp <- species_df
  
  #Calculate buffer in meters
  coordinates(temp) <- ~ decimalLongitude + decimalLatitude
  proj4string(temp) <- CRS("+proj=longlat +datum=WGS84")
  tempTrans <- spTransform(temp, "+proj=cea +lat_ts=0 +lon_0")
  
  # Calculate 80th percentile distance between points
  buffDist <- quantile (x = (apply(spDists(tempTrans), 2, 
                                   FUN = function(x) sort(x)[2])), probs = 0.80, 
                        na.rm = TRUE) 
  # Buffer distance becomes either 75000 or the calculated percentile distance made above
  buffDist <- max(buffDist, 75000)
  
  #Create alpha hull and turn it into a polygon
  shape <- getDynamicAlphaHull(x = temp@coords, fraction = 1, partCount = 1, initialAlpha = 20, 
                               clipToCoast = "terrestrial", proj = "+proj=longlat +datum=WGS84")
  shapeTrans <- spTransform(shape[[1]], "+proj=cea +lat_ts=0 +lon_0=0")
  shape2 <- buffer(x = shapeTrans, width = (buffDist+25000), dissolve = T)
  shape2_sf <- st_as_sf(shape2)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  tempTrans_sf <- st_as_sf(tempTrans)
                   
  plotz <- ggplot() + 
    geom_sf(world, mapping = aes(), fill = NA) +
    geom_sf(shape2_sf, mapping = aes(), fill = "Orange") +
    geom_sf(tempTrans_sf, mapping = aes(), color = "black") +
    coord_sf(xlim = c(min(temp@coords[,1]) - 10, max(temp@coords[,1]) + 10),
             ylim = c(min(temp@coords[,2]) - 10, max(temp@coords[,2]) + 10)) +
    theme_classic()
  
  if(saveImage == TRUE){
    ggsave(filename = paste(imagedir, species_df$validName[1], ".png", 
                            sep = ""), plot = plotz)
  } else{
    print("Plot of alpha hull not saved. If image is wanted please, add 
          saveImage = TRUE to function")
  }

  if(saveShapefile == TRUE){
  #Write the shapefile
  shapefile(spTransform(shape2, "+proj=longlat +datum=WGS84"), 
            filename = paste(shapefiledir, species_df$validName[1], "_AA", ".shp",
                             sep = ""), overwrite = T)
  } else{
    print("Shapefile of accessible area not saved. If accessible area should be saved,
          please add saveShapefile = TRUE to function")
  }

  return(shape2)
  
}

# test
# s <- define_accessibleArea(species_df = coord_clean(species = "Astragalus uliginosus", 
#                                               imagedir = "AAFigs/",
#                                               occdir = "AccessibleAreas/", 
#                                               saveimage = FALSE,
#                                               saveocc = FALSE), 
#                     minBuff = 7500, imagedir = "AAFigs/",
#                     shapefiledir = "AccessibleAreas/", saveImage = TRUE, 
#                     saveShapefile = TRUE)