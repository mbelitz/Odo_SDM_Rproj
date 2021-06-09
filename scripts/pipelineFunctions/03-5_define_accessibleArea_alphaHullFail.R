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

define_FailedaccessibleArea <- function(species_df, minBuff = 75000, 
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
  occ_convex_hull <- chull(temp$decimalLongitude, temp$decimalLatitude)
  ## generate the end points of polygon. 
  CoordsPoly = temp[c(occ_convex_hull, occ_convex_hull[1]), ] %>% 
    as.data.frame() %>% 
    dplyr::select(decimalLongitude, decimalLatitude)# closed polygon
  
  ## convert this polygon coordinate matix to SpatialPolygon, so that buffering cound be done. 
  SpPoly = SpatialPolygons(list(Polygons(list(Polygon(CoordsPoly)), ID=1)))
  crs(SpPoly) <- "+proj=longlat +datum=WGS84"
  SpPolyTrans <- spTransform(SpPoly, "+proj=cea +lat_ts=0 +lon_0=0")
  
  shape2 <- buffer(x = SpPolyTrans, width = (buffDist+25000), dissolve = T)
  shape2_sf <- st_as_sf(shape2)
  
  test <- st_as_sf(SpPolyTrans)
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  tempTrans_sf <- st_as_sf(tempTrans)
  
  plotz <- ggplot() + 
    geom_sf(world, mapping = aes(), fill = NA) +
    geom_sf(shape2_sf, mapping = aes(), fill = "Orange") +
    geom_sf(tempTrans_sf, mapping = aes(), color = "black") +
    coord_sf(xlim = c(min(temp$decimalLongitude) - 10, max(temp$decimalLongitude) + 10),
             ylim = c(min(temp$decimalLatitude) - 10, max(temp$decimalLatitude) + 10)) +
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