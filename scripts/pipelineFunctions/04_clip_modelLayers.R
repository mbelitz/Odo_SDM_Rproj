# Read in past spp specific workflows
#' Function to clip model variable layers to accessible area

clip_variableLayers <- function(layerDir, accessibleArea){
  # read in list of files
  filelist <- list.files(layerDir, full.names = TRUE)
  rstack <- stack(filelist)
    
    #crop file to extent of the buffer
    aa_proj <- spTransform(accessibleArea, CRSobj = crs(rstack))
    
    r_crop <- crop(rstack, aa_proj)
    
    # mask the cropped file 
    r_mask <- raster::mask(r_crop, mask = aa_proj)
    
    return(r_mask)
  
}

# Examples
# ab <- coord_clean(species = "Astragalus bourgovii",
#                   saveimage = FALSE,
#                   saveocc = FALSE)
## 
# ab_aa_shp <- define_accessibleArea(species_df = ab, minBuff = 75000,
#                                    saveImage = FALSE, shapefiledir = FALSE)
## 
## 
# rstack <- clip_variableLayers(layerDir = "ModelVariables/allVars/", 
#                     accessibleArea = ab_aa_shp)
## 
## plot(rstack)
