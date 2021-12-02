library(rnaturalearth)

setwd("/srv/mybook/mbelitz/Odo_SDMs/")

options(java.parameters = "-Xmx22000m")
unixtools::set.tempdir("tmp/")
tmp_dir <- unixtools::set.tempdir("tmp/")
rasterOptions(tmpdir="tmp")
files <- list.files(tmp_dir, full.names = T,  all.files = T, recursive = T)
file.remove(files)
gc()


library("somePackage", lib.loc=.libPaths()[-1])

.libPaths("/srv/mybook/mbelitz/R/x86_64-pc-linux-gnu-library/4.0/")
library(dismo, lib.loc = "/srv/mybook/mbelitz/R/x86_64-pc-linux-gnu-library/4.0/")
library(ENMeval)
library(unixtools)
library(ggplot2)

source("pipelineFunctions/03_define_accessibleArea.R")
source("pipelineFunctions/04_clip_modelLayers.R")
source("pipelineFunctions/05_select_modelVariables.R")
source("pipelineFunctions/08_save_SDMoutputs_TSS.R")

## read in data
odo_central <- data.table::fread("Occ_Data/odonata_central_20210615180556.csv") %>% 
  dplyr::filter(Status == "Accepted")
odo_central <- odo_central %>% 
  dplyr::rename("scientific_name" = "Species Scientific Name",
                "decimalLongitude" = "Longitude",
                "decimalLatitude" = "Latitude") %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude) %>% 
  dplyr::mutate(validName = stringr::str_replace(string = scientific_name, pattern = "_", replacement = " ")) %>% 
  dplyr::mutate(spaces = stringr::str_count(validName, pattern = " ")) %>%  # spaces added to make sure no subspecies are in the validName column
  dplyr::mutate(source = "Odonata Central")

mex_data <- data.table::fread("Occ_Data/Mexican_NearcticOdonatesformaps.csv")
mex_data <- mex_data %>% 
  dplyr::rename("scientific_name" = "sp",
                "decimalLongitude" =  "Longitud", 
                "decimalLatitude" = "Latitud") %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude) %>% 
  mutate(validName = stringr::str_replace(string = scientific_name, pattern = "_", replacement = " ")) %>% 
  mutate(spaces = str_count(validName, pattern = " ")) %>%  # spaces added to make sure no subspecies are in the validName column
  mutate(source = "Mexican DB")

palea <- data.table::fread("Occ_Data/AAADBMAPSPalaearctic20Oct21.txt")
palea <- palea %>%   
  dplyr::rename(scientific_name = "Species name GEODE",
                decimalLongitude = "Longitude",
                decimalLatitude = "Latitude",
                source = "Source") %>% 
  mutate(validName = stringr::str_replace(string = scientific_name, pattern = "_", replacement = " ")) %>% 
  mutate(spaces = str_count(validName, pattern = " ")) %>% 
  dplyr::select(scientific_name, decimalLongitude, decimalLatitude, 
                validName, spaces, source)

occs <- rbind(mex_data, odo_central, palea)

## also read in basemap
world <- rnaturalearth::ne_countries(returnclass = "sf", scale = 10)


run_spp_pipeline <- function(binomial){
  
  cleanedOccs <- occs %>% 
    dplyr::filter(validName == binomial)
  
  aa_shp <- define_accessibleArea(species_df = cleanedOccs, minBuff = 75000,
                                  saveImage = F, 
                                  saveShapefile = FALSE)
  
  mod_vars <- clip_variableLayers(layerDir = "ClimateVariables/ClimateOnly/", 
                                  accessibleArea = aa_shp)
  
  mod_vars <- aggregate(mod_vars, 5) #use this step to upaggregate resolution
  
  spp_df <- cleanedOccs
  
  coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
  
  ## spatial thinning
  area_sqkm <- raster::area(aa_shp)/1000000
  
  # my guess for now is 25km thinning for > 100,000km2, no thinning below that,
  # 50 km thinning for > 100,000km2 but < 1,000,000km2 and 
  # 100 km thinning for > 1,000,000km2
  if(area_sqkm < 100000){
    
    spp_df <- spp_df
    
  } else if(area_sqkm >= 100000 & area_sqkm < 250000){
    
    bio12 <- mod_vars[[2]]
    bio12 <- aggregate(bio12, 5) 
    bio12mdf <- raster::as.data.frame(bio12, xy = T)
    bio12mdf_noNA <- na.omit(bio12mdf) %>% 
      dplyr::rename(z = 3) 
    bio12mdf_noNA <- mutate(bio12mdf_noNA, z = 1:nrow(bio12mdf_noNA))
    bio12_2 <- rasterFromXYZ(bio12mdf_noNA)
    
    e <- raster::extract(bio12_2, spp_df)
    
    spp_df$cell_id <- e
    spp_df_thinned <- st_as_sf(spp_df) %>% 
      dplyr::distinct(cell_id, .keep_all = T) 
    spp_df <- as_Spatial(spp_df_thinned)
    
  } else if(area_sqkm >= 250000 & area_sqkm < 1000000){
    
    bio12 <- mod_vars[[2]]
    bio12 <- aggregate(bio12, 10) 
    bio12mdf <- raster::as.data.frame(bio12, xy = T)
    bio12mdf_noNA <- na.omit(bio12mdf) %>% 
      dplyr::rename(z = 3) 
    bio12mdf_noNA <- mutate(bio12mdf_noNA, z = 1:nrow(bio12mdf_noNA))
    bio12_2 <- rasterFromXYZ(bio12mdf_noNA)
    
    e <- raster::extract(bio12_2, spp_df)
    
    spp_df$cell_id <- e
    spp_df_thinned <- st_as_sf(spp_df) %>% 
      dplyr::distinct(cell_id, .keep_all = T) 
    spp_df <- as_Spatial(spp_df_thinned)
    
  } else{
    
    bio12 <- mod_vars[[2]]
    bio12 <- aggregate(bio12, 20) 
    bio12mdf <- raster::as.data.frame(bio12, xy = T)
    bio12mdf_noNA <- na.omit(bio12mdf) %>% 
      dplyr::rename(z = 3) 
    bio12mdf_noNA <- mutate(bio12mdf_noNA, z = 1:nrow(bio12mdf_noNA))
    bio12_2 <- rasterFromXYZ(bio12mdf_noNA)
    
    e <- raster::extract(bio12_2, spp_df)
    
    spp_df$cell_id <- e
    spp_df_thinned <- st_as_sf(spp_df) %>% 
      dplyr::distinct(cell_id, .keep_all = T) 
    spp_df <- as_Spatial(spp_df_thinned)
    
  }
  
  
  max_model <- maxent(x = mod_vars, p = coordinates(spp_df), 
                      progress = "text") 
  
  predictors <- select_sdmVariables(pred_vars = mod_vars, 
                                    maxent_mod = max_model, 
                                    maxVIF = 5)
  
  eval1 <- ENMeval::ENMevaluate(occ = coordinates(spp_df), 
                                env = predictors,
                                method = "block", 
                                RMvalues = c(0.5, 1, 2, 3, 4),
                                fc= c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                                parallel = TRUE, numCores = 15,
                                algorithm = 'maxent.jar')
  
  bw <- stringr::str_replace(string = binomial, pattern = " ", replacement = "_")
  fp <- file.path("Outputs/Palaearctic_sdmOutputs/")
  fp2 <- file.path("Outputs/Palaearctic_sdmMaps/")
  dir.create(paste(fp, bw, sep = ""))
  
  spp_df2 <- as.data.frame(spp_df) %>% 
    dplyr::rename(decimalLongitude = coords.x1,
                  decimalLatitude = coords.x2)
  
  save_SDM_results(ENMeval_output = eval1, 
                   AUCmin = 0.7,
                   resultDir = paste(fp, bw, "/", sep = ""),
                   spp = binomial,
                   occ_df = spp_df2)
  
  save(eval1,
       file = paste(fp, bw, "/", 
                    bw, "_ENMeval", ".RData", sep = ""))
  
  p <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_sf(st_as_sf(aa_shp), mapping = aes(), fill = "orange", alpha = 0.5) +
    geom_point(cleanedOccs, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               shape = 1, size = 0.75) +
    coord_sf(xlim = c(min(cleanedOccs$decimalLongitude) - 5, max(cleanedOccs$decimalLongitude) + 5),
             ylim = c(min(cleanedOccs$decimalLatitude) - 5, max(cleanedOccs$decimalLatitude) + 5)) +
    ggtitle(paste(binomial, "n =", length(cleanedOccs$validName), sep = " ")) +
    theme_classic()
  
  r <- raster(paste(fp,bw,"/",bw,"_SDM.tif", sep = ""))
  p2 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_tile(as.data.frame(r,xy = T) %>% na.omit() %>% rename(ClogLog = 3),
              mapping = aes(x = x, y = y, fill = ClogLog)) + 
    coord_sf(xlim = c(min(cleanedOccs$decimalLongitude) - 5, max(cleanedOccs$decimalLongitude) + 5),
             ylim = c(min(cleanedOccs$decimalLatitude) - 5, max(cleanedOccs$decimalLatitude) + 5)) +
    scale_fill_viridis_c() +
    theme_classic()
  
  r2 <- raster(paste(fp,bw,"/",bw,"_SDM_PA.tif", sep = "")) 
  p3 <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_tile(as.data.frame(r2,xy = T) %>% na.omit() %>% rename(ClogLog = 3),
              mapping = aes(x = x, y = y, fill = as.character(ClogLog))) + 
    coord_sf(xlim = c(min(cleanedOccs$decimalLongitude) - 5, max(cleanedOccs$decimalLongitude) + 5),
             ylim = c(min(cleanedOccs$decimalLatitude) - 5, max(cleanedOccs$decimalLatitude) + 5)) +
    labs(fill = "Presence") +
    scale_fill_viridis_d() +
    theme_classic()
  
  e <- egg::ggarrange(p2, p3, p, nrow = 2)
  
  ggsave(plot = e, 
         filename = paste(fp2, bw, "_sdmMap.png", sep = ""),
         width = 8,
         height = 6)
  
  
  bp <- ggplot() +
    geom_sf(world, mapping = aes()) +
    geom_tile(as.data.frame(r2,xy = T) %>% na.omit() %>% rename(ClogLog = 3),
              mapping = aes(x = x, y = y, fill = as.character(ClogLog)),
              alpha = 0.8) + 
    geom_point(cleanedOccs, mapping = aes(x = decimalLongitude, y = decimalLatitude),
               size = 0.5, alpha = 0.75, shape = 1) +
    coord_sf(xlim = c(min(cleanedOccs$decimalLongitude) - 10, max(cleanedOccs$decimalLongitude) + 10),
             ylim = c(min(cleanedOccs$decimalLatitude) - 10, max(cleanedOccs$decimalLatitude) + 10)) +
    labs(fill = "Presence") +
    scale_fill_viridis_d() +
    ggtitle(paste(binomial, "n =", length(cleanedOccs$validName), sep = " ")) +
    theme_classic()
  
  ggsave(plot = bp, filename = paste(fp2, bw, "_sdmMapOccPoints.png", sep = ""))
  
  
  files <- list.files(tmp_dir, full.names = T,  all.files = T, recursive = T)
  file.remove(files)
  gc()
  
}

library(googlesheets4)
#spp list
#spp_list <- read_sheet("https://docs.google.com/spreadsheets/d/1DBtMxYikao5GpR0wDbxeYYhg9mhI9EuU5zm7HMjtEww/edit#gid=722276790",
#                       sheet = "Palaearctic")

#spp_list <- spp_list$`Species name`

#speciesL <- sample(spp_list,size = 30)
#for now speciesL are set
l <- list.files("Outputs/Palaearctic_sdmMaps/", pattern = "*.png")
speciesL <- word(l, start = 1, end = 2, sep = fixed("_")) %>% 
  str_replace("_", " ")

#binomial = speciesL[10]
# looping through pipeline
for(i in seq_along(speciesL)){
  tryCatch(run_spp_pipeline(binomial = speciesL[i]),
           error = function(e) print(paste(speciesL[i], "Error in Code Skipping for now!")))
}