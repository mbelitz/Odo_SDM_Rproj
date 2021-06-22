library(tidyverse)

## read in odos data
nco <- read_csv("Occ_Data/NA_OC_All_Records_2020-10-16.csv")

nco <- nco %>% 
  #dplyr::select(scientific_name, decimalLongitude, decimalLatitude, status) %>% 
  mutate(validName = scientific_name) %>% 
  filter(status == "Accepted")

world <- rnaturalearth::ne_countries(returnclass = "sf")

## read in species that did not make occurrence validation images or csvs correctly

#Cordulegaster bidentata
cb <- filter(nco, validName == "Cordulegaster bidentata")

ga <- filter(nco, validName == "Gomphaeschna antilope")

lj <- filter(nco, validName == "Ladona julia")

la <- filter(nco, validName == "Libellula axilena")

ma <- filter(nco, validName == "Macromia margarita")

ni <- filter(nco, validName == "Nehalennia irene")
write.csv(ni, file = "Outputs/CleanedOccs/AllNorthAmericaSpp/Nehalennia irene/occurrenceRecords.csv")

source("scripts/pipelineFunctions/03-5_define_accessibleArea_alphaHullFail.R")
makemaps(binomial = "Ladona julia")
makemaps(binomial = "Macromia margarita")
makemaps_failedAccessibleArea(binomial = "Somatochlora sahlbergi")
makemaps_failedAccessibleArea(binomial = "Sympetrum costiferum")

