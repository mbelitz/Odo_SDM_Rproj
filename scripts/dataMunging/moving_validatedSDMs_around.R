library(dplyr)
library(googlesheets4)

## sdm validation google sheet
sdm_valid <- read_sheet("https://docs.google.com/spreadsheets/d/1nT3Y1uJcSYnwwuJiiLl9vVELiT632_DQDqlLok2Gplo/edit#gid=0",
                        sheet = "Nearctic")


#get species that passed
passed <- dplyr::filter(sdm_valid,
                        PassVsFail == "Pass" | 
                        PassVsFail == "pass?" | 
                        PassVsFail == "pass")

t <- read.csv("Occ_Data/Copyofnearctic_occurrenceRecords_12-15-21.csv")
t <- filter(t, scientific_name == "Hetaerina americana")
t2 <- t %>% dplyr::filter(is.na(Note.for.Mike) | Note.for.Mike != "Remove")
