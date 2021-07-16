

image_dir <- "Outputs/Malesia/"
dir.create("Outputs/Malesia_DotMaps")
move_dir <- "Outputs/Malesia_DotMaps/"

filez <- list.files(image_dir, pattern = ".png", full.names = T, recursive = T)

for(i in seq_along(filez)){
  
  file.copy(from = filez[i],
            to = paste(move_dir))
  
}
