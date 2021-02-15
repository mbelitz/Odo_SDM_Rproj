#' Function to save SDM model results of interest

save_SDM_results <- function(ENMeval_output, AUCmin, resultDir, 
                             spp, occ_df, model_variables){
  
  bestmod <- ENMeval_output@results %>% 
    dplyr::filter(delta.AICc == 0) 
  
  if(bestmod$train.AUC >= AUCmin & bestmod$avg.test.AUC >= AUCmin){
    
    bestmod <- bestmod[1,]
    write.csv(bestmod, 
              file = paste(resultDir, spp, "_bestModel", ".csv", sep = ""),
              row.names = F)
    
    # build the best model
    FC_best <- as.character(bestmod$features)
    rm_best <- bestmod$rm
    maxent_args <- make.args(RMvalues=rm_best, fc=FC_best)
    
    spp_df <- occ_df
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    
    max_best <- maxent(model_variables, spp_df, args = maxent_args[[1]])
    r_best <- predict(max_best, model_variables, overwrite = TRUE, progress = "text")
    writeRaster(x = r_best, 
                filename = paste(resultDir, spp, "_SDM", ".tif", sep = ""), overwrite = TRUE)
    
    ggsave(dismo::response(max_best), 
           filename = paste(resultDir, spp, "_responseCurves", ".png", sep = ""))
    
    lpt <- raster::extract(x = r_best, y = spp_df, na.rm = TRUE)
    lpt_10 <- quantile(lpt, probs = 0.1, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- reclassify(r_best, pa_mat)
    writeRaster(x = r_best_pa, 
                filename = paste(resultDir, spp, "_SDM_PA", ".tif", sep = ""), overwrite = TRUE)
    
    varimp <- var.importance(max_best)
    write.csv(x = varimp,               
              file = paste(resultDir, spp, "_variableImportance", ".csv", sep = ""),
              row.names = F)
    
  } else{
    bestmod <- ENMeval_output@results %>% 
      dplyr::filter(train.AUC == max(train.AUC) | avg.test.AUC == max(avg.test.AUC)) 
    bestmod <- bestmod[1,]
    write.csv(bestmod, 
              file = paste(resultDir, spp, "_bestModel", ".csv", sep = ""),
              row.names = F)
    
    # build the best model
    FC_best <- as.character(bestmod$features)
    rm_best <- bestmod$rm
    maxent_args <- make.args(RMvalues = rm_best, fc = FC_best)
    
    spp_df <- occ_df
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    
    max_best <- maxent(model_variables, spp_df, args = maxent_args[[1]])
    r_best <- predict(max_best, model_variables, overwrite = TRUE, progress = "text")
    writeRaster(x = r_best, 
                filename = paste(resultDir, spp, "_SDM", ".tif", sep = ""), overwrite = TRUE)
    
    ggsave(dismo::response(max_best), 
           filename = paste(resultDir, spp, "_responseCurves", ".png", sep = ""))
    
    lpt <- raster::extract(x = r_best, y = spp_df, na.rm = TRUE)
    lpt_10 <- quantile(lpt, probs = 0.1, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- reclassify(r_best, pa_mat)
    writeRaster(x = r_best_pa, 
                filename = paste(resultDir, spp, "_SDM_PA", ".tif", sep = ""), overwrite = TRUE)
    
    varimp <- var.importance(max_best)
    write.csv(x = varimp,               
              file = paste(resultDir, spp, "_variableImportance", ".csv", sep = ""),
              row.names = F)
    
  }
  
}
# example
#save_SDM_results(ENMeval_output = eval1, 
#                 AUCmin = 0.7,
#                 resultDir = "ModelResults/",spp = "Astragalus bourgovii",
#                 occ_df = ab)
