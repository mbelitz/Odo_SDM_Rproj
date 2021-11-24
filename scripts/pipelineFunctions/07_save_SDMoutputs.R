#' Function to save SDM model results of interest

save_SDM_results <- function(ENMeval_output, AUCmin, resultDir, 
                             spp, occ_df, model_variables){
  
  spp <- stringr::str_replace(string = spp, pattern = " ", replacement = "_")
  
  bestmod <- ENMeval_output@results %>% 
    dplyr::filter(delta.AICc == 0) 
  
  if(bestmod$auc.train >= AUCmin & bestmod$auc.val.avg >= AUCmin){
    
    bestmod <- bestmod[1,]
    write.csv(bestmod, 
              file = paste(resultDir, spp, "_bestModel", ".csv", sep = ""),
              row.names = F)
    
    # build the best model
    maxent_args <- as.character(bestmod$tune.args)
    r_best <- ENMeval_output@predictions[[maxent_args]]
    writeRaster(x = r_best, 
                filename = paste(resultDir, spp, "_SDM", ".tif", sep = ""), overwrite = TRUE)
    
    #  ggsave(dismo::response(max_best), 
    #        filename = paste(resultDir, spp, "_responseCurves", ".png", sep = ""))
    
    spp_df <- occ_df
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    lpt <- raster::extract(x = r_best, y = spp_df, na.rm = TRUE)
    lpt_10 <- quantile(lpt, probs = 0.05, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- reclassify(r_best, pa_mat)
    writeRaster(x = r_best_pa, 
                filename = paste(resultDir, spp, "_SDM_PA", ".tif", sep = ""), overwrite = TRUE)
    
    varimp <- ENMeval_output@variable.importance[[maxent_args]]
    write.csv(x = varimp,               
              file = paste(resultDir, spp, "_variableImportance", ".csv", sep = ""),
              row.names = F)
    
  } else{
    bestmod <- ENMeval_output@results %>% 
      dplyr::filter(auc.train == max(auc.train) | auc.val.avg  == max(auc.val.avg)) 
    bestmod <- bestmod[1,]
    write.csv(bestmod, 
              file = paste(resultDir, spp, "_bestModel", ".csv", sep = ""),
              row.names = F)
    
    # build the best model
    # build the best model
    maxent_args <- as.character(bestmod$tune.args)
    r_best <- ENMeval_output@predictions[[maxent_args]]
    writeRaster(x = r_best, 
                filename = paste(resultDir, spp, "_SDM", ".tif", sep = ""), overwrite = TRUE)
    
    # ggsave(dismo::response(max_best), 
    #       filename = paste(resultDir, spp, "_responseCurves", ".png", sep = ""))
    spp_df <- occ_df
    coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
    lpt <- raster::extract(x = r_best, y = spp_df, na.rm = TRUE)
    lpt_10 <- quantile(lpt, probs = 0.1, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- reclassify(r_best, pa_mat)
    writeRaster(x = r_best_pa, 
                filename = paste(resultDir, spp, "_SDM_PA", ".tif", sep = ""), overwrite = TRUE)
    
    varimp <- ENMeval_output@variable.importance[[maxent_args]]
    write.csv(x = varimp,               
              file = paste(resultDir, spp, "_variableImportance", ".csv", sep = ""),
              row.names = F)
    
  }
  
}
# example
#save_SDM_results(ENMeval_output = ENMeval_output, 
#                 AUCmin = 0.7,
#                 resultDir = "ModelResults/",spp = "Astragalus bourgovii",
#                 occ_df = ab)
