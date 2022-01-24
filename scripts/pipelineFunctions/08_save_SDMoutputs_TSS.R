library(terra)

#' Function to save SDM model results of interest

save_SDM_results <- function(ENMeval_output, AUCmin, resultDir, 
                             spp, occ_df){
  
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
    
    r_best <- terra::rast(r_best)
    back_pts <- spatSample(r_best, size = nrow(occ_df)*3, xy = T) %>% 
      na.omit() %>% 
      slice_sample(n = nrow(occ_df)) %>% 
      dplyr::select(x, y)
    spp_df <- occ_df %>% 
      dplyr::rename(x = decimalLongitude, y = decimalLatitude) %>% 
      dplyr::select(x, y)
    lpt <- terra::extract(x = r_best, y = spp_df) %>% 
      dplyr::rename(cloglog = 2)
    
    # lpts
    p <- list(0, 0.01, 0.025, 0.05, 0.1)
    
    calculate_tss <- function(p){
      
      lpt_10 <- quantile(lpt$cloglog, probs = p, na.rm = TRUE)
      pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
      pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
      r_best_pa <- classify(r_best, pa_mat)
      
      #'• Sensitivity (Se) – percentage of actual presences predicted
      #'• Specificity (Sp) – percentage of actual absences predicted
      #'• s is proportion of true presences among the pseudo-absences 
      
      pres <- terra::extract(x = r_best_pa, y = spp_df) %>% 
        rename(pa = 2)%>% 
        na.omit()
      Se <- sum(pres$pa) / nrow(pres)
      
      abs <- terra::extract(x = r_best_pa, y = back_pts) %>% 
        rename(pa = 2)%>% 
        na.omit()
      Sp <- sum(1-abs$pa) / nrow(abs)
      
      # half weights specificity since we don't have true absences
      # could quarter or full weight as one sees fit
      TSS <- (Se + (0.5*Sp)) - 1
    }
    
    
    t <- lapply(p, calculate_tss)
    t <- unlist(t)
    tss_val <- data.frame(tss = t,
                          vals = c(0,0.01,0.025,0.05,0.1)) 
    lptVal <- dplyr::filter(tss_val, tss == max(tss))$vals
    
    
    lpt_10 <- quantile(lpt, probs = lptVal, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- classify(r_best, pa_mat)
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
    r_best <- terra::rast(r_best)
    back_pts <- spatSample(r_best, size = nrow(occ_df)*3, xy = T) %>% 
      na.omit() %>% 
      slice_sample(n = nrow(occ_df)) %>% 
      dplyr::select(x, y)
    spp_df <- occ_df %>% 
      dplyr::rename(x = decimalLongitude, y = decimalLatitude) %>% 
      dplyr::select(x, y)
    lpt <- terra::extract(x = r_best, y = spp_df) %>% 
      dplyr::rename(cloglog = 2)
    
    # lpts
    p <- list(0, 0.01, 0.025, 0.05, 0.1)
    
    calculate_tss <- function(p){
      
      lpt_10 <- quantile(lpt$cloglog, probs = p, na.rm = TRUE)
      pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
      pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
      r_best_pa <- classify(r_best, pa_mat)
      
      #'• Sensitivity (Se) – percentage of actual presences predicted
      #'• Specificity (Sp) – percentage of actual absences predicted
      #'• s is proportion of true presences among the pseudo-absences 
      
      pres <- terra::extract(x = r_best_pa, y = spp_df) %>% 
        rename(pa = 2)%>% 
        na.omit()
      Se <- sum(pres$pa) / nrow(pres)
      
      abs <- terra::extract(x = r_best_pa, y = back_pts) %>% 
        rename(pa = 2)%>% 
        na.omit()
      Sp <- sum(1-abs$pa) / nrow(abs)
      
      # half weights specificity since we don't have true absences
      # could quarter or full weight as one sees fit
      TSS <- (Se + (0.5*Sp)) - 1
    }
    
    
    t <- lapply(p, calculate_tss)
    t <- unlist(t)
    tss_val <- data.frame(tss = t,
                          vals = c(0,0.01,0.025,0.05,0.1)) 
    lptVal <- dplyr::filter(tss_val, tss == max(tss))$vals
    
    
    lpt_10 <- quantile(lpt, probs = lptVal, na.rm = TRUE)
    pa <- c(0, lpt_10, 0, lpt_10, 1, 1)
    pa_mat <- matrix(pa, ncol = 3, byrow = TRUE)
    r_best_pa <- classify(r_best, pa_mat)
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
