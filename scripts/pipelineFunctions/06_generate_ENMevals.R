

library(dismo)
library(ENMeval)

# Function to run SDMS and choose the model parameters that make the best model 
# selected by AIC as long as AUC > 0.7. If AUC is <0.7, then select top AUC?

generate_ENMeval <- function(occ_df, model_vars, 
                             partitionMethod, 
                             reg_mult,
                             fcs){
  
  spp_df <- occ_df
  
  coordinates(spp_df) <- ~ decimalLongitude + decimalLatitude
  
  max_model <- maxent(x = model_vars, p = coordinates(spp_df), 
                     progress = "text") 
  
  predictors <- select_sdmVariables(pred_vars = model_vars, 
                                    maxent_mod = max_model, 
                                    maxVIF = 5)
    
  eval1 <- ENMeval::ENMevaluate(occ = coordinates(spp_df), 
                                env = predictors,
                                method = partitionMethod, 
                                RMvalues = reg_mult, 
                                fc = fcs,
                                #kfolds = 10,
                                parallel = TRUE,
                                algorithm = 'maxent.jar')
  return(eval1)
}


##Examples
#ab <- coord_clean(species = "Astragalus bourgovii",
#                  saveimage = FALSE,
#                  saveocc = FALSE)
#
#ab_aa_shp <- define_accessibleArea(species_df = ab, minBuff = 75000,
#                                   saveImage = FALSE, shapefiledir = FALSE)
#
#
#clip_variableLayers(layerDir = "ModelVariables/testvars/", 
#                    accessibleArea = ab_aa_shp,
#                    spp = "Astragalus bourgovii")
#
#
#raslist <- list.files(path = tempdir(), pattern = "Astragalus bourgovii*",
#                      all.files = TRUE, full.names = TRUE)
#
#mod_vars <- stack(raslist)
#
### model eval
#
#eval1 <- generate_ENMeval(occ_df = ab, model_vars = mod_vars,
#                          partitionMethod = "randomkfold",
#                          reg_mult = seq(0.5, 3, 0.5),
#                          fcs = c("L", "Q", "LQ"))