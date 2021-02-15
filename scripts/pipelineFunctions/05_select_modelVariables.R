#' Function to clip model variable layers to accessible area
#' Funciton is nested within a function that chooses what variables to keep 
#' by selecting the variables that lead to a VIF below a user specified value.
#' When choosing between two correlated variables, the model selects variable
#' that contributed the most to the maxent model when all variable were used 
#' in the model

# First we have the function to be nested.

remove_singleVariables <- function(pred_vars, maxent_mod, th){
  #calculate vifs of predictor variables
  pVIF <- usdm::vif(pred_vars)
  #calculate variable importance of maxent models
  vIMP <- ENMeval::var.importance(maxent_mod)
  #join var importance with vifs
  jdf <- left_join(pVIF, vIMP, by = c("Variables" = "variable"))
  # select the variables with the highest VIFs and select least important variable
  # based on permutation importance
  lowVar <- jdf %>% 
    filter(VIF > th) %>% 
    filter(VIF == sort(VIF, decreasing = TRUE)[1] |
             VIF == sort(VIF, decreasing = TRUE)[2]) %>% 
    filter(permutation.importance == min(permutation.importance))
  
  # make raster stack of variable without biggest VIF and lowest permutation imp
  predictors_notInfl <- raster::dropLayer(pred_vars, as.character(lowVar$Variables))
  
  #p2_vifs <- usdm::vif(predictors_notInfl)
  
  return(predictors_notInfl)
}

#' Function to automatically select SDM variables

select_sdmVariables <- function(pred_vars, maxent_mod, maxVIF){
  vv <- pred_vars
  
  while(max(usdm::vif(vv)$VIF) >= maxVIF){
    vv <- remove_singleVariables(pred_vars = vv,
                              maxent_mod = maxent_mod,
                              th = maxVIF)
  }
  
  return(vv)
}
