#This function performs the estimation step while we can specify Y,W and X 
#and further if we want to cluster and if we want to perform preselection

#Empty environment
rm(list = ls())

#Set working directory
setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish")

#Load packages
packages = c("dplyr", "DBI", "dbplyr", "httr", "rvest", "data.table", "Hmisc", "grf",
             "ggplot2", "janitor", "reshape","vistime", "dygraphs", "viridis",
             "gridExtra", "magrittr", "ggpubr", "Jmisc", "geodist")
for (pkg in packages){
  eval(bquote(library(.(pkg))))
}

#Run Causal Forest function
func.analysis = function(X, Y, W, cluster, preselection) {
  
  # X: Feature matrix
  # Y: Target vector
  # W: Treatment vector
  # cluster (yes/no) for the cluster-robust or the standard Causal Forest
  # preselection (yes/no) depending if we want to pre-select or not
  
  #If cluster == yes
  if (cluster == "yes") {
    
    #First causal random forest for variable importance
    Y.forest = regression_forest(X, Y, clusters = X$Canton_3) 
    Y.hat    = predict(Y.forest)$predictions
    W.forest = regression_forest(X, W, clusters = X$Canton_3) 
    W.hat    = predict(W.forest)$predictions
    
    cf.raw   = causal_forest(X, 
                             Y, 
                             W,
                             Y.hat = Y.hat, 
                             W.hat = W.hat,
                             clusters = X$Canton_3)
    
    varimp = variable_importance(cf.raw)
    names(varimp) = names(X)
    sorted_varimp = sort(varimp, decreasing = T)
    selected.idx = which(varimp > mean(varimp))
    
    #Second causal random forest if no preselection is selected
    cf = causal_forest(X, 
                       Y, 
                       W, 
                       Y.hat = Y.hat,
                       W.hat = W.hat,
                       cluster = X$Canton_3)
    
    #Preselection                 
    if (preselection == "yes") {
      
      #Second causal random forest if preselection is selected
      cf = causal_forest(X[,selected.idx], 
                         Y, 
                         W, 
                         Y.hat = Y.hat,
                         W.hat = W.hat,
                         cluster = X$Canton_3)
    }
    
    #Heterogeneous treatment effects
    tau.hat = predict(cf, estimate.variance = T)$predictions
    
    #Average treatment effects
    ate = average_treatment_effect(cf)
    ci.ate = paste("95% CI for the ATE:", round(ate[1], 3), 
                   "+/-", round(qnorm(0.975)*ate[2], 3))
    
    ate.overlap = average_treatment_effect(cf, target.sample = "overlap") 
    ci.ate.overlap = paste("95% CI for the ATE:", round(ate.overlap[1], 3),
                           "+/-", round(qnorm(0.975)*ate.overlap[2], 3))
    
    att = average_treatment_effect(cf, target.sample = "treated")
    ci.att = paste("95% CI for the ATE:", round(att[1], 3),
                   "+/-", round(qnorm(0.975)*att[2], 3))
    
    #Test calibration
    test.calibration = test_calibration(cf)
    
    #Test calibration for propensity model (manually)
    DF.prop = data.frame(W = W,
                         e.bar      = mean(cf$W.hat),
                         e.residual = cf$W.hat - mean(cf$W.hat))
    
    
    best.linear.predictor = lm(W ~ e.bar + e.residual + 0, data = DF.prop)
    blp.summary.prop = lmtest::coeftest(best.linear.predictor,
                                   vcov = sandwich::vcovCL,
                                   type = "HC3")
    
    #Convert to one-sided p-values
    dimnames(blp.summary.prop)[[2]][4] = gsub("[|]", "", dimnames(blp.summary.prop)[[2]][4])
    blp.summary.prop[,4] = ifelse(blp.summary.prop[,3] < 0, 1 - blp.summary.prop[,4]/2, blp.summary.prop[,4]/2)
    
    #Test calibration for outcome model
    DF.out = data.frame(Y = Y,
                        m.bar      = mean(cf$Y.hat),
                        m.residual = cf$Y.hat - mean(cf$Y.hat))
    
    best.linear.predictor = lm(Y ~ m.bar + m.residual + 0, data = DF.out)
    blp.summary.out = lmtest::coeftest(best.linear.predictor,
                                    vcov = sandwich::vcovCL,
                                    type = "HC3")
    
    #convert to one-sided p-values
    dimnames(blp.summary.out)[[2]][4] = gsub("[|]", "", dimnames(blp.summary.out)[[2]][4])
    blp.summary.out[,4] = ifelse(blp.summary.out[,3] < 0, 1 - blp.summary.out[,4]/2, blp.summary.out[,4]/2)

  
    #Bias measure
    p = mean(W)
    Y.hat.0 = cf$Y.hat - W.hat * tau.hat
    Y.hat.1 = cf$Y.hat + (1 - W.hat) * tau.hat
    bias = (W.hat - p) * (p * (Y.hat.0 - mean(Y.hat.0))  + (1 - p) * (Y.hat.1 - mean(Y.hat.1)))
    
  } 
  
  #No clustering
  if (cluster == "no") {
    
    #First causal random forest
    Y.forest = regression_forest(X, Y) 
    Y.hat    = predict(Y.forest)$predictions
    W.forest = regression_forest(X, W) 
    W.hat    = predict(W.forest)$predictions
    
    cf.raw   = causal_forest(X, 
                             Y, 
                             W,
                             Y.hat = Y.hat, 
                             W.hat = W.hat)
    
    varimp = variable_importance(cf.raw)
    names(varimp) = names(X)
    sorted_varimp = sort(varimp, decreasing = T)
    selected.idx = which(varimp > mean(varimp))
    
    #Second causal random forest
    cf = causal_forest(X[,selected.idx], 
                       Y, 
                       W, 
                       Y.hat = Y.hat,
                       W.hat = W.hat)
    
    #Preselection
    if (preselection == "yes") {
      
      #Second causal random forest if preselection is selected
      cf = causal_forest(X[,selected.idx], 
                         Y, 
                         W, 
                         Y.hat = Y.hat,
                         W.hat = W.hat,
                         cluster = X$Canton_3)
    }
                     
    #Heterogeneous treatment effects
    tau.hat = predict(cf, estimate.variance = T)$predictions
    
    #Average treatment effects and CIs
    ate = average_treatment_effect(cf)
    ci.ate = paste("95% CI for the ATE:", round(ate[1], 3), 
                   "+/-", round(qnorm(0.975)*ate[2], 3))
    
    ate.overlap = average_treatment_effect(cf, target.sample = "overlap") 
    ci.ate.overlap = paste("95% CI for the ATE:", round(ate.overlap[1], 3),
                           "+/-", round(qnorm(0.975)*ate.overlap[2], 3))
    
    att = average_treatment_effect(cf, target.sample = "treated")
    ci.att = paste("95% CI for the ATE:", round(att[1], 3),
                   "+/-", round(qnorm(0.975)*att[2], 3))
    
    #Test calibration
    test.calibration = test_calibration(cf)
    
    #Test calibration for propensity model (manually)
    DF.prop = data.frame(W = W,
                         e.bar      = mean(cf$W.hat),
                         e.residual = cf$W.hat - mean(cf$W.hat))
    
    
    best.linear.predictor = lm(W ~ e.bar + e.residual + 0, data = DF.prop)
    blp.summary.prop = lmtest::coeftest(best.linear.predictor,
                                        vcov = sandwich::vcovCL,
                                        type = "HC3")
    
    #Convert to one-sided p-values
    dimnames(blp.summary.prop)[[2]][4] = gsub("[|]", "", dimnames(blp.summary.prop)[[2]][4])
    blp.summary.prop[,4] = ifelse(blp.summary.prop[,3] < 0, 1 - blp.summary.prop[,4]/2, blp.summary.prop[,4]/2)
    
    
    
    #Test calibration for outcome model
    DF.out = data.frame(Y = Y,
                        m.bar      = mean(cf$Y.hat),
                        m.residual = cf$Y.hat - mean(cf$Y.hat))
    
    best.linear.predictor = lm(Y ~ m.bar + m.residual + 0, data = DF.out)
    blp.summary.out = lmtest::coeftest(best.linear.predictor,
                                       vcov = sandwich::vcovCL,
                                       type = "HC3")
    
    #convert to one-sided p-values
    dimnames(blp.summary.out)[[2]][4] = gsub("[|]", "", dimnames(blp.summary.out)[[2]][4])
    blp.summary.out[,4] = ifelse(blp.summary.out[,3] < 0, 1 - blp.summary.out[,4]/2, blp.summary.out[,4]/2)
    
    #Bias measure
    p = mean(W)
    Y.hat.0 = cf$Y.hat - W.hat * tau.hat
    Y.hat.1 = cf$Y.hat + (1 - W.hat) * tau.hat
    bias = (W.hat - p) * (p * (Y.hat.0 - mean(Y.hat.0))  + (1 - p) * (Y.hat.1 - mean(Y.hat.1)))
    
  }
  
  #Create list of results
  results = list(cf.raw           = cf.raw,
                 cf               = cf,
                 tau.hat          = tau.hat,
                 ate              = ate,
                 ci.ate           = ci.ate,
                 ate.overlap      = ate.overlap,
                 ci.ate.overlap   = ci.ate.overlap,
                 att              = att,
                 ci.att           = ci.att,
                 test.calibration = test.calibration,
                 bias             = bias,
                 blp.summary.prop = blp.summary.prop,
                 blp.summary.out  = blp.summary.out)
                 
  return(results)
} 