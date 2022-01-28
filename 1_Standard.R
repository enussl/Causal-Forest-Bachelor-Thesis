#DGP: Standard DGP with No Hidden Confounding and No Clustering
#Simulate data, run Causal Forest and evaluate (one simulation run)

#Empty environment
rm(list=ls())

#Set working directory here
setwd("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\Standard")

#Load packages
packages = c("grf", "future.apply", "doParallel", "doSNOW", "doRNG", "FNN",
             "BART", "bartCause", "rlist", "purrr", "data.table", "dplyr")
for (pkg in packages) {
  eval(bquote(library(.(pkg))))
}

#TREATMENT FUNCTIONS
#Nonlinear treatment function
tau_nonlinear = function(x) {
  1 + 1/(1+exp(-20*(x-1/3)))
}

#Linear treatment function
tau_linear = function(x) {
  2*x[,1]+x[,2]
}

#Propensity where dbeta is the beta-density with shape parameters 2 and 4
propensity = function(x) {
  1/4*(1 + dbeta(x,2,4))
}

#DGP WITHOUT HIDDEN CONFOUNDING
gen_data.panel = function(t, t.test, d, noise, entity, rho){
  
  #t = number of observations in training set (number of time units)
  #t.test = number of observations in test set (number of time units in test set)
  #d = number of covariates
  #noise = variance of normal distribution
  #entity = number of entity fixed effects: the amount of times the X matrix is generated which are 
  #then stacked 
  #rho = persistence of serially correlated error
  
  #Pre-allocate the list we are going to put the matrices in
  list.mat =  vector("list", entity)
  
  #Generate a matrix for all entity fixed effects
  for(j in 1:entity){
    
    #Entity error term which is the same for all observations within the entity
    alpha = rnorm(1, 0, noise)
    
    #Serially Correlated Errors
    errors = rnorm(n = t, 0, noise)
    rho = rho
    for(i in 2:t){
      errors[i] = rho * errors[i-1] + rnorm(1, 0, noise)
    }
    
    #Design matrix
    X = matrix(runif(t*d, 0, 1), t, d) 
    X.test = matrix(runif(t.test*d, 0, 1), t.test, d) 
    
    #Naming the columns of X and X.test
    colnames(X) = c(paste0("X", 1:d))
    colnames(X.test) = c(paste0("X.test", 1:d))
    
    #Generate the treatment vector W which is persistent: play around with propensity scores (0,1,...,x)
    #meaning if W_it = 1 \implies W_it+p = 1 \forall p\geq 1
    W = rbinom(t, size = 1, p = 0.05*sapply(X[,1], propensity))
    for(i in 1:(length(W)-1)){
      if(W[i] == 1){
        W[i+1] = 1 
      }
    }
    
    #Generate the target variable Y 
    Y = X[,1] + X[,3] + W * sapply(X[,1], tau_nonlinear) + alpha + errors 
  
    #Compute the het. treatment effects
    tau = sapply(X.test[,1], tau_nonlinear)
    
    #Which entity is it from 
    entity.fe = j
    
    #Make a matrix consisting of Y, W, X, tau and entity.fe
    mat = cbind(Y, W, X, X.test, tau, entity.fe)
    list.mat[[j]] = mat
  }
  
  #Bind the matrices together to one large matrix
  res = NULL
  for(i in 1:entity){
    res = rbind(res, list.mat[[i]])
  }
  
  #Preparing the correct formulation for the columns
  Xnam = paste0("X", 1:d)
  Xtestnam = paste0("X.test", 1:d)
  
  #Return panel data
  data.panel = list(X = res[,Xnam],
                    X.test = res[,Xtestnam],
                    W = res[,"W"],
                    Y = res[,"Y"],
                    tau = res[,"tau"],
                    entity.fe = res[,"entity.fe"])
  
  return(data.panel)
}

#EVALUATION FOR CATE
#MSE  
mse = function(predictions, true) {
  return(mean(predictions-true)^2)
}

#RSQUARED
r.squared = function(predictions, true) {
  return(1 - (mean(predictions-true)^2)/var(true))
}


#Bias
bias = function(predictions, true) {
  return(abs(mean(predictions-true)))
} 

#Coverage
covered = function(predictions, true, sigma) {
  return(mean(abs(predictions-true)/sigma <= 1.96))
}

#Evaluate 
evaluate = function(predictions, true, sigma) {
  return(list(mse = mse(predictions, true),
              r.squared = r.squared(predictions, true),
              bias = bias(predictions, true),
              coverage = covered(predictions, true, sigma)))
}

#EVALUATION FOR THE ATE
ate = function(true) {
  return(mean(true))
}

evaluate.ate = function(predictions, true, sigma) {
  return(list(mse.ate  = mse(predictions, true),
              bias.ate = bias(predictions, true),
              coverage.ate = covered(predictions, true, sigma)))
}


#ESTIMATOR
CF_estimator = function(X,
                        X.test,
                        Y,
                        W,
                        clusters = NULL, 
                        Y.hat = NULL,
                        W.hat = NULL,
                        num.trees = 3000,
                        honesty = TRUE,
                        honesty.fraction = 0.5,
                        honesty.prune.leaves = TRUE,
                        min.node.size = 5) {
  CF = causal_forest(X,
                     Y,
                     W,
                     Y.hat = Y.hat,
                     W.hat = W.hat,
                     num.trees = 3000,
                     clusters = clusters,
                     min.node.size = min.node.size,
                     honesty = honesty,
                     honesty.fraction = honesty.fraction,
                     honesty.prune.leaves = honesty.prune.leaves,
                     seed = 1)
  
  estimates = predict(CF,
                      X.test,
                      estimate.variance = TRUE)
  
  ate = average_treatment_effect(CF, target.sample = "overlap")
  
  return(list(predictions = estimates$predictions,
              sigma = sqrt(estimates$variance.estimates),
              ate = ate,
              var.imp = variable_importance(CF)))
}


#SIMULATION WITHOUT HIDDEN CONFOUNDING AND NO CLUSTERING
simulation.panel = function(t, t.test, d, noise, entity, clusters, rho){ 
  
  #t = number of observations in training set (number of time units)
  #t.test = number of observations in test set (number of time units in test set)
  #d = number of covariates
  #noise = #noise = variance of normal distribution
  #entity = number of entity fixed effects: the amount of times the X matrix is generated which are 
  #then stacked 
  #noise = variance of normal distribution
  #rho = persistence of serially correlated error
  
  #Start with the simulation of the data; we do this for every simulation setup
  data = gen_data.panel(t = t, 
                        t.test = t.test, 
                        d = d, 
                        noise = noise,
                        entity = entity,
                        rho = rho)
  
  #Run the causal forest
  cf = CF_estimator(X = data$X, 
                    X.test = data$X.test,
                    Y = data$Y,
                    W = data$W) #no clustering
  
  
  #Evaluation for CATE
  cf.eval = evaluate(cf$predictions, data$tau, cf$sigma)
  
  #evaluation for ATE
  ate.true = ate(data$tau)
  cf.eval.ate = evaluate.ate(predictions = cf[["ate"]][["estimate"]], true = ate.true, sigma = cf[["ate"]][["std.err"]])
  
  #Put everything into a dataframe for easy access
  data.frame(t = t,
             d = d,
             cf.mse = cf.eval$mse,
             cf.r.squared = cf.eval$r.squared,
             cf.bias = cf.eval$bias,
             cf.sigma = mean(cf$sigma),
             cf.coverage = cf.eval$coverage,
             cf.ate = cf[["ate"]][["estimate"]],
             cf.ate.sd = cf[["ate"]][["std.err"]],
             cf.ate.true = ate.true,
             cf.ate.mse = cf.eval.ate$mse.ate,
             cf.ate.bias = cf.eval.ate$bias.ate,
             cf.ate.coverage = cf.eval.ate$coverage.ate)
}
