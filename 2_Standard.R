#DGP: Standard DGP with No Hidden Confounding and No Clustering
#Run the first function multiple times and aggregate the results over the different runs

#Empty environment
rm(list=ls())

#Set working directory here
setwd("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\Standard")

#Load packages
packages = c("grf", "future.apply", "doParallel", "doSNOW", "doRNG", "FNN",
             "BART", "bartCause", "rlist", "purrr", "data.table")
for (pkg in packages) {
  eval(bquote(library(.(pkg))))
}

#Load the first function
source(".\\1_Standard.R")

#Bringing Everything Together and Paralleization
run.standard = function(t, t.test, d, noise, entity, rho, n.sim, seed) {
  
  #t = number of observations in training set (number of time units)
  #t.test = number of observations in test set (number of time units in test set)
  #d = number of covariates
  #noise = variance of normal distribution
  #entity = number of entity fixed effects: the amount of times the X matrix is generated which are 
  #then stacked 
  #rho = persistence of serially correlated error
  #n.sim = number of simulations S
  #seed = seed for simulation study
  
  #Set seed
  set.seed(seed)
  
  #Parallel Computing 
  numcor = parallel::detectCores()
  clusterspar = makeSOCKcluster(numcor)
  registerDoSNOW(clusterspar)
  
  #Progress bars
  pb = txtProgressBar(max=n.sim, style=3)
  progress = function(t) {
    setTxtProgressBar(pb, t)
  }
  opts = list(progress=progress)
  
  #Add columns 
  columns = c("t","d","cf.mse", "cf.r.squared", "cf.bias","cf.sigma","cf.coverage", 
              "cf.ate", "cf.ate.sd", "cf.ate.true", "cf.ate.mse", "cf.ate.bias", "cf.ate.coverage")
  
  #Pre-define the output matrix that is to be filled
  output = setNames(data.frame(matrix(ncol = length(columns), nrow = 0)), columns)
  
  #Save results
  results = foreach(i=1:n.sim,
                    .combine = rbind,
                    .packages=c("grf", "FNN", "Kendall")) %dorng% {
                      
                      source(".\\1_Standard.R")
                      simulation.panel(t = t,
                                       t.test = t.test, 
                                       d = d, 
                                       noise = noise, 
                                       entity = entity, 
                                       rho = rho) 
                      
                    }
  
  #Combine output
  output = rbind.data.frame(output, colMeans(results))
  
  #Stop paralleization and the progress bar
  close(pb)
  stopCluster(clusterspar)
  
  #Define output
  output = setNames(output, columns)
  return(output)
}


#Run with varying parameters
n = c(200, 1000)
d = c(3:8, 24)

for (j in n){
  for(i in d){
    
    #Run
    result = run.standard(t = j, t.test = j, d = i, noise = 1, entity = 10, rho = 0.2, n.sim = 100, seed = 10)
    save.image(paste(".\\Results\\Standard",j,i,".RData", sep = ""))
  }
}
