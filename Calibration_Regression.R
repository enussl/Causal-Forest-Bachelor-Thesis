#Calibration regressions for design with true hidden confounding while clustering and T = 200, d = 10 
#for S = 100 runs

#Empty environment
rm(list=ls())

#Set working directory here and on line 17
setwd("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\Calibration Regression")

#Load packages
packages = c("grf", "future.apply", "doParallel", "doSNOW", "doRNG", "FNN", "BART",
             "bartCause", "rlist", "purrr", "data.table", "extrafont", "dplyr")
for (pkg in packages) {
  eval(bquote(library(.(pkg))))
}

#Load first function; observe that we draw it from the folder True Hidden Confounding (could also place a copy into this folder)
source("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\True Hidden Confounding\\1_True_Hidden_Confounding.R")

#Pre-allocation
list.results =  vector("list", 100)

for (k in 1:100) { #No parallel computing; not needed for this relatively fast simulation

  #Set seed for reproducibility; obviously a different seed for every run
  set.seed(k)
  
  #Simulate data
  data = gen_data.panelconftrue(t = 200, 
                                t.test = 200, 
                                d = 10, 
                                noise = 1,
                                entity = 10,
                                shift = "no",
                                rho = 0.2)
  
  #Run Causal Forest
  CF = causal_forest(data$X,
                     data$Y,
                     data$W,
                     num.trees = 3000, 
                     clusters = data$entity.fe,
                     seed = 1)
  
  #Test calibrations
  calib = test_calibration(CF)
  
  #Extract values from test calibration
  mean.forest              = calib[1,1]
  mean.forest.pval         = calib[1,4]
  differential.forest      = calib[2,1]
  differential.forest.pval = calib[2,4]
  
  #Return the results from the test calibration
  results =  list(mean.forest         = mean.forest,
              mean.forest.pval        = mean.forest.pval,
              differential.forest     = differential.forest,
              differential.forest.pval= differential.forest.pval)
  
  #List of results
  list.results[[k]] = results
  
} 

#Bind the matrices together to one large matrix
res = data.frame(matrix(unlist(list.results), nrow = length(list.results), byrow = T))
colnames(res) = c("mean.forest", "mean.forest.pval",
                  "differential.forest", "differential.forest.pval")

#Save the data into the folder with true hidden confounding
saveRDS(res, file = ".\\Results\\test.calibration.rds")


