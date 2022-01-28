#DGP: Standard DGP with No Hidden Confounding and No Clustering
#Make table with all the combinations of d,T

#Empty environment
rm(list=ls())

#Set your working directory here; input the results sub-folder
setwd("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\Standard\\Results")

#Load packages
packages = c("grf", "future.apply", "doParallel", "doSNOW", "doRNG", "FNN",
             "BART", "bartCause", "rlist", "purrr", "data.table", "xtable", "tidyverse")
for (pkg in packages) {
  eval(bquote(library(.(pkg))))
}

#Parameters; as a reminder
n = c(200, 1000)
d = c(3:8, 24)

#Names of rdata files in folder
spec = c(2003, 2004, 2005, 2006, 2007, 2008, 20024, 10003, 10004, 10005, 10006, 10007, 10008, 100024)

for(b in 1:length(spec)) {
  
  #Load data
  load(paste("Standard",spec[b],".RData", sep = ""))
  
  #Create df
  df = data.frame(result[c("t","d", "cf.mse", "cf.r.squared", "cf.bias", "cf.coverage",
                           "cf.ate", "cf.ate.sd", "cf.ate.true", "cf.ate.mse", "cf.ate.bias", "cf.ate.coverage")])
  #Name df
  assign(paste("X", spec[b], sep = ""), df)
}

#Rbind the results together
dat = rbind(X2003, X2004, X2005, X2006, X2007, X2008, X20024, X10003, X10004, X10005, X10006, X10007, X10008, X100024)

#Create xtable for latex
xtable(dat, type = latex, digits = 4)
