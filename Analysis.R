#This script performs the analysis of the data

#Empty the environment
rm(list = ls())

#Set the working directory
setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish")

#Load the packages
packages = c("dplyr", "DBI", "dbplyr", "httr", "rvest", "data.table", "Hmisc", "grf",
             "ggplot2", "janitor", "reshape","vistime", "dygraphs", "viridis",
             "gridExtra", "magrittr", "ggpubr", "geodist", "Jmisc", "zoo", "extrafont",
             "lmtest", "sandwich", "xtable", "LaplacesDemon")
for (pkg in packages){
  eval(bquote(library(.(pkg))))
}

#Load data prep function into empty local environment in order no to interfere with saved objects
source(".\\Data Preparation Function.R", local = attach(NULL))

#Load data via data preparation function
merged = data.prep(selection = "yes", differencing = "no", start = "2020-08-21", finish = "2020-10-19")

#Variables that are not some sort of response variable, constant policies, useless timeframes etc.
cols.x = c("percentage_age", "SettlementAreaHa", "O65", "BedsPerCapita", "population", "restGatherings", "cancEvents",
            "ferien", "week", "Canton_3", "deaths", "recovered", "tests", "hosp", "kofstring_plus",
            "government_response_index", "economic_support_index",
            "containment_healt_index", "value.total", "tre200d0", "ure200d0",
            "sre000d0", "Density", "growth.Number.of.transactions", "growth.Amount.CHF") 

#Define design matrix, Y and W where Y, W have to be vectors 
##Response variables with different lags
Y5 = merged$log_median_R_mean_lead_5
Y5 = as.vector(Y5)
Y6 = merged$log_median_R_mean_lead_6
Y6 = as.vector(Y6)
Y7 = merged$log_median_R_mean_lead_7
Y7 = as.vector(Y7)
Y8 = merged$log_median_R_mean_lead_8
Y8 = as.vector(Y8)
Y10 = merged$log_median_R_mean_lead_10
Y10 = as.vector(Y10)
Y14 = merged$log_median_R_mean_lead_14
Y14 = as.vector(Y14)

##Treatment vector
W = merged$mask_treat
W = as.vector(W)

##Design matrix
X = merged[,names(merged) %in% cols.x, drop = F]

#Load data prep function into empty local environment in order no to interfere with saved objects
source("C:\\Users\\eminu\\Desktop\\COVID-19 Finish\\Analysis Function.R", local = attach(NULL))

#(I)MAIN ANALYSIS AND ROBUSTNESS-CHECKS

#Run Causal Forest
#Lag length l = 7 is our main model
res.lead7  = func.analysis(X = X, Y = Y7, W = W, cluster = "yes", preselection = "no")

#Robustness-checks with different lag lengths l
res.lead5  = func.analysis(X = X, Y = Y5, W = W, cluster = "yes", preselection = "no")
res.lead8  = func.analysis(X = X, Y = Y8, W = W, cluster = "yes", preselection = "no")
res.lead10 = func.analysis(X = X, Y = Y10, W = W, cluster = "yes", preselection = "no")
res.lead14 = func.analysis(X = X, Y = Y14, W = W, cluster = "yes", preselection = "no")

#Robustness-check with pre-selection 
res.lead7.presec = func.analysis(X = X, Y = Y7, W = W, cluster = "yes", preselection = "yes")

#Robustness-checls with no clustering
res.noclust = func.analysis(X = X, Y = Y7, W = W, cluster = "no", preselection = "no") 

#(II)FURTHER INVESTIGATIONS

#Best linear projection of the CATE; remove variables that induce multi-collinearity
X..multicoll = X %>%
  select(-c("percentage_age", "growth.Amount.CHF", "BedsPerCapita"))

##Run the best linear projections
best_linear_projection(res.lead7[["cf"]], A = X)
best_linear_projection(res.lead7[["cf"]], A = X.multicoll)

#Compute cantonal level average treatment effects
##Firstly compute the augmented inverse propensity CATE as described in the paper which we call dr.score
dr.score = res.lead7[["tau.hat"]] + res.lead7[["cf"]][["W.orig"]]/res.lead7[["cf"]][["W.hat"]] *
  (res.lead7[["cf"]][["Y.orig"]] - res.lead7[["cf"]][["Y.hat"]] - (1 - res.lead7[["cf"]][["W.hat"]]) *
  res.lead7[["tau.hat"]]) - (1 - res.lead7[["cf"]][["W.orig"]]) * 
  (res.lead7[["cf"]][["Y.orig"]] - res.lead7[["cf"]][["Y.hat"]] + res.lead7[["cf"]][["W.hat"]] * res.lead7[["tau.hat"]])

##Secondly construct the of canton matrix for the cantonal average treatment effects (the canton.matrix) picks out the CATE per canton
diag.mat = diag(x = 1, nrow = 26, ncol = 26)
canton.matrix = do.call(cbind, replicate(60, diag.mat, simplify=FALSE))

##Compute the cantonal ATE
canton.score  = canton.matrix %*% dr.score / 60 
sd(canton.score)

#Compute the vector of CATE per canton over time
tau.hat = res.lead7[["tau.hat"]]
tau.hat.canton = vector("list", 26)

##Computes CATE per canton over time
for (i in 1:26) {
  
  tau.hat.canton[[i]] = tau.hat[seq(i, length(tau.hat), 26)]
  
} 

##Create a data set that contains that information
cate.df = data.frame(AG = tau.hat.canton[[1]],
                     AI = tau.hat.canton[[2]],
                     AR = tau.hat.canton[[3]],
                     BE = tau.hat.canton[[4]],
                     BL = tau.hat.canton[[5]],
                     BS = tau.hat.canton[[6]],
                     FR = tau.hat.canton[[7]],
                     GE = tau.hat.canton[[8]],
                     GL = tau.hat.canton[[9]],
                     GR = tau.hat.canton[[10]],
                     JU = tau.hat.canton[[11]],
                     LU = tau.hat.canton[[12]],
                     NE = tau.hat.canton[[13]],
                     NW = tau.hat.canton[[14]],
                     OW = tau.hat.canton[[15]],
                     SG = tau.hat.canton[[16]],
                     SH = tau.hat.canton[[17]],
                     SO = tau.hat.canton[[18]],
                     SZ = tau.hat.canton[[19]],
                     TG = tau.hat.canton[[20]],
                     TI = tau.hat.canton[[21]],
                     UR = tau.hat.canton[[22]],
                     VD = tau.hat.canton[[23]],
                     VS = tau.hat.canton[[24]],
                     ZG = tau.hat.canton[[25]],
                     ZH = tau.hat.canton[[26]])

##Add indices
cate.df$datum = seq(from = as.Date("2020-08-21"), to = as.Date("2020-10-19"), by = 1)
cate.df$datum = as.Date(cate.df$datum)

#Heterogeneity at cantonal-level; we compute the best linear projection of the cantonal
#average treatment effects on the features that are constant within a month
X.Canton = X %>%
  group_by(Canton_3) %>%
  filter(row_number()==1) %>%
  ungroup() %>%
  select(SettlementAreaHa, O65, population, Density, Canton_3)
X.Canton = as.matrix(X.Canton)

##Best linear projection
coeftest(lm(canton.score ~ X.Canton), vcov = vcovHC)

#Crude comparison of high CTAE vs low CATE 
high.effect = res.lead7[["tau.hat"]] > median(res.lead7[["tau.hat"]])
ate.high = average_treatment_effect(res.lead7[["cf"]], subset = high.effect)
ate.low = average_treatment_effect(res.lead7[["cf"]], subset = !high.effect)
paste ("95% CI for difference in ATE:",
       round (ate.high[1] - ate.low[1], 3) , "+/ -",
       round (qnorm(0.975) * sqrt(ate.high[2]^2 + ate.low[2]^2), 3))


#(III)COMPUTE KULLBACK-LEIBER DIVERGENCES FOR COMPARISON OF CATE OF MODELS

#Main model and lag 5
density.5 = density(res.lead5[["tau.hat"]], n = 1000)
density.7 = density(res.lead7[["tau.hat"]], n = 1000)
kld.5 = KLD(density.7[["y"]], density.5[["y"]])[["mean.sum.KLD"]]

#Main model and lag 8
density.8 = density(res.lead8[["tau.hat"]], n = 1000)
kld.8 = KLD(density.7[["y"]], density.8[["y"]])[["mean.sum.KLD"]]

#Main model and lag 10
density.10 = density(res.lead10[["tau.hat"]], n = 1000)
kld.10 = KLD(density.7[["y"]], density.10[["y"]])[["mean.sum.KLD"]]

#Main model and lag 14
density.14 = density(res.lead14[["tau.hat"]], n = 1000)
kld.14 = KLD(density.7[["y"]], density.14[["y"]])[["mean.sum.KLD"]]

#Main model and un-clustered
density.noclust = density(res.noclust[["tau.hat"]], n = 1000)
kld.noclust = KLD(density.7[["y"]], density.noclust[["y"]])[["mean.sum.KLD"]]

#Main model and pre-selection
density.presec = density(res.lead7.presec[["tau.hat"]], n = 1000)
kld.presec = KLD(density.7[["y"]], density.presec[["y"]])[["mean.sum.KLD"]]


#(IV)EXPORT THE RESULTS FOR THE MAIN MODEL TO CREATE THE PLOTS IN THE PLOTS SCRIPT
saveRDS(res.lead7, file(".\\Plots\\Results_Main_Model.rds"))

