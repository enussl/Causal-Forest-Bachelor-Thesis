#This script creates the plots for the thesis (and some unused plots)

#Empty environment
rm(list = ls())

#Set working directory here; has to be reset once more on line 37 and once on line 231
setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish")

#Load packages
packages = c("dplyr", "DBI", "dbplyr", "httr", "rvest", "data.table", "Hmisc", "grf",
             "ggplot2", "janitor", "reshape","vistime", "dygraphs", "viridis",
             "gridExtra", "magrittr", "ggpubr", "geodist", "Jmisc", "extrafont",
             "sf", "rcartocolor", "RColorBrewer", "colorRamps", "Polychrome",
             "corrplot")
for (pkg in packages){
  eval(bquote(library(.(pkg))))
}

#Install the latex font
font_install("fontcm")
loadfonts(device = "win")

#Load data prep function into empty local environment in order no to interfere with saved objects
source(".\\Data Preparation Function.R", local = attach(NULL))

#(I)MAKE PLOTS 

##(i)COVID-19 RELATED PLOTS

#Load COVID-19 data
merged = data.prep(selection = "yes", differencing = "no", start = "2020-08-21", finish = "2020-10-19")

##Correct data type for date
merged$datum = as.Date(merged$datum)

#Reset working directory (due to source)
setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish")

#Plot of R values per canton over time (Figure 6)
pdf(".\\Plots\\R_canton.pdf", width = 15, height = 10)
ggplot(merged, aes(x = as.Date(datum), y = median_R_mean)) +
  geom_line(color = "firebrick", size = 1) +
  geom_ribbon(data = merged, aes(ymin = median_R_lowHPD, ymax = median_R_highHPD), 
              alpha = 0.3) +
  facet_wrap( ~ geoRegion) +
  coord_cartesian(ylim = c(0, 6)) +
  geom_segment(x = as.Date("2020-08-19"), y = 1, xend = as.Date("2020-10-19"), yend = 1, linetype = 2) +
  scale_x_date(breaks = "1 month", date_labels = "%b %y") +
  labs(x = "", y = "Effective Reproductive Number", color = "Cantons") +
  theme_minimal() +
  theme(text = element_text(size = 20, family = "CM Roman"), 
        axis.title.y = element_text(margin = margin(r = 5)),
        panel.grid = element_line(linetype = 2, size = 0.5),
        panel.grid.minor = element_line(linetype = 2, size = 0.5),
        panel.border = element_blank())
dev.off()


#Correlation matrix
##Select continuous variables 
cols.corr = c("percentage_age", "SettlementAreaHa", "O65", "BedsPerCapita", "population",
              "week", "deaths", "recovered", "tests", "hosp", "kofstring_plus",
              "government_response_index", "economic_support_index",
              "containment_healt_index", "value.total", "tre200d0", "ure200d0",
              "sre000d0", "Density", "growth.Number.of.transactions", "growth.Amount.CHF", "median_R_mean") 

vars = merged[,names(merged) %in% cols.corr, drop = F]
vars = na.omit(vars)

colnames(vars) = c("median_r", "perc_age", "area", "density", "abs_age", "beds", "deaths", "recovered",
                   "tests", "hosp", "gov_resp", "eco_supp", "pop", "kof_stringency",  "amount_spent",
                   "transactions", "mobility", "sunshine", "temp", "humidity", "week")

##Compute correlations
vars.corr = cor(vars)
colnames(vars.corr) = c("median_r", "perc_age", "area", "density", "abs_age", "beds", "deaths", "recovered",
                        "tests", "hosp", "gov_resp", "eco_supp", "pop", "kof_stringency",  "amount_spent",
                        "transactions", "mobility", "sunshine", "temp", "humidity", "week")
rownames(vars.corr) = c("median_r", "perc_age", "area", "density", "abs_age", "beds", "deaths", "recovered",
                        "tests", "hosp", "gov_resp", "eco_supp", "pop", "kof_stringency",  "amount_spent",
                        "transactions", "mobility", "sunshine", "temp", "humidity", "week")

pdf(".\\Plots\\corrmat.pdf",
    height = 8, width = 8)
corrplot(vars.corr, family = "CM Roman", method = "color", addCoef.col = "black",
         number.cex = 0.55, addgrid.col = "white", tl.col = "black",
         type = "upper", tl.srt = 45, col = brewer.pal(n = 8, "Reds"),
         number.font = 5)
dev.off()

##(ii)POLICY-RELATED PLOTS

#Read in policy data
measures = read.table(".\\Data\\KOFMeasures.txt", sep = ",", header = T, encoding = "UTF-8")
measures$geo = toupper(measures$geo) # capital letters
names(measures)[names(measures) == "geo"] = "geoRegion"
names(measures)[names(measures) == "time"] = "datum"

##Reshape to wide
measures_wide = reshape(data      = measures,
                        idvar     = c("datum", "geoRegion"),
                        v.names   = "value",
                        timevar   = "variable",
                        direction = "wide")
measures_wide = measures_wide[order(measures_wide$geoRegion),] 
measures_wide = measures_wide[measures_wide$datum >= "2020-02-24",] 
names(measures_wide)[names(measures_wide) == "value.c1_schoolclosing"] = "schoolClosing"
names(measures_wide)[names(measures_wide) == "value.c2_workplaceclosing"] = "workClosing2"
names(measures_wide)[names(measures_wide) == "value.c2a_workplaceclosing"] = "workClosing2a"
names(measures_wide)[names(measures_wide) == "value.c3_cancelpublicevents"] = "cancEvents"
names(measures_wide)[names(measures_wide) == "value.c4_restrictionsongatherings"] = "restGatherings"
names(measures_wide)[names(measures_wide) == "value.c5_closepublictransport"] = "closePubtransp"
names(measures_wide)[names(measures_wide) == "value.c6_stayathomerequirements"] = "stayHomeReq"
names(measures_wide)[names(measures_wide) == "value.c7_domestictravel"] = "domTravel"
names(measures_wide)[names(measures_wide) == "value.c8_internationaltravel"] = "intTravel"
names(measures_wide)[names(measures_wide) == "value.h1_publicinfocampaign"] = "infoCamp"
names(measures_wide)[names(measures_wide) == "value.h6_facialcoverings"] = "facialCover"
names(measures_wide)[names(measures_wide) == "value.stringency"] = "kofstring"
names(measures_wide)[names(measures_wide) == "value.stringency_plus"] = "kofstring_plus"

remove(measures)

##Date adjustment
measures_wide = measures_wide %>%
  mutate(datum = as.Date(datum))

#Facial cover plot (Figure 5)
pdf(".\\Plots\\policy_fraction.pdf", width = 20, height = 10)
ggplot(mapping = aes(x = datum, y = facialCover, group = geoRegion)) +
  geom_line(data = measures_wide %>% filter(datum <= "2020-10-31" & datum >= "2020-06-22" & geoRegion != "CH" & geoRegion != "FL"), 
            size = 1.5) +
  geom_line(data = measures_wide %>% filter(geoRegion == "CH" & datum <= "2020-10-31" & datum >= "2020-06-22"),
            size = 2.5, color = "firebrick") +
  geom_segment(aes(x = as.Date(c("2020-08-20")), y = 1.79, xend  = as.Date(c("2020-08-20")), yend = 1.99), data = measures_wide,
                                                     colour = "grey", linetype = 1, size = 0.75) +
  geom_segment(aes(x = as.Date(c("2020-08-23")), y = 1.69, xend  = as.Date(c("2020-08-23")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-09-16")), y = 1.89, xend  = as.Date(c("2020-09-16")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-10-11")), y = 1.89, xend  = as.Date(c("2020-10-11")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-10-13")), y = 1.79, xend  = as.Date(c("2020-10-13")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-10-15")), y = 1.69, xend  = as.Date(c("2020-10-15")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-10-16")), y = 1.59, xend  = as.Date(c("2020-10-16")), yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  geom_segment(aes(x = as.Date(c("2020-10-17"))+0.1, y = 1.49, xend  = as.Date(c("2020-10-17"))+0.1, yend = 1.99), data = measures_wide,
               colour = "grey", linetype = 1, size = 1) +
  annotate("text", x = as.Date(c("2020-08-20")), y = 1.75, label = c("NE"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-08-23")), y = 1.65, label = c("BS"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-09-16")), y = 1.85, label = c("VD"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-10-11")), y = 1.85, label = c("BE"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-10-13")), y = 1.75, label = c("GE"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-10-15")), y = 1.65, label = c("SZ"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-10-16")), y = 1.55, label = c("LU, GR, FR"), family = "CM Roman", size = 8) +
  annotate("text", x = as.Date(c("2020-10-17")), y = 1.45, label = c("VS"), family = "CM Roman", size = 8) +
  scale_x_date(breaks = "1 month", date_labels = "%b %y") +
  scale_y_continuous(breaks= c(1,2,3)) +
  labs(x = "", y = "Facial Mask Policies", color = "Cantons") +
  theme_minimal() +
  theme(text = element_text(size = 30, family = "CM Roman"), 
        panel.grid = element_line(linetype = 2, size = 1.25),
        panel.grid.minor = element_line(linetype = 2, size = 1.25),
        panel.border = element_blank()) 
dev.off()


##(iii)ANALYSIS-RELATED PLOTS

#Read in analysis data
res.lead7 = readRDS(file = ".\\Plots\\Results_Main_Model.rds")

#Bias plot
##Percentiles bias
quantile(res.lead7[["bias"]], c(0.05, 0.95)) 
median(res.lead7[["bias"]])

##Plot (Figure 8)
pdf(file = ".\\Plots\\bias.pdf", width = 9, height = 9) 
par(cex.lab = 1.4)
par(cex.axis = 1.4)
hist(res.lead7[["bias"]]/sd(res.lead7[["cf"]][["Y.orig"]]), 
     prob = T, col = "white", 
     xlab = "", main = "", breaks = 20, family = "CM Roman",
     panel.first = grid(lwd = 3), ylim = c(0,8))
title(xlab = expression(frac(Bias, sd[Y])), mgp = c(4,1,0), family = "CM Roman", cex.lab = 1.4)
lines(density(res.lead7[["bias"]]/sd(res.lead7[["cf"]][["Y.orig"]]), adjust = 3), 
      col = "firebrick", lwd = 3)
dev.off()

#Heterogeneous treatment effects plot (Figure 4)
pdf(file = ".\\Plots\\tauhat.pdf",
    width = 8, height = 8)
par(cex.lab = 1.4)
par(cex.axis = 1.4)
hist(res.lead7[["tau.hat"]], breaks = 20, 
     prob = T, col = "white", 
     xlab = "Out-Of-Bag Heterog. Treatment Effects" ,
     main = "", panel.first = grid(lwd = 3), family = "CM Roman",
     ylim = c(0,14), xlim = c(-0.2,0.1))
lines(density(res.lead7[["tau.hat"]], adjust = 3), col = "firebrick", family = "CM Roman", lwd = 3)
dev.off()

#QQ-plot for heterogeneous treatment effects (Figure 4)
pdf(file = ".\\Plots\\tauhatqqplot.pdf",
    width = 8, height = 8)
par(cex.lab = 1.4)
par(cex.axis = 1.4)
qqnorm(res.lead7[["tau.hat"]], main = "", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       panel.first = grid(lwd = 3), family = "CM Roman")
qqline(res.lead7[["tau.hat"]], col = "firebrick", lwd = 3)
dev.off()

#Overlap Plot (Figure 7)
pdf(".\\Plots\\overlap.pdf", 
    width = 8, height = 8)
par(cex.lab = 1.4)
par(cex.axis = 1.4)
hist(res.lead7[["cf"]][["W.hat"]], breaks = 20, 
     main = "", col = "white", 
     prob = T, xlab = expression(hat(W)), 
     family = "CM Roman", panel.first = grid(lwd = 3),
     xlim = c(0,1))
dev.off()

##(iv)PLOTS OF CALIBRATION REGRESSIONS

#Set working directory
setwd("C:\\Users\\eminu\\Desktop\\Bachelor Thesis Finish\\Calibration Regression\\Results")

#Load data
calib = readRDS(file = ".\\test.calibration.rds")

#Histograms of alpha 
pdf(file = ".\\alpha.pdf",
    width = 8, height = 8)
par(cex.lab = 1.4)
par(cex.axis = 1.4)
hist(calib[["mean.forest"]], breaks = 20, 
     prob = T, col = "white", 
     xlab = "Mean Forest Prediction" ,
     main = "", panel.first = grid(lwd = 3), family = "CM Roman",
     xlim = c(0.7,1.3), ylim = c(0,12))
lines(density(calib[["mean.forest"]], adjust = 3), col = "firebrick", family = "CM Roman", lwd = 4)
dev.off()

#Histograms of beta 
pdf(file = ".\\beta.pdf",
    width = 8, height = 8)
par(cex.lab = 1.4)
par(cex.axis = 1.4)
hist(calib[["differential.forest"]], breaks = 20, 
     prob = T, col = "white", 
     xlab = "Differential Forest Prediction" ,
     main = "", panel.first = grid(lwd = 3), family = "CM Roman",
     xlim = c(-10,4), ylim = c(0,0.4))
lines(density(calib[["differential.forest"]], adjust = 3), col = "firebrick", family = "CM Roman", lwd = 4)
dev.off()








# #(II)PLOTS NOT USED IN THESIS
# 
# #Make fancy map plots (a little bit extra)
# #(data: https://www.bfs.admin.ch/bfs/en/home/services/geostat/swiss-federal-statistics-geodata/administrative-boundaries/generalized-boundaries-local-regional-authorities.html)
# swiss_cantons = st_read("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Kantone Daten\\ggg_2021-LV03\\shp\\k3k21.shp")
# swiss_lakes = st_read("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Kantone Daten\\ggg_2021-LV03\\shp\\k4s21.shp")
# 
# setwd("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff")
# merged = read.csv("Data_Covid.csv", sep = ",", header = T, encoding = "UTF-8")
# 
# merged["geoRegion"][merged["geoRegion"] == "ZH"] = "Zürich"
# merged["geoRegion"][merged["geoRegion"] == "BE"] = "Bern / Berne"
# merged["geoRegion"][merged["geoRegion"] == "AG"] = "Aargau"
# merged["geoRegion"][merged["geoRegion"] == "AI"] = "Appenzell Innerrhoden"
# merged["geoRegion"][merged["geoRegion"] == "AR"] = "Appenzell Ausserrhoden"
# merged["geoRegion"][merged["geoRegion"] == "LU"] = "Luzern"
# merged["geoRegion"][merged["geoRegion"] == "UR"] = "Uri"
# merged["geoRegion"][merged["geoRegion"] == "SZ"] = "Schwyz"
# merged["geoRegion"][merged["geoRegion"] == "OW"] = "Obwalden"
# merged["geoRegion"][merged["geoRegion"] == "NW"] = "Nidwalden"
# merged["geoRegion"][merged["geoRegion"] == "GL"] = "Glarus"
# merged["geoRegion"][merged["geoRegion"] == "ZG"] = "Zug"
# merged["geoRegion"][merged["geoRegion"] == "FR"] = "Fribourg / Freiburg"
# merged["geoRegion"][merged["geoRegion"] == "SO"] = "Solothurn"
# merged["geoRegion"][merged["geoRegion"] == "BS"] = "Basel-Stadt"
# merged["geoRegion"][merged["geoRegion"] == "BL"] = "Basel-Landschaft"
# merged["geoRegion"][merged["geoRegion"] == "SH"] = "Schaffhausen"
# merged["geoRegion"][merged["geoRegion"] == "SG"] = "St. Gallen"
# merged["geoRegion"][merged["geoRegion"] == "GR"] = "Graubünden / Grigioni / Grischun"
# merged["geoRegion"][merged["geoRegion"] == "TG"] = "Thurgau"
# merged["geoRegion"][merged["geoRegion"] == "TI"] = "Ticino"
# merged["geoRegion"][merged["geoRegion"] == "VD"] = "Vaud"
# merged["geoRegion"][merged["geoRegion"] == "VS"] = "Valais / Wallis"
# merged["geoRegion"][merged["geoRegion"] == "NE"] = "Neuchâtel"
# merged["geoRegion"][merged["geoRegion"] == "GE"] = "Genève"
# merged["geoRegion"][merged["geoRegion"] == "JU"] = "Jura"
# 
# names(merged)[names(merged) == "geoRegion"] = "KTNAME"
# 
# merged = merged %>%
#   group_by(KTNAME) %>%
#   filter(row_number()==1) %>%
#   ungroup()
# 
# #### dataset with which we can work
# cantonal = left_join(merged, swiss_cantons)
# 
# #### Plot of density
# ggsave("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Plots\\density.pdf")
# ggplot(cantonal, aes(geometry = geometry)) +
#   geom_sf(aes(fill = Density), size = 0.3) +
#   scale_fill_carto_c(palette = 12,
#                      guide = guide_legend(direction = "horizontal",
#                                           keyheight = unit(2, units = "mm"),
#                                           keywidth = unit(70 / 5, units = "mm"),
#                                           title.position = 'top',
#                                           title.hjust = 0.5,
#                                           label.hjust = 0.5,
#                                           nrow = 1,
#                                           byrow = T,
#                                           label.position = "bottom")) +
#   geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea") +
#   theme_void() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom",
#         text = element_text(size = 9, family = "CM Roman"))
# dev.off()
# 
# #### Plot of average covid R value
# setwd("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff")
# merged = read.csv("Data_Covid.csv", sep = ",", header = T, encoding = "UTF-8")
# 
# merged["geoRegion"][merged["geoRegion"] == "ZH"] = "Zürich"
# merged["geoRegion"][merged["geoRegion"] == "BE"] = "Bern / Berne"
# merged["geoRegion"][merged["geoRegion"] == "AG"] = "Aargau"
# merged["geoRegion"][merged["geoRegion"] == "AI"] = "Appenzell Innerrhoden"
# merged["geoRegion"][merged["geoRegion"] == "AR"] = "Appenzell Ausserrhoden"
# merged["geoRegion"][merged["geoRegion"] == "LU"] = "Luzern"
# merged["geoRegion"][merged["geoRegion"] == "UR"] = "Uri"
# merged["geoRegion"][merged["geoRegion"] == "SZ"] = "Schwyz"
# merged["geoRegion"][merged["geoRegion"] == "OW"] = "Obwalden"
# merged["geoRegion"][merged["geoRegion"] == "NW"] = "Nidwalden"
# merged["geoRegion"][merged["geoRegion"] == "GL"] = "Glarus"
# merged["geoRegion"][merged["geoRegion"] == "ZG"] = "Zug"
# merged["geoRegion"][merged["geoRegion"] == "FR"] = "Fribourg / Freiburg"
# merged["geoRegion"][merged["geoRegion"] == "SO"] = "Solothurn"
# merged["geoRegion"][merged["geoRegion"] == "BS"] = "Basel-Stadt"
# merged["geoRegion"][merged["geoRegion"] == "BL"] = "Basel-Landschaft"
# merged["geoRegion"][merged["geoRegion"] == "SH"] = "Schaffhausen"
# merged["geoRegion"][merged["geoRegion"] == "SG"] = "St. Gallen"
# merged["geoRegion"][merged["geoRegion"] == "GR"] = "Graubünden / Grigioni / Grischun"
# merged["geoRegion"][merged["geoRegion"] == "TG"] = "Thurgau"
# merged["geoRegion"][merged["geoRegion"] == "TI"] = "Ticino"
# merged["geoRegion"][merged["geoRegion"] == "VD"] = "Vaud"
# merged["geoRegion"][merged["geoRegion"] == "VS"] = "Valais / Wallis"
# merged["geoRegion"][merged["geoRegion"] == "NE"] = "Neuchâtel"
# merged["geoRegion"][merged["geoRegion"] == "GE"] = "Genève"
# merged["geoRegion"][merged["geoRegion"] == "JU"] = "Jura"
# 
# names(merged)[names(merged) == "geoRegion"] = "KTNAME"
# 
# merged.average = merged %>%
#   group_by(KTNAME) %>%
#   dplyr::summarize(mean.R = mean(median_R_mean, na.rm = TRUE))
# 
# #### dataset with which we can work
# cantonal.r = left_join(merged.average, swiss_cantons)
# 
# colfunc = colorRampPalette(c("firebrick", "white"))
# colfunc(10)
# 
# ggsave("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Plots\\meanRval.pdf")
# ggplot(cantonal.r, aes(geometry = geometry)) +
#   geom_sf(aes(fill = mean.R), size = 0.3) +
#   scale_fill_gradient(low = "#DC9C9C", high = "firebrick",
#                       guide = guide_legend(direction = "horizontal",
#                                            keyheight = unit(2, units = "mm"),
#                                            keywidth = unit(70 / 5, units = "mm"),
#                                            title.position = 'top',
#                                            title.hjust = 0.5,
#                                            label.hjust = 0.5,
#                                            nrow = 1,
#                                            byrow = T,
#                                            label.position = "bottom")) +
#   geom_sf(data = swiss_lakes, fill = "#d1eeea", color = "#d1eeea") +
#   theme_void() +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom", 
# #         text = element_text(size = 25, family = "CM Roman")) 
# # dev.off()
# 
# 
# #Restrictions on gatherings (not in thesis; therefore ugly)
# pdf("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Plots\\policy_gatherings.pdf", width = 20, height = 10)
# ggplot(mapping = aes(x = datum, y = restGatherings, group = geoRegion, color = geoRegion)) +
#   geom_line(data = measures_wide %>% filter(datum <= "2020-10-31" & datum >= "2020-06-22" & geoRegion != "CH" & geoRegion != "FL"), 
#             size = 1) +
#   geom_line(data = measures_wide %>% filter(geoRegion == "CH" & datum <= "2020-10-31" & datum >= "2020-06-22"), 
#             color = "black", size = 1.5) +
#   scale_x_date(breaks = "1 month", date_labels = "%b %y") +
#   labs(x = "Datum", y = "Restrictions on Gatherings", color = "Cantons") +
#   theme_minimal() +
#   theme(text = element_text(size = 30, family = "CM Roman"), 
#         panel.grid = element_line(linetype = 2, size = 1),
#         panel.grid.minor = element_line(linetype = 2, size = 1),
#         panel.border = element_blank()) 
# dev.off()
# 
# #Cancellation of events (not in thesis; therefore ugly)
# pdf("C:\\Users\\eminu\\OneDrive\\Desktop\\HS2021\\Bachelorarbeit\\Covid Stuff\\Plots\\canc_events.pdf", width = 20, height = 10)
# ggplot(mapping = aes(x = datum, y = cancEvents, group = geoRegion, color = geoRegion)) +
#   geom_line(data = measures_wide %>% filter(datum <= "2020-10-31" & datum >= "2020-06-22" & geoRegion != "CH" & geoRegion != "FL"), 
#             size = 1) +
#   geom_line(data = measures_wide %>% filter(geoRegion == "CH" & datum <= "2020-10-31" & datum >= "2020-06-22"), 
#             color = "black", size = 1.5) +
#   scale_x_date(breaks = "1 month", date_labels = "%b %y") +
#   labs(x = "Datum", y = "Cancellation of Events", color = "Cantons") +
#   theme_minimal() +
#   theme(text = element_text(size = 30, family = "CM Roman"), 
#         panel.grid = element_line(linetype = 2, size = 2),
#         panel.grid.minor = element_line(linetype = 2, size = 2),
#         panel.border = element_blank()) 
# dev.off()
# # 
# # #Plot of ENTRIES over time 
# merged %>% ggplot(aes(datum, entries, group = geoRegion)) + 
#   aes(color = geoRegion) + 
#   geom_line(size = 1, alpha = 0.4) +
#   geom_point(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Entries") +
#   coord_fixed(ratio = 0.2) + #ratio is y/x
#   theme_bw() #background
# 
# #Plot of differences entries over time
# merged %>% ggplot(aes(datum, entries_1d, group = geoRegion)) + 
#   aes(color = geoRegion) + 
#   geom_line(size = 1, alpha = 0.4) +
#   geom_point(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Differenced Entries") +
#   coord_fixed(ratio = 0.2) + #ratio is y/x
#   theme_bw() #background
# 
# #Plot mask policy over time
# merged %>% ggplot(aes(as.Date(datum), facialCover)) + 
#   aes(color = geoRegion) + 
#   geom_vline(xintercept = as.numeric(as.Date("2020-08-24")), linetype = "dotted") +
#   #geom_text(aes(x=as.numeric(as.Date("2020-08-24")), label="BS and NE", y=2), colour = "black", angle=90, text=element_text()) +
#   facet_wrap( ~ geoRegion) +
#   geom_line(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Mask Policy") +
#   #coord_fixed(ratio = 0.2) + #ratio is y/x
#   theme_bw() #background
# 
# #Plot entries and mask policy for one canton (take VD)
# # ggplot is not made for two axis so we have to create a scale factor
# g1 = ggplot(subset(merged, geoRegion == "VD"), aes(datum, entries,  color = geoRegion)) +
#   geom_line(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Entries Waadt") +
#   theme_bw()
# g2 = ggplot(subset(merged, geoRegion == "VD"), aes(datum, facialCover,  color = geoRegion)) +
#   geom_line(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Mask Policy Waadt") +
#   theme_bw()
# grid.arrange(g1, g2, nrow = 2) # thats quite interesting...
# 
# # plot for a canton that did it late  
# g3 = ggplot(subset(merged, geoRegion == "BL"), aes(datum, entries,  color = geoRegion)) +
#   geom_line(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Entries Basel Land") +
#   theme_bw()
# g4 = ggplot(subset(merged, geoRegion == "BL"), aes(datum, facialCover,  color = geoRegion)) +
#   geom_line(size = 1, alpha = 0.4) +
#   labs(x = "Datum") +
#   labs(y = "Mask Policy Basel Land") +
#   theme_bw()
# grid.arrange(g3, g4, nrow = 2) # thats quite interesting...
# 
# ## Compare cancel publich events with facial coverings!!! 
# g5 = ggplot(merged, aes(x = datum, y = facialCover, color = geoRegion)) +
#   geom_line() +
#   facet_wrap( ~ geoRegion)
# g6 = ggplot(merged, aes(x = datum, y = cancEvents, color = geoRegion)) +
#   geom_line() +
#   facet_wrap( ~ geoRegion)
# grid.arrange(g5, g6, nrow = 2)
# # seems problematic: compare more carefully: the canc Events was somewhat later!
# comp = merged %>%
#   group_by(geoRegion) %>%
#   arrange(facialCover) %>%
#   filter(datum >= "2020-07-22") %>%
#   filter(facialCover == 3) %>%
#   filter(row_number() == 1)
# 
# ordered = merged %>%
#   group_by(geoRegion) %>%
#   arrange(facialCover) %>%
#   filter(facialCover == 3) %>%
#   filter(row_number() == 1 | row_number() == n())
# # gives us a table with the starting and ending day of facialCover == 3 for every canton
# # ending day is the same for all the cantons so just take starting day
# ordered = merged %>%
#   group_by(geoRegion) %>%
#   arrange(facialCover) %>%
#   filter(facialCover == 3) %>%
#   filter(row_number() == 1)
# 
# ### Plot that information: there should be sufficient differences!!
# ggplot() +
#   geom_segment(
#     aes(x = min(ordered$datum) - 10
#         , xend = max(ordered$datum) + 20
#         , y = 0
#         , yend = 0)
#   ) +
#   geom_linerange(
#     aes(x = ordered$datum
#         , ymin = 0
#         , ymax = 0.5)
#     , col = "red") +
#   xlab("Date") +
#   theme_minimal() +
#   theme(axis.text.y = element_blank()
#         , axis.title.y = element_blank()
#         , panel.grid.major.y = element_blank()
#         , panel.grid.minor.y = element_blank()
#   ) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%Y\n%b-%d")
# # 
# # #Plot of tau.hat per canton over time
# ggsave(".\\Plots\\tau_time.pdf",
#        width = 15, height = 10)
# ggplot(cate.df, aes(x = datum, y = median_R_mean)) +
#   geom_line(color = "firebrick", size = 1) +
#   geom_ribbon(data = merged, aes(ymin = median_R_lowHPD, ymax = median_R_highHPD), 
#               alpha = 0.3) +
#   facet_wrap( ~ geoRegion) +
#   coord_cartesian(ylim = c(0, 6)) +
#   geom_segment(x = as.Date("2020-08-19"), y = 1, xend = as.Date("2020-10-19"), yend = 1, linetype = 2) +
#   scale_x_date(breaks = "1 month", date_labels = "%b %y") +
#   labs(x = "", y = "Effective Reproductive Number", color = "Cantons") +
#   theme_minimal() +
#   theme(text = element_text(size = 20, family = "CM Roman"), 
#         panel.grid = element_line(linetype = 2, size = 0.25),
#         panel.grid.minor = element_line(linetype = 2, size = 0.25),
#         panel.border = element_blank())
# dev.off()
# 
# #### Moving average of the cate for easier visual inspection
# nums = c("datum", "date")
# cate.df.ma = cbind(cate.df, roll = rollmeanr(cate.df[,!names(cate.df) %in% nums, drop = F], 7, fill = NA))
# 
# #### That allows us to plot the treatment effects per canton over time
# plot(cate.df$AG, type ="l")
# lines(cate.df.ma$datum, cate.df.ma$roll.AG, type = "l", col = "red")
# # 
