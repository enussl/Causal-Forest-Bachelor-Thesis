#This function prepares the data by combining the data, cleaning and creating variables etc. 
#Note that one hash-tag is used to describe one task that is coded while two hash-tags before a comment
#imply that a sub-task of a task is described

#Empty environment
rm(list = ls())

#Set working directory where the data apart from the weather data are stored
setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish\\Data")

#Load packages
packages = c("dplyr", "DBI", "dbplyr", "httr", "rvest", "data.table", "Hmisc", "grf",
             "ggplot2", "janitor", "reshape","vistime", "dygraphs", "viridis",
             "gridExtra", "magrittr", "ggpubr", "Jmisc", "geodist")
for (pkg in packages){
  eval(bquote(library(.(pkg))))
}

#Function that prepares the data
data.prep = function(selection, differencing, start, finish) {
  
  #selection: make manual variable selection or not (yes/no)
  #differencing: difference variables or not (yes/no)
  #start: starting date of quasi-random period
  #end: ending date of quasi-random period
  
  #(I) READ IN CSV DATA
  
  listdf = list()
  listcsv = dir(pattern = "*.csv") #all csv files in directory
  for (k in 1:length(listcsv)) {
    listdf[[k]] = read.csv(listcsv[k])
  }
  
  #Name data sets
  spending     = listdf[[1]] 
  additional   = listdf[[2]]
  regions      = listdf[[3]]
  hospcap      = listdf[[4]]
  intcases     = listdf[[5]]
  intqua       = listdf[[6]]
  rval         = listdf[[7]]
  
  #(II) READ IN TEXT DATA

  #Population
  pop = read.table("Population.txt", sep = ",", header = T, encoding = "UTF-8")
  pop = pop[,c("Kanton", "Population", "age_group", "sex")]
  pop = pop[which(pop$Kanton != "" & pop$Kanton != "FL"),]
  pop$Kanton    = as.factor(pop$Kanton)
  pop$age_group = as.factor(pop$age_group)
  
  
  
  #Demographics
  demogr = read.table("Demographics.txt", sep = ",", header = T, encoding = "UTF-8")
  demogr = demogr[which(demogr$Canton != "CH"),]
  demogr = demogr[order(demogr$Canton),]  ## order by canton
  names(demogr)[names(demogr) == "Canton"] = "geoRegion"
  
  
  
  #Cantonal policy measures
  measures = read.table("KOFMeasures.txt", sep = ",", header = T, encoding = "UTF-8")
  measures$geo = toupper(measures$geo) # capital letters
  names(measures)[names(measures) == "geo"] = "geoRegion"
  names(measures)[names(measures) == "time"] = "datum"
  
  ##Reshape to wide
  measures_wide = reshape(data      = measures,
                          idvar     = c("datum", "geoRegion"),
                          v.names   = "value",
                          timevar   = "variable",
                          direction = "wide")
  measures_wide = measures_wide[order(measures_wide$geoRegion),] # order
  measures_wide = measures_wide[measures_wide$datum >= "2020-02-24",] # timeframe
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
  
  
  
  #Mobility Data
  mobility = read.table("MobilityKOF.txt", sep = ",", header = T, encoding = "UTF-8")
  names(mobility)[names(mobility) == "time"] = "datum"
  
  ##Reshape to wide
  mobility_wide = reshape(data      = mobility,
                          idvar     = "datum",
                          v.names   = "value",
                          timevar   = "group",
                          direction = "wide")
  remove(mobility)
  
  ##Remove useless variables (for the analysis)
  cols.dont.want.mob = c("variable", "value.abotyp_ga", "value.abotyp_halbtax_strecken__verbundabo",
                         "value.abotyp_kein_abo", "value.alter_15_29", "value.alter_30_64",
                         "value.alter_65_79", "value.auto_ja", "value.auto_nein",
                         "value.haushaltsgroesse_1_person", "value.haushaltsgroesse_2_personen",
                         "value.haushaltsgroesse_3__personen", "value.in_ausbildung",
                         "value.kanton_zuerich_ja", "value.kanton_zuerich_nein", "value.kinder_ja",
                         "value.kinder_nein", "value.nicht_erwerbstätig")
  
  ##Remove these variables if selection == yes
  if (selection == "yes") {
    
    mobility_wide = mobility_wide[,!names(mobility_wide) %in% cols.dont.want.mob, drop = F]
  }
  
  
  # (III) READ IN WEATHER DATA
  
  #Change working directory to weather data folder
  setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish\\Weather Data")
  
  #Read in data
  listdfwheather = list()
  listcsv = dir(pattern = "*.csv") #all csv files in directory
  for (k in 1:length(listcsv)) {
    listdfwheather[[k]] = read.csv(listcsv[k], sep = ";")
  }
  
  #Change date format for all data sets and keep variables and corresponding time frames
  listdfwheather = lapply(listdfwheather, function(x) {
    x = x[x$date >= "20200224",]
    x = t(t(x)) #Reverses order of observations via transposing twice
  })
  
  #Name data
  beso     = as.data.frame(rbind(listdfwheather[[2]],  listdfwheather[[1]]))
  bsblju   = as.data.frame(rbind(listdfwheather[[4]],  listdfwheather[[3]]))
  ge       = as.data.frame(rbind(listdfwheather[[6]],  listdfwheather[[5]]))
  gl       = as.data.frame(rbind(listdfwheather[[8]],  listdfwheather[[7]]))
  gr       = as.data.frame(rbind(listdfwheather[[10]], listdfwheather[[9]]))
  lu       = as.data.frame(rbind(listdfwheather[[12]], listdfwheather[[11]]))
  nefr     = as.data.frame(rbind(listdfwheather[[14]], listdfwheather[[13]]))
  ownw     = as.data.frame(rbind(listdfwheather[[16]], listdfwheather[[15]]))
  sgaraitg = as.data.frame(rbind(listdfwheather[[18]], listdfwheather[[17]]))
  ti       = as.data.frame(rbind(listdfwheather[[20]], listdfwheather[[19]]))
  ursz     = as.data.frame(rbind(listdfwheather[[22]], listdfwheather[[21]]))
  vd       = as.data.frame(rbind(listdfwheather[[24]], listdfwheather[[23]]))
  vs       = as.data.frame(rbind(listdfwheather[[26]], listdfwheather[[25]]))
  zhagshzg = as.data.frame(rbind(listdfwheather[[28]], listdfwheather[[27]]))
  
  #Create master data set
  weather = rbind(beso, bsblju, ge, gl, gr, lu, nefr, ownw, sgaraitg, ti, ursz, vd, vs, zhagshzg)
  remove(beso, bsblju, ge, gl, gr, lu, nefr, ownw, sgaraitg, ti, ursz, vd, vs, zhagshzg, listdfwheather)
  
  #Clean up weather data and match canton to weather station
  weather$date = format(as.Date(weather$date, format = "%Y%m%d"), "%Y-%m-%d")
  names(weather)[names(weather) == "date"] = "datum"
  names(weather)[names(weather) == "station.location"] = "geoRegion"
  weather["geoRegion"][weather["geoRegion"] == "BER"] = "BE"
  weather["geoRegion"][weather["geoRegion"] == "BAS"] = "BL"
  weather["geoRegion"][weather["geoRegion"] == "GVE"] = "GE"
  weather["geoRegion"][weather["geoRegion"] == "ELM"] = "GL"
  weather["geoRegion"][weather["geoRegion"] == "DAV"] = "GR"
  weather["geoRegion"][weather["geoRegion"] == "LUZ"] = "LU"
  weather["geoRegion"][weather["geoRegion"] == "NEU"] = "NE"
  weather["geoRegion"][weather["geoRegion"] == "ENG"] = "OW"
  weather["geoRegion"][weather["geoRegion"] == "STG"] = "SG"
  weather["geoRegion"][weather["geoRegion"] == "LUG"] = "TI"
  weather["geoRegion"][weather["geoRegion"] == "ALT"] = "UR"
  weather["geoRegion"][weather["geoRegion"] == "PAY"] = "VD"
  weather["geoRegion"][weather["geoRegion"] == "SIO"] = "VS"
  weather["geoRegion"][weather["geoRegion"] == "SMA"] = "ZH"
  
  #Order along geoRegion
  weather = weather[order(weather$geoRegion),]
  
  #Rename geoRegion for merging to weatherind (indicator that allows merging later)
  names(weather)[names(weather) == "geoRegion"] = "weatherind"
  
  #Change back working directory
  setwd("C:\\Users\\eminu\\Desktop\\COVID-19 Finish\\Data")
  
  
  #(IV) DATA PREPARATION
  
  #Spending
  spending = spending[,c("Date", "Canton", "Amount.CHF", "Number.of.transactions"),]
  names(spending)[names(spending) == "Canton"] = "geoRegion"
  names(spending)[names(spending) == "Date"] = "datum"
  spending = spending[which(spending$geoRegion != "9999"),] # is that the entirety of switzerland?
  spending = spending[order(spending$geoRegion),]
  spending = spending[spending$datum >= "2020-02-24",]
  
  ##Compute growth rates
  spending = spending %>%
    group_by(geoRegion) %>%
    mutate(growth.Amount.CHF = (Amount.CHF - lag(Amount.CHF))/lag(Amount.CHF),
           growth.Number.of.transactions = (Number.of.transactions - lag(Number.of.transactions))/lag(Number.of.transactions)) %>%
    ungroup()
  
  
  #Population: determine percentage of over 80 year olds
  ##Determine old and young groups
  pop$old = ifelse(pop$age_group %in% c("0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79"), 0, 1)
  pop$old = as.factor(pop$old)
  
  ##Group by canton and age to compute the percentage
  pop_sum = pop %>%
    group_by(Kanton) %>%
    mutate(pop_sum = sum(Population)) %>%
    ungroup() %>%
    group_by(Kanton, old) %>%
    mutate(pop_sum_age = sum(Population)) %>%
    mutate(percentage_age = ifelse(old == 1, pop_sum_age / pop_sum, 0)) %>%
    select(Kanton, old, pop_sum_age, pop_sum, percentage_age) %>%
    ungroup()
  
  ##Make data set via selecting non-zero percentages
  pop_sum = as.data.frame(pop_sum)
  pop_sum = pop_sum[which(pop_sum$percentage_age != 0),]
  
  ##Only keep percentage variable 
  pop_sum = pop_sum[,c("percentage_age", "Kanton")]
  names(pop_sum)[names(pop_sum) == "Kanton"] = "geoRegion"
  pop_sum = pop_sum[!duplicated(pop_sum$geoRegion),] ##Select first value per canton
  
  
  
  #Regions, Demographics and Population
  ##Pick out canton-specific observations
  data = regions[which(regions$geoRegion != "CH" & regions$geoRegion != "CHFL" & regions$geoRegion != "FL"),]
  
  ##Delete variables we do not need 
  cols.dont.want.dat1 = c("timeframe_7d", "offset_last7d", "offset_Phase2", "offset_Phase2b",
                          "offset_Phase3", "offset_Phase4", "offset_Phase5", "offset_last14d",
                          "offset_last28d", "offset_vacc_info", "timeframe_14d",
                          "timeframe_28d", "timeframe_14d", "timeframe_all", "timeframe_phase2",
                          "timeframe_phase2b", "timeframe_phase3", "timeframe_phase4",
                          "timeframe_phase5", "timeframe_vacc_info", "sumTotal_Phase2",
                          "sumTotal_Phase2b", "sumTotal_Phase3", "sumTotal_vacc_info",
                          "sumTotal_Phase4", "sumTotal_Phase5", "version", "type", "datum_unit",
                          "inzsumTotal_Phase2", "inzsumTotal_Phase2b", "inzsumTotal_Phase3",
                          "inzsumTotal_Phase4", "inzsumTotal_Phase5", "entries_diff_last_age",
                          "type_variant", "Population") #Population already contained in additional data
  
  ##Delete columns if selection == yes
  if (selection == "yes") {
    
    data = data[,!names(data) %in% cols.dont.want.dat1, drop = F]
  } 

  
  #Additional data from Data hub
  ##Rename for correspondence
  names(additional)[names(additional) == "key_local"] = "geoRegion"
  
  ##Drop rows where the entirety of Switzerland is concerned and order along canton
  additional = additional[order(additional$geoRegion),]
  additional = additional[which(additional$geoRegion != ""),]
  
  #Date adjustment
  additional = additional[additional$date >= "2020-02-24",]
  names(additional)[names(additional) == "date"] = "datum"
  
  ##Columns we never want; wrong information and useless info; deleted independently of selection
  cols.dont.want.add = c("id", "administrative_area_level", "administrative_area_level_1", "administrative_area_level_3",
                         "iso_alpha_3", "iso_alpha_2", "iso_numeric", "iso_currency", "key_google_mobility",
                         "key_apple_mobility", "key_jhu_csse", "key_nuts", "key_gadm",
                         "latitude", "longitude", "administrative_area_level_2", "school_closing",
                         "workplace_closing", "cancel_events", "gatherings_restrictions",
                         "transport_closing", "stay_home_restrictions", "internal_movement_restrictions",
                         "international_movement_restrictions", "information_campaigns",
                         "testing_policy", "contact_tracing", "facial_coverings", "vaccination_policy",
                         "elderly_people_protection") 
  
 
  ##Delete in either case of selection
  additional = additional[,!names(additional) %in% cols.dont.want.add, drop = F] 


  
  #R Value Data
  ##Remove geographical regions that are not cantons
  rval = rval[which(rval$geoRegion != "grR Central Switzerland" & 
                      rval$geoRegion != "grR Lake Geneva Region" & 
                      rval$geoRegion != "grR Zurich" &
                      rval$geoRegion != "seR Sentinella 1" &
                      rval$geoRegion != "seR Sentinella 4" &
                      rval$geoRegion != "grR Eastern Switzerland" &
                      rval$geoRegion != "grR Northwestern Switzerland" &
                      rval$geoRegion != "seR Sentinella 2" &
                      rval$geoRegion != "seR Sentinella 5" &
                      rval$geoRegion != "grR Espace Mittelland" &
                      rval$geoRegion != "grR Ticino" &
                      rval$geoRegion != "seR Sentinella 3" &
                      rval$geoRegion != "seR Sentinella 6" &
                      rval$geoRegion != "CH" &
                      rval$geoRegion != "FL"),]
  
  ##Date adjustment
  rval = rval[rval$date >= "2020-02-24",]
  
  ##Rename
  names(rval)[names(rval) == "date"] = "datum"
  
  ##Remove useless columns; remove in either case
  cols.dont.want.rval = c("timeframe_14d", "timeframe_28d", "timeframe_phase2",
                          "timeframe_phase2b", "timeframe_phase3", "timeframe_phase4",
                          "timeframe_phase5", "timeframe_all", "type", "version")
  #Remove in either case
  rval = rval[,!names(rval) %in% cols.dont.want.rval, drop = F]
    

  
  #Hospital capacity data
  ##Only keep cantonal data
  hospcap = hospcap[which(hospcap$geoRegion != "CH" & hospcap$geoRegion != "FL"),] 
  
  ##Delete weird duplicate observations; bug from BAG
  hospcap = hospcap[c(1:16042),]
  
  ##Order along geoRegion
  hospcap = hospcap[order(hospcap$geoRegion),]
  
  
  ##Remove uneless columns; remove in either case
  cols.dont.want.cap = c("timeframe_14d", "timeframe_28d", "timeframe_phase2",
                         "timeframe_phase2b", "timeframe_phase3", "timeframe_phase4",               
                         "timeframe_phase5", "timeframe_all", "version", "type")
  
  ##Remove in either case
  hospcap = hospcap[,!names(hospcap) %in% cols.dont.want.cap, drop = F]
    
  
 
  #International cases data (not used; might be useful for later analysis (paper))
  ##Date adjustment
  intcases = intcases[intcases$date >= "2020-02-24",]
  
  ## Select useless columns; remove in either case
  cols.dont.want.intcas = c("source", "timeframe_14d", "timeframe_28d", "timeframe_phase2",
                            "timeframe_phase2b", "timeframe_phase3", "timeframe_phase4",
                            "timeframe_phase5", "timeframe_all", "type", "geoLevel", "version")

  ##Removed in either case
  intcases = intcases[,!names(intcases) %in% cols.dont.want.intcas, drop = F]
 
  #International quarantine data (not used; might be useful for later analysis (paper))
  #### Select useless columns; remove in either case
  cols.dont.want.qua = c("geoLevel", "type", "version")
  
  ##Remove in either case
  intqua = intqua[,!names(intqua) %in% cols.dont.want.qua, drop = F]
    
 
  
  #(V) MERGE DATA SETS
  
  #Start merging
  merged1  = merge(data, pop_sum, by = "geoRegion") #Add population
  merged2  = merge(merged1, demogr, by = "geoRegion") #Add demographics
  merged3  = merge(merged2, additional) #Add additional
  merged4  = merge(merged3, rval) #Add R values
  merged5  = merge(merged4, measures_wide) #Add policy stuff
  merged6  = merge(merged5, spending) #Add consumption
  merged7  = merge(merged6, mobility_wide) #Add mobility

  ##Rename
  merged = merged7 
  
  
  
  #Construct indicator on which we can merge the weather data
  ##Construct a merge indicator as the weather data comes from less stations than cantons
  ##meaning that we have a many to many mapping where the weather data from one canton can
  ##be merged to multiple cantons. geoRegion in weather has to be called weatherind
  merged$weatherind = ifelse(merged$geoRegion %in% c("BE", "SO"), "BE", 
                             ifelse(merged$geoRegion %in% c("BL", "BS", "JU"), "BL",
                                    ifelse(merged$geoRegion %in% c("NE", "FR"), "NE",
                                           ifelse(merged$geoRegion %in% c("OW", "NW"), "OW",
                                                  ifelse(merged$geoRegion %in% c("SG", "AR", "AI", "TG"), "SG",
                                                         ifelse(merged$geoRegion %in% c("UR", "SZ"), "UR",
                                                                ifelse(merged$geoRegion %in% c("ZH", "AG", "SH", "ZG"), "ZH", merged$geoRegion)))))))
  ##Merge 
  merged8 = merge(merged, weather, by = c("datum", "weatherind"))
  
  ##Rename
  merged = merged8
  
  #Remove old merges and old data sets for clarity and to accelerate the computation
  remove(merged1, merged2, merged3, merged4, merged5, merged6, merged7, merged8)
  remove(additional, data, demogr, hospcap, intcases, intqua, listdf, measures, measures_wide, mobility_wide,
         mobind_wide, pop, pop_sum, regions, rval, spending, weather)
  
  ##Order along geoRegion
  merged = merged[order(merged$geoRegion),]
  
  #Create holidays indicator where we look up the weather data from the official cantonal authorities
  merged$ferien = ifelse(merged$datum >= "2020-09-28" & merged$datum <= "2020-10-09" & merged$geoRegion == "AG", 1,
                         ifelse(merged$datum >= "2020-10-05" & merged$datum <= "2020-10-16" & merged$geoRegion == "AR", 1,
                                ifelse(merged$datum >= "2020-10-03" & merged$datum <= "2020-10-18" & merged$geoRegion == "AI", 1, 
                                       ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-10-11" & merged$geoRegion == "BL", 1,
                                              ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-10-10" & merged$geoRegion == "BS", 1,
                                                     ifelse(merged$datum >= "2020-09-19" & merged$datum <= "2020-10-11" & merged$geoRegion == "BE", 1, 
                                                            ifelse(merged$datum >= "2020-10-19" & merged$datum <= "2020-10-30" & merged$geoRegion == "FR", 1,
                                                                   ifelse(merged$datum >= "2020-10-19" & merged$datum <= "2020-10-23" & merged$geoRegion == "GE", 1,
                                                                          ifelse(merged$datum >= "2020-10-03" & merged$datum <= "2020-10-18" & merged$geoRegion == "GL", 1,
                                                                                 ifelse(merged$datum >= "2020-10-12" & merged$datum <= "2020-10-25" & merged$geoRegion == "GR", 1,
                                                                                        ifelse(merged$datum >= "2020-10-12" & merged$datum <= "2020-10-23" & merged$geoRegion == "JU", 1,
                                                                                               ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-10-11" & merged$geoRegion == "LU", 1,
                                                                                                      ifelse(merged$datum >= "2020-10-05" & merged$datum <= "2020-10-16" & merged$geoRegion == "NE", 1,
                                                                                                             ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-09-26" & merged$geoRegion == "NW", 1,
                                                                                                                    ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-10-11" & merged$geoRegion == "OW", 1,
                                                                                                                           ifelse(merged$datum >= "2020-09-27" & merged$datum <= "2020-10-18" & merged$geoRegion == "SG", 1,
                                                                                                                                  ifelse(merged$datum >= "2020-09-26" & merged$datum <= "2020-10-17" & merged$geoRegion == "SH", 1,
                                                                                                                                         ifelse(merged$datum >= "2020-09-28" & merged$datum <= "2020-10-09" & merged$geoRegion == "SZ", 1,
                                                                                                                                                ifelse(merged$datum >= "2020-09-28" & merged$datum <= "2020-10-16" & merged$geoRegion == "SH", 1,
                                                                                                                                                       ifelse(merged$datum >= "2020-10-31" & merged$datum <= "2020-11-08" & merged$geoRegion == "TI", 1,
                                                                                                                                                              ifelse(merged$datum >= "2020-10-05" & merged$datum <= "2020-10-18" & merged$geoRegion == "TG", 1,
                                                                                                                                                                     ifelse(merged$datum >= "2020-10-03" & merged$datum <= "2020-10-18" & merged$geoRegion == "UR", 1,
                                                                                                                                                                            ifelse(merged$datum >= "2020-10-10" & merged$datum <= "2020-10-25" & merged$geoRegion == "VD", 1,
                                                                                                                                                                                   ifelse(merged$datum >= "2020-10-12" & merged$datum <= "2020-10-23" & merged$geoRegion == "VS", 1,
                                                                                                                                                                                          ifelse(merged$datum >= "2020-10-05" & merged$datum <= "2020-10-17" & merged$geoRegion == "ZH", 1,
                                                                                                                                                                                                 ifelse(merged$datum >= "2020-10-03" & merged$datum <= "2020-10-18" & merged$geoRegion == "ZG", 1, 0))))))))))))))))))))))))))
  
  #Create weekly indicator
  merged = merged %>%
  mutate(datum = as.Date(datum)) %>%
  mutate(week = cut.Date(datum, breaks = "week", labels = FALSE)) %>%
  arrange(geoRegion)
  
  #Reordering (somewhat useless but nevertheless)
  merged = merged %>%
    relocate(weatherind, .after = ferien) %>%
    relocate(median_R_mean, .after = entries_diff_last) %>%
    relocate(median_R_highHPD, .after = median_R_mean) %>%
    relocate(median_R_lowHPD, .after = median_R_highHPD) %>%
    relocate(median_R_mean_mean7d, .after = median_R_lowHPD) 
  
  
  
  #(VI) DATATYPES OF X, Y ,W: CHECKING AND ADJUSTING 
  #AS WELL AS HANDLING CATEGORICAL VARIABLES
  
  #Checking and changing datatypes
  merged$datum = as.Date(merged$datum)
  merged$Canton = as.numeric(as.factor(merged$geoRegion))
  
  #Converting characters to numeric 
  ##Character variables
  char = c("gre000d0", "hto000d0", "nto000d0", "prestad0", "rre150d0", "sre000d0",
           "tre200d0", "tre200dn", "tre200dx", "ure200d0")
  
  ##Apply conversion function to those variables
  merged[,char] = lapply(merged[,char], function(x){
    x = as.numeric(as.character(x))
  })
  
  #Order variables so that we have Y, W, X
  merged = merged %>% select(facialCover, everything()) 
  merged = merged %>% select(entries, everything()) 
  merged = merged %>% select(datum, everything()) 
  
  
  
  
  #(VII) DIFFERENCING, FEATURE SELECTION, TIME HORIZON, LAGGED VALUES
  
  ##Target variable; we end up using the lagged (negative lag so leaded) versions of 
  ##the logarithmic R-values
  merged = merged %>% 
    group_by(geoRegion) %>%
    mutate(entries_1d = entries - lag(entries),
           inz_entries_1d = inz_entries - lag(inz_entries),
           log_entries_1d = log(entries) - log(lag(entries)),
           log_median_R_mean = log(median_R_mean),
           log_median_R_mean_lead_14 = lead(log_median_R_mean, 14),
           log_median_R_mean_lead_10 = lead(log_median_R_mean, 10),
           log_median_R_mean_lead_8 = lead(log_median_R_mean, 8),
           log_median_R_mean_lead_7 = lead(log_median_R_mean, 7), 
           log_median_R_mean_lead_6 = lead(log_median_R_mean, 6), 
           log_median_R_mean_lead_5 = lead(log_median_R_mean, 5), 
           log_median_R_mean_lead_4 = lead(log_median_R_mean, 4), 
           log_median_R_mean_lead_3 = lead(log_median_R_mean, 3), 
           log_median_R_mean_lead_2 = lead(log_median_R_mean, 2), 
           log_median_R_mean_lead_1 = lead(log_median_R_mean, 1)) %>% 
    relocate(entries_1d, .after = entries) %>%
    relocate(log_entries_1d, .after = entries_1d) %>%
    relocate(log_median_R_mean, .after = datum) %>%
    relocate(geoRegion, .after = datum) %>%
    ungroup()
  
  
  #Difference numerical variables that appear non-stationary if differencing == yes
  #which is not used in the paper as its not necessary compared to linear models for example
  if (differencing == "yes") {
    
    merged = merged %>%
      group_by(geoRegion) %>%
      mutate(deaths_1d = deaths - lag(deaths),
             recovered_1d = recovered - lag(recovered),
             tests_1d = tests - lag(tests),
             vaccines_1d = vaccines - lag(vaccines),
             people_vaccinated_1d = people_vaccinated - lag(people_vaccinated),
             people_fully_vaccinated_1d = people_fully_vaccinated - lag(people_fully_vaccinated),
             hosp_1d = hosp - lag(hosp),
             government_response_index_1d = government_response_index - lag(government_response_index),
             stringency_index_1d = stringency_index - lag(stringency_index),
             containment_health_index_1d = containment_health_index - lag(containment_health_index),
             economic_support_index_1d = economic_support_index - lag(economic_support_index),
             kofstring_1d = kofstring - lag(kofstring),
             kofstring_plus_1d = kofstring_plus - lag(kofstring_plus),
             Amount.CHF_1d = Amount.CHF - lag(Amount.CHF),
             Number.of.transactions_1d = Number.of.transactions - lag(Number.of.transactions),
             value.total_1d = value.total - lag(value.total),
             tre200d0_1d = tre200d0 - lag(tre200d0),
             ure200d0_1d = ure200d0 - lag(ure200d0),
             sre000d0_1d = sre000d0 - lag(sre000d0)) %>%
      ungroup()
    
  }
  
  #Frame of interest regarding time
  merged = merged %>%
    filter(datum >= start & datum <= finish) 
  
  #Construct treatment vector as discussed in paper
  merged$mask_treat = ifelse(merged$facialCover == 3, 1, 0)
  
  #Work out clever labeling of cantons such that the distances are represented
  ##The coordinates are the main cities of the cantons
  longlat = data.frame(longitude = c(683354, 600670, 665892, 691867, 692370, 661499, 670604, 723766, 681646,
                                     578929, 607573, 611220, 622338, 689722, 739022, 749492, 746284,
                                     759662, 645731, 709835, 722340, 538291, 593997, 561256, 500532,
                                     592827),
                       latitude = c(247353, 199655, 211591, 192999, 208488, 194193, 201070, 211267, 224472,
                                    183935, 228576, 267503, 259290, 283562, 249957, 244381, 254335,
                                    190702, 249290, 268281, 116829, 152330, 120194, 204454, 117325,
                                    246044),
                       name = c("ZH", "BE", "LU", "UR", "SZ", "OW",
                                "NW", "GL", "ZG", "FR", "SO",
                                "BS", "BL", "SH", "AR", "AI",
                                "SG", "GR", "AG", "TG", "TI",
                                "VD", "VS", "NE", "GE", "JU"))
  rownames(longlat) = longlat$name
  
  ##Compute distance matrix 
  dist = geodist(longlat, measure = "cheap")
  
  ##MDS projection of distance matrix onto one dimension
  mds = dist %>%
    cmdscale(k = 1) 
  mds = as.data.frame(mds)
  
  ##Rename
  colnames(mds) = c("Dim1")
  rownames(mds) = rownames(longlat)
  mds$geoRegion = rownames(mds)
  
  ##Order the values of the indicator according to the MDS scores for 1:26
  mds = mds %>%
    mutate(Canton_2 = ceil(10*scale(Dim1))) %>%
    arrange(Canton_2) %>%
    mutate(Canton_3 = seq(1:26)) %>%
    select(Canton_2, Canton_3, geoRegion)
  
  ##Merge with master data set
  merged = merge(merged, mds)
  
  ##Remove old data sets
  remove(longlat, mds, dist)
  
  #Remove variables that are of absolutely no interest even if selection == "no"
  if (selection == "no") {
    
    merged = merged %>%
      mutate(timeframe_7d       = NULL,
             timeframe_14d      = NULL,
             timeframe_28d      = NULL,
             timeframe_phase2   = NULL,
             timeframe_phase2b  = NULL,
             timeframe_phase3   = NULL,
             timeframe_vacc_info= NULL,
             timeframe_phase4   = NULL,
             timeframe_phase5   = NULL,
             timeframe_all      = NULL,
             pop                = NULL,
             type               = NULL,
             type_variant       = NULL,
             version            = NULL,
             datum_unit         = NULL,
             variable           = NULL,
             weatherind         = NULL,
             Canton             = NULL,
             Canton_2           = NULL,
             vaccines           = NULL,
             people_fully_vaccinated = NULL,
             people_vaccinated  = NULL,
             sumTotal_last7d     = NULL,
             sumTotal_last14d   = NULL,
             sumTotal_last28d   = NULL,
             sumTotal_Phase2    = NULL,
             sumTotal_Phase2b   = NULL,
             sumTotal_Phase3    = NULL,
             sumTotal_vacc_info = NULL,
             sumTotal_Phase4    = NULL,
             sumTotal_Phase5    = NULL,
             inzsumTotal_Phase3 = NULL,
             inzsumTotal_Phase4 = NULL,
             inzsumTotal_Phase5 = NULL,
             inzsumTotal_last7d = NULL,
             inzsumTotal_last14d= NULL,
             inzsumTotal_last28d= NULL)
             
  }
  
  #Write master data set to csv
  write.csv(merged,"C:\\Users\\eminu\\Desktop\\COVID-19 Finish\\Data_Covid.csv", row.names = FALSE)
  
  #Return data so we can use it immediately
  return(merged)
}