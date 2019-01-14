# title: "Metrics Combined"
# author: "Emily Wefelmeyer, Ziyaun Huang, Pranita Patil, Sridhar Ravula"
# date: "November 20, 2018"

# Community Metrics calculations

Metrics_macro <- function(macroCounts2) {
  temp <- macroCounts2[,-(1:4)]
  r <- nrow(temp)
  c <- ncol(temp)
  
  #Pielou metric
  
  if (!require('vegan')) install.packages('vegan', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(vegan)
  
  if (!require('gdata')) install.packages('gdata', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(gdata)
  
  H <- diversity(temp)
  S <- specnumber(temp)
  macroCounts2$pielou_macro <- H/log(S)
  
  #Species Count
  
  macroCounts2$speciesCount_macro <- rowSums(temp > 0)
  
  
  
  #Hill's N1
  
  macroCounts2$Hills_N1_macro <- exp(H)
  rm(H, S)
  
  
  
  #Hill's N2
  
  if (!require('analogue')) install.packages('analogue', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(analogue)
  
  macroCounts2$Hills_N2_macro <- n2(temp, "sites")
  
  
  #Margalef 
  #Calculate Margalef
  
  if (!require('benthos')) install.packages('benthos', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(benthos)
  
  if (!require('lubridate')) install.packages('lubridate', quiet=TRUE)
  library(lubridate)
  
  if (!require('dplyr')) install.packages('dplyr', quiet=TRUE)
  library(dplyr)

  for (i in 1:r){
    macroCounts2$margalef_macro[i] <- margalef(.data = temp[i, ], taxon = colnames(temp), count = temp[i,])
  }
  
  return(macroCounts2)
  
}



