# title: "Metrics Combined"
# author: "Emily Wefelmeyer, Ziyaun Huang, Pranita Patil, Sridhar Ravula"
# date: "November 20, 2018"

# Community Metrics calculations

Metrics_fish <- function(fishCounts2) {
  temp <- fishCounts2[,-(1:4)]
  r <- nrow(temp)
  c <- ncol(temp)
  
  
  
  #Pielou metric
  if (!require('vegan')) install.packages('vegan', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(vegan)
  
  if (!require('gdata')) install.packages('gdata', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(gdata)
  
  H <- diversity(temp)
  S <- specnumber(temp)
  fishCounts2$pielou <- H/log(S)
  
  #Species Count
  
  fishCounts2$speciesCount <- rowSums(temp > 0)
  
  
  
  #Hill's N1
  
  fishCounts2$Hills_N1 <- exp(H)
  rm(H, S)
  
  
  
  #Hill's N2
  
  if (!require('analogue')) install.packages('analogue', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(analogue)
  
  fishCounts2$Hills_N2 <- n2(temp, "sites")
  
  
  #Margalef 
  #Calculate Margalef
  
  if (!require('benthos')) install.packages('benthos', quiet=TRUE, repos = "http://cran.us.r-project.org")
  library(benthos)
  
  if (!require('lubridate')) install.packages('lubridate', quiet=TRUE)
  library(lubridate)
  
  if (!require('dplyr')) install.packages('dplyr', quiet=TRUE)
  library(dplyr)
  
  for (i in 1:r){
    fishCounts2$margalef[i] <- margalef(.data = temp[i,], 
                                        taxon = colnames(temp), 
                                        count = temp[i,])
  }  
  
  return(fishCounts2)
  
}



