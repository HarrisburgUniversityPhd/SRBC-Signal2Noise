Corr_SRBC <- function(allData) {
  
  library(Hmisc)
  
  # Get all combined data
  str(allData)
  
  # find factor data type in all data
  ID_name <- names(Filter(is.factor, allData))
  
  #factor_data <- Filter(is.factor, allData)
  
  # convert factor data into numeric
  allData1 <- allData
  indx <- sapply(allData1, is.factor)
  allData1[indx] <- lapply(allData1[indx], function(x) as.numeric(as.factor(x)))
  
  # remove some of the variables such as Station ID, Alias Name, Latitude, etc.
  allData1$DateTime <- NULL
  allData1$Date <- NULL
  allData1[,ID_name[c(1:12)]] <- list(NULL)
  allData1[,c(1,326,327)] <- NULL # remove station ID, "Station.ID", "Latitude", "Longitude"  
  
  str(allData1)
  #colnames(allData1)
  
  # convert all data to numeric to calculate correlation coefficients
  allData1 <- sapply(allData1, as.numeric)
  
  # remove count data for fish and macro
  allData2 <- allData1[,-c(40:94,100:319)]
  
  # calculate correlation matrix and significance levels
  corre_ent2 <- rcorr(as.matrix(allData2))
  
  cor_coef2 <- corre_ent2$r
  cor_p2 <- corre_ent2$P
  
  cor_coef2[cor_coef2 == "NaN"] = NA
  cor_p2[cor_p2 == "NaN"] = NA
  
  return(list(cor_coef2,cor_p2))
  
}