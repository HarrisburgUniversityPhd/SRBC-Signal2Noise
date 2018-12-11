# Load and Clean Fish Data 

import_fish <- function() {
  
  ###Fish Counts
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/AllPineCreekFish.csv"
  
  skip <- 0
  
  fishCounts <- importGitHub(url, skip)
  rm(url, skip)
  
  describe(fishCounts)
  
  #Fix DateTime Stamp from import
  temp = t(as.data.frame(strsplit(as.character(fishCounts$Time),' ')))
  rownames(temp)=NULL
  fishCounts$Time <- temp
  rm(temp)
  
  fishCounts$Date <- as.Date(fishCounts$Date, format = "%m/%d/%Y")
  
  
  fishCounts$DateTime <- as.POSIXct(paste(fishCounts$Date, fishCounts$Time), format = "%Y-%m-%d %H:%M")
  
  #Convert fish names to all lowercase to avoid duplicates & some other data cleanup.
  fishCounts$Fish <- tolower(fishCounts$Fish)
  fishCounts$Fish <- gsub(" ", "_", fishCounts$Fish)
  fishCounts$Fish <- gsub("\\(", "", fishCounts$Fish)
  fishCounts$Fish <- gsub("\\)", "", fishCounts$Fish)
  fishCounts$Fish <- factor(fishCounts$Fish)
  
  #Convert counts to numeric
  fishCounts$County <- as.numeric(fishCounts$County)
  
  #Remove redundent variables
  #fishCounts$Date <- NULL
  fishCounts$Time <- NULL
  
  
  #Drop unneeded variables dealing with how data was collected
  fishCounts$Activity <- NULL
  fishCounts$SRBC.Method <- NULL
  fishCounts$Gear <- NULL
  
  #Unstack
  if (!require('tidyr')) install.packages('tidyr', quiet=TRUE)
  library(tidyr)
  
  
  fishCounts2 <- spread(fishCounts, 
                        Fish, 
                        County,
                        fill = 0)
  
  fishCounts2 <- as.data.frame(fishCounts2)
  
  summary(fishCounts2)
  
  return(list(fishCounts,fishCounts2))
  
}
