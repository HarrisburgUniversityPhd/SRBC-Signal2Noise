# Load and Clean Macro Data 

import_macro <- function() {
  
  ###Macro Counts
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/AllPineCreekMacros.csv"
  skip = 0
  macroCounts <- importGitHub(url, skip)
  rm(url, skip)
  
  #Fix DateTime Stamp from import
  temp = t(as.data.frame(strsplit(as.character(macroCounts$Time),' ')))
  rownames(temp)=NULL
  macroCounts$Date <- as.Date(macroCounts$Date, 
                              format = "%m/%d/%Y")
  macroCounts$Time <- temp
  rm(temp)
  
  macroCounts$DateTime <- as.POSIXct(paste(macroCounts$Date, macroCounts$Time), format = "%Y-%m-%d %H:%M")
  
  #Convert fish names to all lowercase to avoid duplicates & some other data cleanup.
  macroCounts$Macrostemum <- tolower(macroCounts$Macrostemum)
  macroCounts$Activity <- NULL
  macroCounts$SRBC.Method <- NULL
  macroCounts$Gear <- NULL
  
  #Remove redundent variables
  macroCounts$Date <- NULL
  macroCounts$Time <- NULL
  
  
  #Unstack
  macroCounts2 <- spread(macroCounts, 
                         Macrostemum, 
                         Count, 
                         fill = 0)
  macroCounts2 <- as.data.frame(macroCounts2)
  attach(macroCounts2)
  
  summary(macroCounts2)
  
  rm(macroCounts)
  
  return(macroCounts2)
}


