# Load and Clean Attribute and Water Quality Data 

import_attribute_micro <- function() {
  ###Attribute data for each site
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/Pine_attributes.csv"
  
  skip = 1
  
  attributes <- importGitHub(url, skip)
  
  rm(url, skip)
  
  describe(attributes)
  
  
  ###Water Quality Measures for entire system
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/92_LittlePine_Cont.csv"
  
  skip = 3
  
  micro <- importGitHub(url, skip)
  
  colnames(micro) <-  c("DateTime", 
                        "O2Dis", 
                        "pH", 
                        "SpCond", 
                        "Turbidity", 
                        "WaterTemp")
  
  rm(url, skip)
  
  #Clean Date/Time
  temp = t(as.data.frame(strsplit(as.character(micro$DateTime),' ')))
  rownames(temp)=NULL
  micro$Date <- as.Date(temp[,1], 
                        format = "%m/%d/%Y")
  micro$Time <- temp[,2]
  rm(temp)
  
  micro$DateTime <- as.POSIXct(paste(micro$Date, 
                                     micro$Time), 
                               format = "%Y-%m-%d %H:%M")
  
  #Remove redundent variables
  micro$Date <- NULL
  micro$Time <- NULL
  
  summary(micro)
  
  return(list(attributes,micro))
}
