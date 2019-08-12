# Load and Clean Chemistry Data 

import_chemistry <- function() {
  
  
  ####Load chemistry data prior to 2018
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/AllPineCreekChemistry.csv"
  
  skip = 0
  
  colNames <- c("StationID",
                "StationName",
                "Date",
                "Time",
                "Parameter",
                "Results",
                "RDL")
  
  chemistry <- importGitHub(url, skip)
  
  colnames(chemistry) <- colNames
  
  rm(skip, url, colNames)
  
  ####Add additional chemistry data from 2018
  
  url <- "https://raw.githubusercontent.com/HarrisburgUniversityPhd/SRBC-Signal2Noise/master/Data/2018PineHU.csv"
  
  skip = 0
  
  colNames <- c("StationID",
                "StationName",
                "Date",
                "Time",
                "Parameter",
                "Results",
                "RDL")
  
  temp <- importGitHub(url, skip)
  colnames(temp) <- colNames
  rm(colNames, skip, url)
  
  #Combine Chemistry data frames
  chemistry <- rbind(chemistry, temp)
  
  
  
  rm(temp)
  
  
  ####Clean up chemistry data
  chemistry$RDL = NULL #Not going to be used 
  
  #Clean Results
  chemistry <- within(chemistry,
                      Results[Results == "Present Below Quantification Limit"] <- NA)
  
  chemistry <- within(chemistry,
                      Results[Results == "Present Below Quantitification Level"] <- NA)
  
  chemistry <- within(chemistry,
                      Results[Results == "Present Below Quantification Level"] <- NA)
  
  chemistry <- within(chemistry,
                      Results[Results == "Present Below Quanitification Level"] <- NA)
  
  chemistry <- within(chemistry,
                      Results[Results == "ND"] <- NA)
  
  chemistry$Results <- as.character(chemistry$Results)
  
  chemistry <- within(chemistry,
                      Results[Results == "4..08"] <- "4.08")
  
  
  chemistry$Results <- as.numeric(chemistry$Results)
  
  #####Clean parameters
  
  #Change from factors to characters
  chemistry$Parameter <- as.character(chemistry$Parameter)
  
  #Remove ","s
  chemistry$Parameter <- gsub(",", "", chemistry$Parameter)
  
  #Remove " - Field " from parameter names
  chemistry$Parameter <- gsub("- Field", "", chemistry$Parameter)
  
  #Remove "Total " from parameter names
  chemistry$Parameter <- gsub("Total", "", chemistry$Parameter)
  
  #Remove "TOT" from parameter names
  chemistry$Parameter <- gsub("TOT ", "", chemistry$Parameter)
  
  #Remove "T" from parameter names
  chemistry$Parameter <- gsub(" T ", " ", chemistry$Parameter)
  
  #Remove "D" from parameter names
  chemistry$Parameter <- gsub(" D ", " ", chemistry$Parameter)
  
  #Remove "Dissolved" from parameter names
  chemistry$Parameter <- gsub("Dissolved", "", chemistry$Parameter)
  
  #Remove spaces
  chemistry$Parameter <- gsub("  ", " ", chemistry$Parameter)
  
  #Remove "(None)" from pH
  chemistry$Parameter <- gsub("\\(None\\)", "", chemistry$Parameter)
  
  #Replace "Specific Con(umho/cm)" with "Specific Conductivity (umho/cm)"
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Specific Con (umho/cm)"] <- "Specific Conductivity (umho/cm)")
  
  #Replace "T Org Carbon" with "Carbon"
  chemistry$Parameter <- gsub("T Org Carbon", "Carbon", chemistry$Parameter)
  
  #Replace "Alkalinity Bivarbonate" with "Alkalinity"
  chemistry$Parameter <- gsub("Alkalinity Bicarbonate", "Alkalinity", chemistry$Parameter)
  
  #Replace " Oxygen" with "Oxygen"
  chemistry$Parameter <- gsub(" Oxygen", "Oxygen", chemistry$Parameter)
  
  #Replace "Organic N" with "Nitrogen"
  chemistry$Parameter <- gsub("Organic N", "Nitrogen", chemistry$Parameter)
  
  #Replace "N" with "Nitrogen"
  chemistry$Parameter <- gsub(" N ", "Nitrogen", chemistry$Parameter)

  #Replace "Nitrite-N" with "Nitrite"
  chemistry$Parameter <- gsub("Nitrite-N", "Nitrite", chemistry$Parameter)
  
    
  #Combine Nitrogen(mg/l) with Nitrogen (mg/l)
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Nitrogen(mg/l)"] <- "Nitrogen (mg/l)")
  
  #Replace "P Ortho Diss (mg/l)" with "Orthophospate (mg/l)
  chemistry <- within(chemistry,
                      Parameter[Parameter == "P Ortho Diss (mg/l)"] <- "Orthophosphate (mg/l)")
  
  
  
  #Change "Aluminum (mg/l)" to "Aluminum (ug/l)"
  chemistry$Results <- ifelse((chemistry$Parameter == "Aluminum (mg/l)" & 
                                 !is.na(chemistry$Results)), 
                              chemistry$Results * 1000,
                              chemistry$Results)
  
  
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Aluminum (mg/l)"] <- "Aluminum (ug/l)")
  
  #Change "Barium (mg/l)" to "Barium (ug/l)"
  chemistry$Results <- ifelse((chemistry$Parameter == "Barium (mg/l)" & 
                                 !is.na(chemistry$Results)), 
                              chemistry$Results * 1000,
                              chemistry$Results)
  
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Barium (mg/l)"] <- "Barium (ug/l)")
  
  
  #Change "Bromide (mg/l)" to "Bromide (ug/l)"
  chemistry$Results <- ifelse((chemistry$Parameter == "Bromide (mg/l)" & 
                                 !is.na(chemistry$Results)), 
                              chemistry$Results * 1000,
                              chemistry$Results)
  
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Bromide (mg/l)"] <- "Bromide (ug/l)")
  
  
  #Change "Iron (mg/l)" to "Iron (ug/l)"
  chemistry$Results <- ifelse((chemistry$Parameter == "Iron (mg/l)" & 
                                 !is.na(chemistry$Results)), 
                              chemistry$Results * 1000,
                              chemistry$Results)

  chemistry <- within(chemistry,
                      Parameter[Parameter == "Iron (mg/l)"] <- "Iron (ug/l)")
  
  
  #Change "Manganese (mg/l)" to "Manganese (ug/l)"
  chemistry$Results <- ifelse((chemistry$Parameter == "Manganese (mg/l)" & 
                                 !is.na(chemistry$Results)), 
                              chemistry$Results * 1000,
                              chemistry$Results)
  
  
  chemistry <- within(chemistry,
                      Parameter[Parameter == "Manganese (mg/l)"] <- "Manganese (ug/l)")
  
  chemistry$Parameter <- as.factor(chemistry$Parameter)
  
  chemistry <- unique(chemistry)
  
  #####Fix date/time stamp
  
  #Fix DateTime Stamp from import
  temp = t(as.data.frame(strsplit(as.character(chemistry$Time),' ')))
  rownames(temp)=NULL
  chemistry$Time <- temp
  rm(temp)
  
  chemistry$Date <- as.Date(chemistry$Date, format = "%m/%d/%Y")
  
  
  chemistry$DateTime <- as.POSIXct(paste(chemistry$Date,
                                         chemistry$Time), 
                                   format = "%Y-%m-%d %H:%M",
                                   tz = "UTC")
  
  
  #Remove redundent variables
  chemistry$Date <- NULL
  chemistry$Time <- NULL
  
  #Remove NAs
  chemistry <- na.omit(chemistry)
  
  #####Remove duplicate rows
  
  ##Remove duplicate rows
  #Idenfies duplicates in a new data frame
  duplicates <- chemistry[(duplicated(chemistry[c("StationID", "StationName", "Parameter", "DateTime")]) | duplicated(chemistry[c("StationID", "StationName", "Parameter", "DateTime")], fromLast = TRUE)), ]
  
  #Removes duplicates from data
  chemistry <- chemistry[!(duplicated(chemistry[c("StationID", "StationName", "Parameter", "DateTime")]) | duplicated(chemistry[c("StationID", "StationName", "Parameter", "DateTime")], fromLast = TRUE)), ]
  
  #Average duplicates by Date/Time, Station ID, Parameter
  duplicatesAveraged <- aggregate(Results ~ 
                                    StationID +
                                    StationName +
                                    Parameter +
                                    DateTime, 
                                  duplicates, 
                                  mean)
  
  #Add averaged duplicates back in
  chemistry <- rbind(chemistry, duplicatesAveraged)
  
  #Remove unneeded variables
  rm(duplicates, duplicatesAveraged)
  
  
  #####Change to wide view
  
  if (!require('tidyr')) install.packages('tidyr', quiet=TRUE)
  library(tidyr)
  
  chemistry2 <- spread(chemistry, 
                       Parameter, 
                       Results, 
                       fill = NA)
  
  describe(chemistry2)
  
  rm(chemistry)
  
  return(chemistry2)
}