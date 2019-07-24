# Name:     qbms.R
# Purpose:  Set of functions to create a BMS query package using BrAPI calls
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.1
# Revision: v0.1 - 24 Jul 2019 - initial version
# License:  GPLv3

# Load/install required packages
if (!require(httr)) { install.packages("httr"); library(httr) }
if (!require(tcltk)) { install.packages("tcltk"); library(tcltk) }
if (!require(jsonlite)) { install.packages("jsonlite"); library(jsonlite) }
if (!require(devtools)) install.packages("devtools")
if (!require(brapi)) { devtools::install_github("CIP-RIU/brapi"); library(brapi) }


# Configure BMS server settings
qbmsConfig <- list(server="localhost")
qbmsConfig$port <- 48080
qbmsConfig$protocol <- "http://"
qbmsConfig$path <- "bmsapi"
qbmsConfig$crop <- "Tutorial1"
qbmsConfig$baseURL <- paste0(qbmsConfig$protocol, qbmsConfig$server, ":", qbmsConfig$port, "/", qbmsConfig$path)

# Query State
qbmsState <- list(token=NULL)

getLoginDetails <- function(){
  tt <- tktoplevel()
  tkwm.title(tt, "Login BMS Server")
  
  ss <- "Please enter your BMS login details"
  tkgrid(tklabel(tt, text=ss), columnspan=2, padx=50, pady=10)
  
  Username <- tclVar("")
  Password <- tclVar("")
  
  entry.UserLabel <- tklabel(tt, text="Username:")
  entry.PassLabel <- tklabel(tt, text="Password:")
  
  entry.Username <- tkentry(tt, width="30", textvariable=Username)
  entry.Password <- tkentry(tt, width="30", textvariable=Password, show="*")
  
  tkgrid(entry.UserLabel, entry.Username, sticky="ew", padx=5)
  tkgrid(entry.PassLabel, entry.Password, sticky="ew", padx=5)
  
  OnOK <- function(){
    tkdestroy(tt)
  }
  
  OK.button <- tkbutton(tt, text=" OK ", command=OnOK)
  tkbind(entry.Password, "<Return>", OnOK)
  tkgrid(OK.button, columnspan=2, pady=5)
  
  tkfocus(tt)
  tkwait.window(tt)
  
  invisible(c(usr=tclvalue(Username), pwd=tclvalue(Password)))
}

qbmsLogin <- function(){
  qbmsConfig$baseURL <<- paste0(qbmsConfig$protocol, qbmsConfig$server, ":", qbmsConfig$port, "/", qbmsConfig$path)
  
  credentials <- getLoginDetails()
  
  connection <- ba_connect(db=qbmsConfig$server, port=qbmsConfig$port, apipath=qbmsConfig$path, 
                           user=credentials["usr"], password=credentials["pwd"], 
                           multicrop=TRUE, bms=TRUE, crop="", protocol=qbmsConfig$protocol)
  
  qbmsConfig$con <<- ba_login(connection)
  qbmsState$token <<- qbmsConfig$con$token
}

listCrops <- function(){
  # ba_crops deprecated, but ba_commoncropnames generate an error!
  bmsCrops <- suppressWarnings(ba_crops(qbmsConfig$con, rclass="data.frame"))
  
  return(bmsCrops[c("crops")])
}

setCrop <- function(cropName){
  qbmsConfig$crop <<- cropName
  
  qbmsConfig$con$crop <<- qbmsConfig$crop
}

listPrograms <- function(){
  bmsPrograms <- ba_programs(qbmsConfig$con, rclass="data.frame")
  
  return(bmsPrograms[c("name")])
}

setProgram <- function(programName){
  bmsPrograms <- ba_programs(qbmsConfig$con, rclass="data.frame")
  
  qbmsState$programDbId <<- bmsPrograms[which(bmsPrograms$name==programName), "programDbId"]
}

listTrials <- function(year=NULL){
  bmsTrials <- ba_trials(qbmsConfig$con, programDbId=qbmsState$programDbId, rclass="data.frame")
  
  if(!is.null(year)){
    fromDate <- year * 10000
    toDate <- year * 10000 + 1231
    
    bmsTrials <- subset(bmsTrials, startDate >= fromDate & startDate <= toDate)
  }
  
  trials <- unique(bmsTrials[c("trialName")])
  
  return(trials)
}

setTrial <- function(trialName){
  bmsTrials <- ba_trials(qbmsConfig$con, programDbId=qbmsState$programDbId, rclass="data.frame")
  
  qbmsState$trialDbId <<- as.character(bmsTrials[which(bmsTrials$trialName==trialName)[1], c("trialDbId")])
}

listStudies <- function(){
  bmsTrials <- ba_trials(qbmsConfig$con, programDbId=qbmsState$programDbId, rclass="data.frame")
  
  studies <- bmsTrials[which(bmsTrials$trialDbId==qbmsState$trialDbId), c("locationName")]
  
  return(studies)
}

setStudy <- function(locationName){
  bmsTrials <- ba_trials(qbmsConfig$con, programDbId=qbmsState$programDbId, rclass="data.frame")
  
  qbmsState$studyDbId <<- as.character(bmsTrials[which((bmsTrials$trialDbId==qbmsState$trialDbId & bmsTrials$locationName==locationName)), "studyDbId"])
}

getStudyInfo <- function(){
  studyInfo <- ba_studies_details(qbmsConfig$con, studyDbId=qbmsState$studyDbId, rclass="data.frame")
  
  return(studyInfo)
}

getStudyData <- function(){
  studyData <- ba_studies_table(qbmsConfig$con, studyDbId=qbmsState$studyDbId, rclass="data.frame")
  
  return(studyData)
}

getGermplasmList <- function(){
  # replace the API call by the following BrAPI once implemented in BMS
  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Studies/Studies_Germplasm_GET.md
  
  myUrl <- paste0(qbmsConfig$baseURL, "/study/", qbmsConfig$crop, "/", qbmsState$trialDbId, "/germplasm")
  
  response <- GET(url = myUrl, add_headers("X-Auth-Token" = qbmsConfig$con$token))
  
  germplasmList <- fromJSON(content(response, as = "text"))
  
  return(germplasmList)
}

getTrialData <- function(){
  trialData <- data.frame()
  env <- listStudies()
  
  for(i in env){
    setStudy(i)
    studyData <- getStudyData()
    trialData <- rbind(trialData, studyData)
  }
  
  return(trialData)
}

