# Name:     example.R
# Purpose:  Example set of function calls to query and retrieve data from BMS in use case scenario
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.1
# Revision: v0.1 - 24 Jul 2019 - initial version
# License:  GPLv3

# include and load required libraries
source("qbms.R")

# config your BMS connection
qbmsConfig$server <- "bms.icarda.org"

# if you are not connecting BMS server using SSL layer,
# then comment the following two lines 
qbmsConfig$protocol <- "https://"
qbmsConfig$port <- 18443

# login using your BMS account
qbmsLogin()

# list supported crops in the bms server
listCrops()
setCrop("Tutorial1")

# list existing breeding programs
listPrograms()
# select a breeding program by name
setProgram("Training Breeding Program")

# list all studies/trials in the selected program
listTrials()
listTrials(2017)
# select a specific study/trial by name
setTrial("CIDTN-2016")

# list all environments/locations in the selected study/trial
listStudies()
# select a specific environment/location dataset
setStudy("FLRP")

# retrive general information, data, and germplasm list of the selected environment/location
info <- getStudyInfo()
data <- getStudyData()
germplasm <- getGermplasmList()

# retrive multi-environment trial data
MET <- getTrialData()
