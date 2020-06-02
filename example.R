# Name:     example.R
# Purpose:  Example set of function calls to query and retrieve data from BMS in use case scenario
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.3
# Revision: v0.1 - 24 Jul 2019 - initial version
#           v0.2 - 20 Aug 2019 - update function calls to reflect the new names
#           v0.3 -  2 Jun 2020 - Introduce new functionalities of MET, germplasm observations, and program studies
# License:  GPLv3

# include and load required libraries
source("qbms.R")

# config your BMS connection
qbms_config$server <- "bms.icarda.org"

# if you are not connecting BMS server using SSL layer,
# then comment the following two lines 
qbms_config$protocol <- "https://"
qbms_config$port <- 18443

# login using your BMS account (interactive mode)
# You can pass BMS username and password as parameters (batch mode)
login_bms()

# list supported crops in the bms server
list_crops()

set_crop("Tutorial1")

# list existing breeding programs
list_programs()

# select a breeding program by name
set_program("Training Breeding Program")

# list all studies/trials in the selected program
list_trials()
list_trials(2017)

# select a specific study/trial by name
set_trial("CIDTN-2016")

# get observation variable ontology
ontology <- get_trial_obs_ontology()

# list all environments/locations information in the selected study/trial
list_studies()

# select a specific environment/location dataset
set_study("FLRP")

# retrieve general information, data, and germplasm list of the selected environment/location
info <- get_study_info()
data <- get_study_data()
germplasm <- get_germplasm_list()

# retrive multi-environment trial data
MET <- get_trial_data()

# retrive observations data of a given germplasm aggregated from all trials
germplasm_observations <- get_germplasm_data("FLIP10-3C")

# retrive all environments/locations information in the selected program studies/trials
program_studies <- get_program_studies()
