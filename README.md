# QBMS
R package to Query [Breeding Management System](https://bmspro.io/) database (using API calls) in favor of scientists/researchers as targeted end users who want to retrieve their experiments data directly into R statistical analyzing environment.

```r
# Name:     example.R
# Purpose:  An example of the usage scenario to query and retrieve data from BMS using this QBMS R package
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# License:  GPLv3
# Version:  0.4
#
# Revision: v0.1 - 24 Jul 2019 - initial version
#           v0.2 - 20 Aug 2019 - update function calls to reflect the new names
#           v0.3 -  2 Jun 2020 - introduce new functionalities of MET, germplasm observations, and program studies
#           v0.4 -  3 Jul 2020 - R package version at GitHub

# install the package from GitHub
# library(devtools)
# Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
# install_github("khaled-alshamaa/QBMS")

# load QBMS library
library(QBMS)

# config your BMS connection
set_qbms_config("bms.example.com", 443, "https://")

# login using your BMS account (interactive mode)
# or pass your BMS username and password as parameters (batch mode)
login_bms()

# list supported crops in the bms server
list_crops()

# select a crop by name
set_crop("Tutorial1")

# list all breeding programs in the selected crop
list_programs()

# select a breeding program by name
set_program("Training Breeding Program")

# list all studies/trials in the selected program (optional, filter by year)
list_trials()
list_trials(2017)

# select a specific study/trial by name
set_trial("CIDTN-2016")

# get observation variable ontology in the selected study/trial
ontology <- get_trial_obs_ontology()

# list all environments/locations information in the selected study/trial
list_studies()

# select a specific environment/location by name
set_study("CIDTN-2016 Environment Number 1")

# retrieve data, general information, and germplasm list of the selected environment/location
data <- get_study_data()
info <- get_study_info()
germplasm <- get_germplasm_list()

# retrieve multi-environment trial data of the selected study/trial
MET <- get_trial_data()

# retrieve observations data of given germplasm aggregated from all trials in the selected program
germplasm_observations <- get_germplasm_data("FLIP10-3C")

# retrieve all environments/locations information in the selected program
program_studies <- get_program_studies()

```