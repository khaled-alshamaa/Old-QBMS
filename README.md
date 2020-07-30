# QBMS
<!-- https://shields.io/ -->
R package to Query the [Breeding Management System](https://bmspro.io/) database (using [BrAPI](https://brapi.org/) calls) in favor of scientists/researchers as targeted end-users who want to retrieve their experiments data directly into R statistical analyzing environment.

> ___Package Author and Maintainer:__ Khaled Al-Shamaa <k.el-shamaa (at) cgiar (dot) org>_
>
> ___Package Copyright Holder:__ [International Center for Agricultural Research in the Dry Areas (ICARDA)](https://www.icarda.org/)_

## Breeding Management System
Breeding Management System (BMS) is an information management system developed by the Integrated Breeding Platform to help breeders manage the breeding process, from programme planning to decision-making. The BMS is customizable for most crop breeding programs, and comes pre-loaded with curated ontology terms for many crops (bean, cassava, chickpea, cowpea, groundnut, maize, rice, sorghum, soybean, wheat, and others). The BMS is available as a cloud application, which can be installed on local or remote servers and accessed by multiple users.

## BrAPI
The Breeding API (BrAPI) project is an effort to enable interoperability among plant breeding databases. BrAPI is a standardized RESTful web service API specification for communicating plant breeding data. This community driven standard is free to be used by anyone interested in plant breeding data management.

## _Install_
```r
install.packages("remotes")
remotes::install_github("khaled-alshamaa/QBMS")
```

## _Example_
```r
# load the QBMS library
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
