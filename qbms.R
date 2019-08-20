# Name:     qbms.R
# Purpose:  Set of functions to query BMS by wrapper using BrAPI calls
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.2
#
# Revision: 
#           v0.1 - 24 Jul 2019 
#                * initial version
#
#           v0.2 - 20 Aug 2019 
#                * adopt tidyverse style guide https://style.tidyverse.org/
#                * add functions documentation using roxygen2 format
#                * add basic error handling to the functions
#                * add a function to retrieve the traits ontology of a trial
#
# To-do:
#           * Validation URL using regular expression (e.g. base_url and my_url)
#           * Constructor to setup/encapsulate both of qbms_config and qbms_state
#
# License:  GPLv3

# Load/install required packages
if (!require(httr)) {
  install.packages("httr")
  library(httr)
}

if (!require(tcltk)) {
  install.packages("tcltk")
  library(tcltk)
}

if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

if (!require(devtools)) {
  install.packages("devtools")
  library("devtools")
}

if (!require(brapi)) {
  devtools::install_github("CIP-RIU/brapi", upgrade=FALSE)
  library(brapi)
}


# Configure BMS server settings
qbms_config <- list(server = "localhost")

qbms_config$port     <- 48080
qbms_config$protocol <- "http://"
qbms_config$path     <- "bmsapi"
qbms_config$crop     <- NULL
qbms_config$base_url <- paste0(qbms_config$protocol, qbms_config$server, ":", 
                               qbms_config$port, "/", qbms_config$path)

# Query State
qbms_state <- list(token = NULL)

#' Login pop-up window
#' 
#' Build a GUI pop-up window using Tcl/Tk to insert BMS username and password
#' 
#' @return a vector of inserted username and password
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
get_login_details <- function() {
  tt <- tktoplevel()
  tkwm.title(tt, "Login BMS Server")
  
  ss <- "Please enter your BMS login details"
  tkgrid(tklabel(tt, text = ss), columnspan = 2, padx = 50, pady = 10)
  
  usr <- tclVar("")
  pwd <- tclVar("")
  
  user_label <- tklabel(tt, text = "Username:")
  pass_label <- tklabel(tt, text = "Password:")
  
  user_input <- tkentry(tt, width = "30", textvariable = usr)
  pass_input <- tkentry(tt, width = "30", textvariable = pwd, show = "*")
  
  tkgrid(user_label, user_input, sticky = "ew", padx = 5)
  tkgrid(pass_label, pass_input, sticky = "ew", padx = 5)
  
  on_okay <- function() {
    tkdestroy(tt)
  }
  
  ok_button <- tkbutton(tt, text = " OK ", command = on_okay)
  tkbind(pass_input, "<Return>", on_okay)
  tkgrid(ok_button, columnspan = 2, pady = 5)
  
  tkfocus(tt)
  tkwait.window(tt)
  
  invisible(c(usr = tclvalue(usr), pwd = tclvalue(pwd)))
}

#' Login to the BMS server
#' 
#' Connect to the BMS server. If username or password parameters are missing, 
#' then a login window will pop-up to insert username and password. 
#' 
#' All other connection parameters (i.e. server IP or domain, connection port, 
#' API path, and connection protocol e.g. http://) will retrieve from the 
#' qbms_config list.
#' 
#' This function will update both of the qbms_config list (brapi connection 
#' object in the con key) and qbms_state list (token value in the token key).
#' 
#' @param username the BMS username (optional, default is NULL)
#' @param password the BMS password (optional, default is NULL)
#' 
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @examples
login_bms <- function(username = NULL, password = NULL) {
  qbms_config$base_url <<- paste0(qbms_config$protocol, qbms_config$server, ":", 
                                  qbms_config$port, "/", qbms_config$path)
  
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }
  
  connection <- ba_connect(db        = qbms_config$server, 
                           port      = qbms_config$port, 
                           apipath   = qbms_config$path, 
                           protocol  = qbms_config$protocol,
                           user      = credentials["usr"], 
                           password  = credentials["pwd"], 
                           multicrop = TRUE, 
                           bms       = TRUE, 
                           crop      = "")
  
  qbms_config$con  <<- ba_login(connection)
  qbms_state$token <<- qbms_config$con$token
}

#' Get the list of supported crops
#' 
#' @return a list of supported crops
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()]
#' @examples
list_crops <- function() {
  if (!is.ba_con(qbms_config$con)) {
    stop(paste("No BMS server has been connected yet!", 
               "You have to connect a BMS server first",
               "using the `bms_login()` function"))
  }
  
  # ba_crops deprecated, but ba_commoncropnames generate an error!
  bms_crops <- suppressWarnings(ba_crops(qbms_config$con, rclass = "data.frame"))
  
  return(bms_crops[c("crops")])
}

#' Set the current active crop
#' 
#' This function will update the current active crop in the internal 
#' configuration object (including the brapi connection object).
#' 
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [list_crops()]
#' @examples
set_crop <- function(crop_name) {
  valid_crops <- list_crops()
  
  if (!crop_name %in% valid_crops$crops) {
    stop(paste("Your crop name is not supported in this connected BMS server!",
         "You may use the `list_crops()` function to check the available crops"))
  }
  
  qbms_config$crop <<- crop_name
  
  qbms_config$con$crop <<- qbms_config$crop
}

#' Get the list of breeding programs names
#' 
#' This function will retrieve the breeding programs list from the current active 
#' crop as configured in the internal configuration object using `set_crop()`
#' function.
#' 
#' @return a list of breeding programs names
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()]
#' @examples
list_programs <- function() {
  if (!is.ba_con(qbms_config$con)) {
    stop(paste("No BMS server has been connected yet!", 
               "You have to connect a BMS server first",
               "using the `bms_login()` function"))
  }
  
  if (is.null(qbms_config$crop)) {
    stop(paste("No crop has been selected yet!",
               "You have to set your crop first using",
               "the `set_crop()` function"))
  }
  
  bms_programs <- ba_programs(qbms_config$con, rclass = "data.frame")
  
  return(bms_programs[c("name")])
}

#' Set the current active breeding program
#' 
#' This function will update the current active breeding program in the 
#' internal state object using the programDbId retrieved from BMS which is 
#' associated to the given program_name parameter.
#' 
#' @param program_name the name of the breeding program
#' 
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [list_programs()]
#' @examples
set_program <- function(program_name) {
  valid_programs <- list_programs()
  
  if (!program_name %in% valid_programs$name) {
    stop(paste("Your breeding program name is not exists in this crop database!", 
               "You may use the `list_programs()` function",
               "to check the available breeding programs"))
  }

  bms_programs <- ba_programs(qbms_config$con, rclass = "data.frame")
  
  program_row <- which(bms_programs$name == program_name)
  
  qbms_state$program_db_id <<- bms_programs[program_row, "programDbId"]
}

#' Get the list of trials in the current active breeding program 
#' 
#' This function will retrieve the trials list from the current active breeding 
#' program as configured in the internal state object using `set_program()` 
#' function.
#' 
#' @param year the starting year to filter the list of trials (optional, default is NULL)
#' 
#' @return a list of trials names
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()]
#' @examples
list_trials <- function(year = NULL) {
  if (is.null(qbms_state$program_db_id)) {
    stop(paste("No breeding program has been selected yet!",
               "You have to set your breeding program first using",
               "the `set_program()` function"))
  }
  
  bms_trials <- ba_trials(qbms_config$con, rclass = "data.frame", 
                          programDbId = qbms_state$program_db_id)
  
  # startDate format in bms_trials is yyyymmdd
  if (!is.null(year)) {
    if (!is.numeric(year)) {
      stop("Year parameter if exists should be numeric")
    }
    
    from_date <- year * 10000
    to_date   <- year * 10000 + 1231
    
    bms_trials <- subset(bms_trials, 
                         startDate >= from_date & startDate <= to_date)
  }
  
  trials <- unique(bms_trials[c("trialName")])
  
  # if (length(trials$trialName) == 0) {
  #   warning("No single trial fit your query parameters!")
  #   trials <- NA
  # }
  
  return(trials)
}

#' Set the current active trial
#' 
#' This function will update the current active trial in the internal state 
#' object using the trialDbId retrieved from BMS which is associated to the 
#' given trial_name parameter.
#' 
#' @param trial_name the name of the trial
#' 
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [list_trials()]
#' @examples
set_trial <- function(trial_name) {
  valid_trials <- list_trials()
  
  if (!trial_name %in% valid_trials$trialName) {
    stop(paste("Your trial name is not exists in this breeding program!", 
               "You may use the `list_trials()` function",
               "to check the available trials"))
  }
  
  bms_trials <- ba_trials(qbms_config$con, rclass = "data.frame",
                          programDbId = qbms_state$program_db_id)
  
  trial_row <- which(bms_trials$trialName == trial_name)[1]
  
  qbms_state$trial_db_id <<- as.character(bms_trials[trial_row, c("trialDbId")])
}

#' Get the list of studies in the current active trial
#' 
#' This function will retrieve the studies list from the current active trial 
#' as configured in the internal state object using `set_trial()` function.
#' 
#' @return a list of study location names
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]
#' @examples
list_studies <- function() {
  if (is.null(qbms_state$trial_db_id)) {
    stop(paste("No trial has been selected yet!",
               "You have to set your trial first using",
               "the `set_trial()` function"))
  }
  
  bms_trials <- ba_trials(qbms_config$con, rclass = "data.frame",
                          programDbId = qbms_state$program_db_id)
  
  trial_rows <- which(bms_trials$trialDbId == qbms_state$trial_db_id)
  
  studies <- bms_trials[trial_rows, c("locationName")]
  
  return(studies)
}

#' Set the current active study by location name
#' 
#' This function will update the current active study in the internal state 
#' object using the studyDbId retrieved from BMS which is associated to the 
#' given location_name parameter.
#' 
#' @param location_name the name of the location
#' 
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], 
#' [list_studies()]
#' @examples
set_study <- function(location_name) {
  valid_studies <- list_studies()
  
  if (!location_name %in% valid_studies) {
    stop(paste("Your location name is not exists in this trial!", 
               "You may use the `list_studies()` function",
               "to check the available study location names"))
  }
  
  bms_trials <- ba_trials(qbms_config$con, rclass = "data.frame",
                          programDbId = qbms_state$program_db_id)
  
  study_row <- which((bms_trials$trialDbId == qbms_state$trial_db_id & 
                      bms_trials$locationName == location_name))
  
  qbms_state$study_db_id <<- as.character(bms_trials[study_row, "studyDbId"])
}

#' Get the details/metadata of the current active study
#' 
#' This function will retrieve the details/metadata of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study details/metadata
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], 
#' [set_study()]
#' @examples
get_study_info <- function() {
  if (is.null(qbms_state$study_db_id)) {
    stop(paste("No study has been selected yet!",
               "You have to set your study first using",
               "the `set_study()` function"))
  }
  
  study_info <- ba_studies_details(qbms_config$con, rclass = "data.frame",
                                   studyDbId = qbms_state$study_db_id)
  
  return(study_info)
}

#' Get the observations data of the current active study
#' 
#' This function will retrieve the observations data of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study observations data
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], 
#' [set_study()]
#' @examples
get_study_data <- function() {
  if (is.null(qbms_state$study_db_id)) {
    stop(paste("No study has been selected yet!",
               "You have to set your study first using",
               "the `set_study()` function"))
  }

  study_data <- ba_studies_table(qbms_config$con, rclass = "data.frame", 
                                 studyDbId = qbms_state$study_db_id)
  
  return(study_data)
}

#' Get the germplasm list of the current active study
#' 
#' This function will retrieve the germplasm list of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study germplasm list
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], 
#' [set_study()]
#' @examples
get_germplasm_list <- function() {
  # replace the API call by the following BrAPI once implemented in BMS
  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Studies/Studies_Germplasm_GET.md

  if (is.null(qbms_state$trial_db_id)) {
    stop(paste("No trial has been selected yet!",
               "You have to set your trial first using",
               "the `set_trial()` function"))
  }
  
  my_url <- paste0(qbms_config$base_url, "/study/", qbms_config$crop, "/", 
                   qbms_state$trial_db_id, "/germplasm")
  
  response <- GET(url = my_url, 
                  add_headers("X-Auth-Token" = qbms_config$con$token))
  
  germplasm_list <- fromJSON(content(response, as = "text"))
  
  return(germplasm_list)
}

#' Get the observations data of the current active trial
#' 
#' This function will retrieve the observations data of the current active trial
#' (i.e. including all studies within) as configured in the internal state 
#' object using `set_trial()` function.
#' 
#' @return a data frame of the trial observations data
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]
#' @examples
get_trial_data <- function() {
  trial_data <- data.frame()
  env <- list_studies()
  
  for (i in env) {
    set_study(i)
    study_data <- get_study_data()
    trial_data <- rbind(trial_data, study_data)
  }
  
  return(trial_data)
}

#' Get the traits ontology/metadata of the current active trial
#' 
#' This function will retrive the traits ontology/metadata of the current active 
#' trial as configured in the internal state object using `set_trial()` function.
#' 
#' @return a data frame of the traits ontology/metadata
#' @export
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' 
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]
#' @examples
get_trial_obs_ontology <- function() {
  set_study(list_studies()[1])
  
  my_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, 
                  "/brapi/v1/studies/", qbms_state$study_db_id, "/table")
  
  auth_code <- paste0("Bearer ", qbms_config$con$token)
  response  <- GET(my_url, add_headers("Authorization" = auth_code))
  
  study_data <- fromJSON(content(response, as = "text"))

  my_url <- paste0(qbms_config$base_url, "/ontology/", qbms_config$crop, 
                  "/variables/?programId=", qbms_state$program_db_id)
  
  response <- GET(my_url, add_headers("X-Auth-Token" = qbms_config$con$token))
  
  ontology <- fromJSON(content(response, as = "text"), flatten = TRUE)
  
  study_obs <- study_data$result$observationVariableDbIds
  
  study_ontology <- ontology[ontology$id %in% study_obs, ]
  
  return(study_ontology)
}

GetGermplasmList <- function(germplasmName) {
  #################################################
  #qbms_config$server <- "34.226.132.187"
  #qbms_config$port     <- 48080
  #qbms_config$protocol <- "http://"
  
  #LoginBMS("khaled", "icarda")

  #SetCrop("wheat")
  #SetProgram("Wheat Example Program")
  
  #germplasmName <- "AAR"
  
  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_POST.md
  myUrl <- paste0(qbms_config$base_url, "/germplasmList/", 
                  qbms_config$crop, "/search?q=", germplasmName)
  
  response <- GET(url = myUrl, 
                  add_headers("X-Auth-Token" = qbms_config$con$token))
  
  germplasmLists <- fromJSON(content(response, as = "text"))
  
  for (listId in germplasmLists$listId) {
    print(listId)
    # get the list of trials where this germplasm list has been used
    # for each trial
    #   get MET dataset
    #   subset by GID or germplasmName
    #   rbind from all trials involved
  }
  return ()
}
