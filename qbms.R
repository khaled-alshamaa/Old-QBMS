# Name:     qbms.R
# Purpose:  Set of functions to query BMS by a wrapper using BrAPI calls
# Author:   Khaled Al-Shamaa <k.el-shamaa@cgiar.org>
# Version:  0.3
#
# Revision: 
#           v0.1 - 24 Jul 2019 
#                * Initial version.
#
#           v0.2 - 20 Aug 2019 
#                * Adopt tidyverse style guide https://style.tidyverse.org/
#                * Add functions documentation using roxygen2 format.
#                * Add basic error handling to the functions.
#                * Add a function to retrieve the traits ontology of a trial.
#
#           v0.3 - ?? Feb 2020
#                * Call BrAPI directly (i.e. not required "CIP-RIU/brapi" from GitHub anymore).
#                * Add a function to get all data of the current active trial (combined all studies).
#                * Add a function to get a list of studies where a given germplasm has been used.
#
# To-do:
#           * (in progress) Handle BrAPI pagination in a proper way in the `brapi_get_call()` function.
#           * (in progress) Test BrAPI call for `get_study_info()` on BMS v14 (v13 bug).
#           * (in progress) Test BrAPI call for `get_germplasm_list()` on BMS v14.
#           * (in progress) Finalize `get_germplasm_data()` function by join location info.
#
# License:  GPLv3

# Load/install required packages
if (!require(httr)) install.packages("httr")
if (!require(tcltk)) install.packages("tcltk")
if (!require(jsonlite)) install.packages("jsonlite")
if (!require(dplyr)) install.packages("dplyr")

# Configure BMS server settings
qbms_config <- list(server = "localhost")

qbms_config$port      <- 48080
qbms_config$protocol  <- "http://"
qbms_config$path      <- "bmsapi"
qbms_config$page_size <- 1000
qbms_config$crop      <- NULL
qbms_config$base_url  <- paste0(qbms_config$protocol, qbms_config$server, ":", qbms_config$port, "/", qbms_config$path)

# Query State
qbms_state <- list(token = NULL)


#' Internal function used for core BrAPI GET calls
#' 
#' @description
#' This function created for *internal use only* to cal BrAPI in GET method and 
#' retrieve the rough response data and send back the results. This function take
#' care of pagination, authintication, encoding, compress, decode JSON response, etc.
#' 
#' @param call_url BrAPI URL to call in GET method
#' @return result object returned by JSON API response
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}

brapi_get_call <- function(call_url){
  separator <- if (grepl("\\?", call_url)) "&" else "?"
  full_url  <- paste0(call_url, separator, "page=0&pageSize=", qbms_config$page_size)

  auth_code <- paste0("Bearer ", qbms_state$token)
  headers   <- c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")

  response  <- GET(URLencode(full_url), add_headers(headers))

  result_object <- fromJSON(content(response, as = "text"))
  result_info   <- result_object$result
  
  if (result_object$metadata$pagination$totalPages > 1 && is.null(result_object$errors)) {
    last_page <- result_object$metadata$pagination$totalPages - 1

    pb <- txtProgressBar(min = 1, max = last_page, style = 3)
    
    for (n in 1:last_page) {
      full_url <- paste0(call_url, separator, "page=", n, "&pageSize=", qbms_config$page_size)
      
      # Code to aggregate the response from several pages in one result object

      setTxtProgressBar(pb, n)
    }
    
    close(pb)
  }
  
  return(result_info)
}


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
#' @description
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
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}

login_bms <- function(username = NULL, password = NULL) {
  qbms_config$base_url <<- paste0(qbms_config$protocol, qbms_config$server, ":", qbms_config$port, "/", qbms_config$path)
  
  if (is.null(username) || is.null(password)) {
    credentials <- get_login_details()
  } else {
    credentials <- c(usr = username, pwd = password)
  }

  call_url  <- paste0(qbms_config$base_url, "/brapi/v1/token")
  call_body <- list(username = credentials["usr"], password = credentials["pwd"])
  
  response <- POST(call_url, body=call_body, encode="json")

  if (!is.null(content(response)$errors)) {
    stop(content(response)$errors[[1]]$message)
  }
  
  qbms_state$token <<- content(response)$access_token
  qbms_state$user  <<- content(response)$userDisplayName
  qbms_state$expires_in <<- content(response)$expires_in
}


#' Get the list of supported crops
#' 
#' @return a list of supported crops
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()]

list_crops <- function() {
  if (is.null(qbms_state$token)) {
    stop("No BMS server has been connected yet! You have to connect a BMS server first using the `bms_login()` function")
  }
  
  call_url <- paste0(qbms_config$base_url, "/brapi/v1/crops")
  
  bms_crops <- brapi_get_call(call_url)
  
  return(bms_crops$data)
}


#' Set the current active crop
#' 
#' @description
#' This function will update the current active crop in the internal 
#' configuration object (including the brapi connection object).
#' 
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [list_crops()]

set_crop <- function(crop_name) {
  valid_crops <- list_crops()
  
  if (!crop_name %in% valid_crops) {
    stop("Your crop name is not supported in this connected BMS server! You may use the `list_crops()` function to check the available crops")
  }
  
  qbms_config$crop <<- crop_name
}


#' Get the list of breeding programs names
#' 
#' @description
#' This function will retrieve the breeding programs list from the current active 
#' crop as configured in the internal configuration object using `set_crop()`
#' function.
#' 
#' @return a list of breeding programs names
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()]

list_programs <- function() {
  if (is.null(qbms_state$token)) {
    stop("No BMS server has been connected yet! You have to connect a BMS server first using the `bms_login()` function")
  }
  
  if (is.null(qbms_config$crop)) {
    stop("No crop has been selected yet! You have to set your crop first using the `set_crop()` function")
  }
  
  call_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1/programs")
  
  bms_programs <- brapi_get_call(call_url)
  
  return(bms_programs$data[c("name")])
}


#' Set the current active breeding program
#' 
#' @description
#' This function will update the current active breeding program in the 
#' internal state object using the programDbId retrieved from BMS which is 
#' associated to the given program_name parameter.
#' 
#' @param program_name the name of the breeding program
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [list_programs()]

set_program <- function(program_name) {
  valid_programs <- list_programs()
  
  if (!program_name %in% valid_programs$name) {
    stop("Your breeding program name is not exists in this crop database! You may use the `list_programs()` function to check the available breeding programs")
  }

  call_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1/programs")
  
  bms_programs <- brapi_get_call(call_url)

  program_row <- which(bms_programs$data$name == program_name)
  
  qbms_state$program_db_id <<- bms_programs$data[program_row, "programDbId"]
}


#' Internal function used to retrive the rough list of trials
#' 
#' @description
#' This function created for *internal use only* to retrieve the rough list of trials 
#' from the pre-selected (i.e. currently active) crop and breeding program combination
#' as already configured in the internal state object using `set_crop()` and `set_program()` 
#' functions respectivily.
#' 
#' @return a list of trials information
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [list_trials()]

get_program_trials <- function() {
  call_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1/trials")
  
  bms_crop_trials <- brapi_get_call(call_url)
  
  bms_program_trials <- subset(bms_crop_trials$data, programDbId == qbms_state$program_db_id)

  return(bms_program_trials)  
}


#' Get the list of trials in the current active breeding program 
#' 
#' @description
#' This function will retrieve the trials list from the current active breeding 
#' program as configured in the internal state object using `set_program()` 
#' function.
#' 
#' @param year the starting year to filter the list of trials (optional, default is NULL)
#' @return a list of trials names
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()]

list_trials <- function(year = NULL) {
  if (is.null(qbms_state$program_db_id)) {
    stop("No breeding program has been selected yet! You have to set your breeding program first using the `set_program()` function")
  }
  
  bms_trials <- get_program_trials()

  # startDate format in bms_trials is yyyymmdd
  if (!is.null(year)) {
    if (!is.numeric(year)) {
      stop("Year parameter if exists should be numeric")
    }
    
    from_date <- year * 10000
    to_date   <- year * 10000 + 1231
    
    bms_trials <- subset(bms_trials, startDate >= from_date & startDate <= to_date)
  }
  
  trials <- unique(bms_trials[c("trialName")])
  
  if (length(trials$trialName) == 0) {
    warning("No single trial fit your query parameters!")
    trials <- NA
  }
  
  return(trials)
}


#' Set the current active trial
#' 
#' @description
#' This function will update the current active trial in the internal state 
#' object using the trialDbId retrieved from BMS which is associated to the 
#' given trial_name parameter.
#' 
#' @param trial_name the name of the trial
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [list_trials()]

set_trial <- function(trial_name) {
  valid_trials <- list_trials()
  
  if (!trial_name %in% valid_trials$trialName) {
    stop("Your trial name is not exists in this breeding program!  You may use the `list_trials()` function to check the available trials")
  }
  
  bms_trials <- get_program_trials()
  
  trial_row <- which(bms_trials$trialName == trial_name)[1]
  
  qbms_state$trial_db_id <<- as.character(bms_trials[trial_row, c("trialDbId")])
}


#' Get the list of studies in the current active trial
#' 
#' @description
#' This function will retrieve the studies list from the current active trial 
#' as configured in the internal state object using `set_trial()` function.
#' 
#' @return a list of study location names
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]

list_studies <- function() {
  if (is.null(qbms_state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  bms_trials <- get_program_trials()
  
  trial_row <- which(bms_trials$trialDbId == qbms_state$trial_db_id)
  
  studies <- bms_trials[trial_row, c("studies")][[1]]$locationName
  
  return(studies)
}


#' Set the current active study by location name
#' 
#' @description
#' This function will update the current active study in the internal state 
#' object using the studyDbId retrieved from BMS which is associated to the 
#' given location_name parameter.
#' 
#' @param location_name the name of the location
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], [list_studies()]

set_study <- function(location_name) {
  valid_studies <- list_studies()
  
  if (!location_name %in% valid_studies) {
    stop("Your location name is not exists in this trial! You may use the `list_studies()` function to check the available study location names")
  }
  
  bms_trials <- get_program_trials()

  trial_row <- which(bms_trials$trialDbId == qbms_state$trial_db_id)
  
  bms_studies <- bms_trials[trial_row, c("studies")][[1]]
  
  study_row <- which(bms_studies$locationName == location_name)
  
  qbms_state$study_db_id <<- as.character(bms_studies[study_row, "studyDbId"])
}


#' Get the details/metadata of the current active study
#' 
#' @description
#' This function will retrieve the details/metadata of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study details/metadata
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], [set_study()]

get_study_info <- function() {
  if (is.null(qbms_state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }
  
  # broken in BMS ver 13, and bug fixed in BMS ver 14
  # https://ibplatform.atlassian.net/servicedesk/customer/portal/4/IBPS-602
  # study_info <- ba_studies_details(qbms_config$con, rclass = "data.frame", studyDbId = qbms_state$study_db_id)

  crop_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_state$study_db_id)
  
  study_info <- brapi_get_call(call_url)
  
  return(study_info)
}


#' Get the observations data of the current active study
#' 
#' @description
#' This function will retrieve the observations data of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study observations data
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], [set_study()]

get_study_data <- function() {
  if (is.null(qbms_state$study_db_id)) {
    stop("No study has been selected yet! You have to set your study first using the `set_study()` function")
  }

  crop_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_state$study_db_id, "/table")
  
  study_result <- brapi_get_call(call_url)
  
  study_data <- as.data.frame(study_result$data)
  study_header <- c(study_result$headerRow, study_result$observationVariableNames)
  colnames(study_data) <- study_header
  
  return(study_data)
}


#' Get the germplasm list of the current active study
#' 
#' @description
#' This function will retrieve the germplasm list of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @return a data frame of the study germplasm list
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()], [set_study()]

get_germplasm_list <- function() {
  if (is.null(qbms_state$trial_db_id)) {
    stop("No trial has been selected yet! You have to set your trial first using the `set_trial()` function")
  }
  
  # replace the API call by the following BrAPI once implemented in BMS
  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Studies/Studies_Germplasm_GET.md
  #
  # BMS implementation for this BrAPI call is coming in version 14
  # https://github.com/plantbreeding/API/blob/master/Specification/Studies/Studies_StudyDbId_Germplasm_GET.yaml

  my_url <- paste0(qbms_config$base_url, "/study/", qbms_config$crop, "/", qbms_state$trial_db_id, "/germplasm")
  response <- GET(url = my_url, add_headers("X-Auth-Token" = qbms_state$token))
  germplasm_list <- fromJSON(content(response, as = "text"))
  
  #crop_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1")
  #call_url <- paste0(crop_url, "/studies/", qbms_state$study_db_id, "/germplasm")
  #germplasm_list <- brapi_get_call(call_url)
  
  return(germplasm_list)
}


#' Get the observations data of the current active trial
#' 
#' @description
#' This function will retrieve the observations data of the current active trial
#' (i.e. including all studies within) as configured in the internal state 
#' object using `set_trial()` function.
#' 
#' @return a data frame of the trial observations data
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]

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
#' @description
#' This function will retrive the traits ontology/metadata of the current active 
#' trial as configured in the internal state object using `set_trial()` function.
#' 
#' @return a data frame of the traits ontology/metadata
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()], [set_trial()]

get_trial_obs_ontology <- function() {
  set_study(list_studies()[1])
  
  crop_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/studies/", qbms_state$study_db_id, "/table")
  
  study_data <- brapi_get_call(call_url)

  my_url <- paste0(qbms_config$base_url, "/ontology/", qbms_config$crop, 
                   "/variables/?programId=", qbms_state$program_db_id)
  
  response <- GET(my_url, add_headers("X-Auth-Token" = qbms_state$token))
  
  ontology <- fromJSON(content(response, as = "text"), flatten = TRUE)
  
  study_obs <- study_data$observationVariableDbIds
  
  study_ontology <- ontology[ontology$id %in% study_obs, ]
  
  return(study_ontology)
}


#' Get the observations data of a given germplasm name
#' 
#' @description
#' This function will retrieve the observations data of the current active study
#' as configured in the internal state object using `set_study()` function.
#' 
#' @param germplasm_name the name of the germplasm
#' @return a data frame of the germplasm observations data aggregate from all trials
#' @author K. Al-Shamaa, \email{k.el-shamaa@cgiar.org}
#' @seealso [login_bms()], [set_crop()], [set_program()]

get_germplasm_data <- function(germplasm_name) {
  crop_url <- paste0(qbms_config$base_url, "/", qbms_config$crop, "/brapi/v1")
  call_url <- paste0(crop_url, "/germplasm-search?germplasmName=", germplasm_name)

  germplasm_db_id <- brapi_get_call(call_url)$data$germplasmDbId

  # https://github.com/plantbreeding/API/blob/V1.2/Specification/Phenotypes/PhenotypesSearch_POST.md
  # Note 1: It does not work with germplasm name (BrAPI specifications): e.g. {"germplasmDbIds": ["ILC 3279"]}
  # Note 2: Return "Invalid request body" if we search for one germplasm_db_id!

  call_url  <- paste0(crop_url, "/phenotypes-search")
  call_body <- list(germplasmDbIds = c(germplasm_db_id,""), observationLevel = "PLOT")
  auth_code <- paste0("Bearer ", qbms_state$token)

  response <- POST(call_url, body=call_body, encode="json", add_headers(c("Authorization" = auth_code, "Accept-Encoding" = "gzip, deflate")))
  
  results <- content(response)$result$data
  
  flatten_results <- fromJSON(toJSON(results), flatten = TRUE)

  # unlist nested list with id
  unlisted_observations <- rbindlist(flatten_results$observations, fill = TRUE, idcol = "id")
  
  # create same id in remaining data frame
  flatten_results$id <- seq.int(nrow(flatten_results))
  
  # join data frame with unlisted list
  flatten_results <- left_join(flatten_results, unlisted_observations, by = "id")
  
  # get rid of unnecessary columns
  flatten_results$observations <- NULL
  flatten_results$id <- NULL
  
  # we still need to filter out unnecessary columns, 
  # extract locations info. using get_study_info function, 
  # and then rejoin that info. to this master observation table 

  return(flatten_results)
}
