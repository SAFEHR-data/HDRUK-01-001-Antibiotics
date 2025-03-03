# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study
# install.packages("renv")
# renv::activate()
renv::restore()

library(CDMConnector)
library(DBI)
library(log4r)
library(readr)
library(DrugUtilisation)
library(IncidencePrevalence)
library(OmopSketch)
library(dplyr)
library(here)
library(tidyr)
library(CodelistGenerator)
library(CohortConstructor)
library(CohortCharacteristics)
library(PatientProfiles)
library(DrugExposureDiagnostics)
library(omopgenerics)
library(stringr)
library(RPostgres)
library(odbc)

## START OF SETTINGS copied between benchmarking, characterisation & antibiotics study

# acronym to identify the database
# beware dbName identifies outputs, dbname is UCLH db
# here different outputs can be created for each UCLH schema which is a different omop extract

# TO RUN choose a dbName,cdmSchema pair, comment out others, source script

#dbName <- "UCLH-EHDEN"
#cdmSchema <- "ehden_001"
#2025-01-20 completed in ~2.5 hours

# dbName <- "UCLH-6months"
# cdmSchema <- "data_catalogue_003" #6 months
# put brief progress here

dbName <- "UCLH-2years"
cdmSchema <- "data_catalogue_004" #2 years
# put brief progress here

# create a DBI connection to UCLH database
# using credentials in .Renviron or you can replace with hardcoded values here
user <- Sys.getenv("user")
host <- Sys.getenv("host")
port <- Sys.getenv("port")
dbname <- Sys.getenv("dbname")
pwd <- Sys.getenv("pwd")
# schema in database where you have writing permissions
writeSchema <- "_other_andsouth"

if("" %in% c(user, host, port, dbname, pwd, writeSchema))
  stop("seems you don't have (all?) db credentials stored in your .Renviron file, use usethis::edit_r_environ() to create")

#now pwd got from .Renviron
#pwd <- rstudioapi::askForPassword("Password for omop_db")


con <- DBI::dbConnect(RPostgres::Postgres(),user = user, host = host, port = port, dbname = dbname, password=pwd)

#you get this if not connected to VPN
#Error: could not translate host name ... to address: Unknown host

# created tables will start with this prefix
prefix <- "uclh_hdruk_benchmark"


# to create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema =  writeSchema,
  writePrefix = prefix,
  cdmName = dbName,
  .softValidation = TRUE
)

# 2025-02-04
# a patch to cope with records where drug_exposure_start_date > drug_exposure_end_date
# this causes error in benchmarking with 2 year extract (only 577 rows)
cdm$drug_exposure <- cdm$drug_exposure |> dplyr::filter(drug_exposure_start_date <= drug_exposure_end_date)

# Minimum cell count
# This is the minimum counts that can be displayed according to data governance.
min_cell_count <- 5

## END OF SETTINGS copied between benchmarking, characterisation & antibiotics study

# Study start date -----
# The earliest start date for this study "2012-01-01".
# Please put the study start date as "2012-01-01 if you have usable data from 2012 onwards.
# If you do not have data from 2012 onwards please put the earliest date possible for your data.
# For example if you only have usable data from 2015 you would put 2015-01-01.
# study_start <- "2012-01-01"
study_start <- "2024-01-01"

# Minimum cell count -----
# This is the minimum counts that can be displayed according to data governance.
min_cell_count <- 5

# Run the study ------
# if run_watch_list is TRUE, we run analyses both at ingredient and concept level
# if run_watch_list is FALSE, we only run analyses at concept level
run_watch_list <- TRUE 

# analyses to run -----
# setting to FALSE will skip analysis
run_drug_exposure_diagnostics <- TRUE
run_drug_utilisation <- TRUE
run_characterisation <- TRUE
run_incidence <- TRUE

# Run the study -----
source(here("RunStudy.R"))

# Study Results to share ---
# After the study is run you should have the following files to share in your results folder:
# 1) drug exposure diagnosis (DED) zip file
# 2) log file of study
# 3) results.csv containing the main results of the study