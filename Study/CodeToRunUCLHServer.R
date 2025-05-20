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

dbName <- "UCLH-from-2019"
cdmSchema <- "omop_catalogue_raw"

# create a DBI connection to UCLH database
# using credentials in .Renviron or you can replace with hardcoded values here
user <- Sys.getenv("user")
host <- "uclvldddtaeps02.xuclh.nhs.uk"
port <- 5432
dbname <- "uds"
pwd <- Sys.getenv("pwduds")

# schema in database where you have writing permissions
writeSchema <- "omop_catalogue_analyse"

if("" %in% c(user, host, port, dbname, pwd, writeSchema))
  stop("seems you don't have (all?) db credentials stored in your .Renviron file, use usethis::edit_r_environ() to create")

#pwd <- rstudioapi::askForPassword("Password for omop_db")

con <- DBI::dbConnect(RPostgres::Postgres(),user = user, host = host, port = port, dbname = dbname, password=pwd)

#you get this if not connected to VPN
#Error: could not translate host name ... to address: Unknown host

#list tables
DBI::dbListObjects(con, DBI::Id(schema = cdmSchema))
DBI::dbListObjects(con, DBI::Id(schema = writeSchema))

# created tables will start with this prefix
prefix <- "hdruk_characterisation"

# minimum cell counts used for suppression
minCellCount <- 5

# to create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema =  writeSchema,
  writePrefix = prefix,
  cdmName = dbName,
  #cdmVersion = "5.3",
  .softValidation = TRUE
)

# 2025-02-04
# a patch to cope with records where drug_exposure_start_date > drug_exposure_end_date
# this causes error in benchmarking with 2 year extract (only 577 rows)
cdm$drug_exposure <- cdm$drug_exposure |> dplyr::filter(drug_exposure_start_date <= drug_exposure_end_date)

########################
# fix observation_period that got messed up in latest extract
op2 <- cdm$visit_occurrence |>
  group_by(person_id) |> 
  summarise(minvis = min(coalesce(date(visit_start_datetime), visit_start_date), na.rm=TRUE),
            maxvis = max(coalesce(date(visit_end_datetime), visit_end_date), na.rm=TRUE)) |> 
  left_join(select(cdm$death,person_id,death_date), by=join_by(person_id)) |> 
  #set maxvisit to death_date if before
  #mutate(maxvis=min(maxvis, death_date, na.rm=TRUE))
  mutate(maxvis = if_else(!is.na(death_date) & maxvis > death_date, death_date, maxvis))

cdm$observation_period <- cdm$observation_period |>    
  left_join(op2, by=join_by(person_id)) |>
  select(-observation_period_start_date) |> 
  select(-observation_period_end_date) |> 
  rename(observation_period_start_date=minvis,
         observation_period_end_date=maxvis)

#trying a small sample but still fails at bit from run_study.R
#cdm$observation_period <- cdm$observation_period |> head(100)
#results[["obs_period"]] <- summariseObservationPeriod(cdm$observation_period)


############################################################################################
# temporary dmd patch (won't be necessary after next extract because now done within omop_es)
dmdlookup <- read_csv(here::here("dmdnew2old2rxnorm.csv"), col_types = "ccccic")

cdm$drug_exposure <- cdm$drug_exposure |> 
  left_join(select(dmdlookup,drug_source_value=dmd_new,
                   omop_rxnorm), by="drug_source_value", copy=TRUE) |> 
  #                  omop_rxnorm), by="drug_source_value") |>   
  mutate(drug_concept_id = if_else(drug_concept_id==0, omop_rxnorm, drug_concept_id)) |> 
  select(-omop_rxnorm)

# Minimum cell count
# This is the minimum counts that can be displayed according to data governance.
min_cell_count <- 5

## END OF SETTINGS copied between benchmarking, characterisation & antibiotics study

# Study start date -----
# please put the earliest date possible for your data.
# study_start <- "2012-01-01"
study_start <- "2019-04-01"

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