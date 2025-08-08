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

# database connection credentials in .Renviron or hardcoded here

#####
# UDS
# beware dbName identifies outputs, dbname is UCLH db
# dbName <- "UCLH-from-2019-uds"
# cdmSchema <- "omop_catalogue_raw"
# user <- Sys.getenv("user")
# host <- "uclvldddtaeps02.xuclh.nhs.uk"
# port <- 5432
# dbname <- "uds"
# pwd <- Sys.getenv("pwduds")
# writeSchema <- "omop_catalogue_analyse"


########################
# omop_reservoir version
# older extract, but more up to date postgres
# beware dbName identifies outputs, dbname is UCLH db
dbName <- "UCLH-from-2019-v2"
cdmSchema <- "data_catalogue_007" #from 2019
user <- Sys.getenv("user")
host <- Sys.getenv("host")
port <- Sys.getenv("port")
dbname <- Sys.getenv("dbname")
pwd <- Sys.getenv("pwd")
writeSchema <- "_other_andsouth"

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
prefix <- "hdruk_antib_newserver"

# minimum cell counts used for suppression
minCellCount <- 5

# to create the cdm object
cdm <- CDMConnector::cdmFromCon(
  con = con,
  cdmSchema = cdmSchema,
  writeSchema =  writeSchema,
  writePrefix = prefix,
  cdmName = dbName,
  .softValidation = TRUE
)

#to drop tables, beware if no prefix also everything()
#cdm <- CDMConnector::dropSourceTable(cdm = cdm, name = dplyr::starts_with("hdruk"))


# a patch to remove records where drug_exposure_start_date > drug_exposure_end_date
# ~2.5k rows in 2019 extract
#defail <- cdm$drug_exposure |> dplyr::filter(drug_exposure_start_date > drug_exposure_end_date) |>  collect()
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


############################################################################################
# temporary dmd patch (won't be necessary after next extract because now done within omop_es)
dmdlookup <- read_csv(here::here("dmdnew2old2rxnorm.csv"), col_types = "ccccic")

cdm$drug_exposure <- cdm$drug_exposure |> 
  left_join(select(dmdlookup,drug_source_value=dmd_new,
                   omop_rxnorm), by="drug_source_value", copy=TRUE) |> 
  #                  omop_rxnorm), by="drug_source_value") |>   
  mutate(drug_concept_id = if_else(drug_concept_id==0, omop_rxnorm, drug_concept_id)) |> 
  select(-omop_rxnorm)

# 2025-05-19 first attempt failed with :
# Error in `validateGeneratedCohortSet()`:
# ! cohort_start_date must be <= tham cohort_end_date. There is not the case for 1751 entries where cohort_end_date < cohort_start_date 

# 1752 patients to remove 
persremove <- cdm$observation_period |> 
  filter(observation_period_end_date < observation_period_start_date) |> 
  pull(person_id)

cdm$person              <- cdm$person |> filter(! person_id %in% persremove)
cdm$observation_period  <- cdm$observation_period |> filter(! person_id %in% persremove)
cdm$visit_occurrence    <- cdm$visit_occurrence |> filter(! person_id %in% persremove)
cdm$drug_exposure        <- cdm$drug_exposure |> filter(! person_id %in% persremove)

# cdm$condition_occurrence <- cdm$condition_occurrence |> filter(! person_id %in% persremove)
# cdm$procedure_occurrence <- cdm$procedure_occurrence |> filter(! person_id %in% persremove)
# cdm$device_exposure     <- cdm$device_exposure |> filter(! person_id %in% persremove)
# cdm$observation         <- cdm$observation |> filter(! person_id %in% persremove)
# cdm$measurement         <- cdm$measurement |> filter(! person_id %in% persremove)

cdm$person <- cdm$person |> mutate(location_id = ifelse(is.na(location_id),1,location_id))

#trying compute & save to temporary table (will appear in db with pre) 
cdm$observation_period <- cdm$observation_period |> 
  select(-death_date) |> 
  compute("observation_period")

cdm$person <- cdm$person |> 
  compute("person")  

# Minimum cell count
# This is the minimum counts that can be displayed according to data governance.
min_cell_count <- 5

## END OF SETTINGS copied between benchmarking, characterisation & antibiotics study

# TEMPORARY SUBSET OF CDM TO TRY TO GET IT TO RUN
# warning("TEMPORARY SUBSET OF CDM TO TRY TO GET IT TO RUN")
# cdm_subset <- cdm %>%
#   cdmSubset(personId = (1:1000))
# cdm <- cdm_subset

# Study start date -----
# please put the earliest date possible for your data.
# study_start <- "2012-01-01"
study_start <- "2019-04-01"

# Minimum cell count -----
# This is the minimum counts that can be displayed according to data governance.
min_cell_count <- 5

### Database settings
# Hospital databases should set the restrict_to_inpatient flag to TRUE.
restrict_to_inpatient <- TRUE #UCLH 2025-06-09 setting this to TRUE

# Databases that only include paediatric data should set the restrict_to_paediatric to TRUE. 
restrict_to_paediatric <- FALSE

# analyses to run -----
# setting to FALSE will skip analysis
run_characterisation <- TRUE
run_incidence <- TRUE
run_code_use <- FALSE

#Only set run_drug_exposure_diagnostics as TRUE if you are running the code for the first time.
run_drug_exposure_diagnostics <- FALSE

# Run the study
source(here("RunStudy.R"))

# Study Results to share ---
# After the study is run you should have the following files to share in your results folder:
# 1) log file of study
# 2) results.csv containing the main results of the study
