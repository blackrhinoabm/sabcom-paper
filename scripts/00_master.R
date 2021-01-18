# SET DIRECTORIES -------------------------------------------------------------

setwd("/Users/Allan/Dropbox/Research/sabcom/sabcom-paper")

base_data_directory <- "/Users/Allan/Dropbox/Research/sabcom/sabcom-paper/simulation_output"

output_folder <- "/Users/Allan/Dropbox/Research/sabcom/sabcom-paper/results"

# PACKAGES --------------------------------------------------------------------

libs <- c(
  "tidyverse", "data.table", "RColorBrewer", "sf", "tmap", "knitr",
  "xtable", "rjson", "here"
)

lapply(libs, require, character.only = TRUE)

# SET PARAMETERS --------------------------------------------------------------


learning <- c("de_groot", "lexicographic")

cities <- c("cape_town", "johannesburg")

scenarios <- c("lockdown", "no_intervention")

model_population <- 100000

populations <- c(3740026, 4434827)

# Cape Town hospital capacity and excess deaths

cpt_parameters <- fromJSON(file = "cape_town/parameters.json")
cpt_hospital_capacity <- cpt_parameters$health_system_capacity *
  populations[1]
cpt_excess_deaths <- cpt_parameters$empirical_fatalities

# Johannesburg hospital capacity and excess deaths

jhb_parameters <- fromJSON(file = "johannesburg/parameters.json")
jhb_hospital_capacity <- jhb_parameters$health_system_capacity *
  populations[2]
jhb_excess_deaths <- jhb_parameters$empirical_fatalities

# SCRIPTS ---------------------------------------------------------------------

run_01_tables <- T
run_02_figure_functions <- T
run_03_figures <- T


# RUN SCRIPTS -----------------------------------------------------------------

# Render tables
if (run_01_tables) source(here("scripts", "01_tables.R"))

# Create functions to render figures
if (run_02_figure_functions) source(here("scripts", "02_figure_functions.R"))

# Render figures
if (run_03_figures) source(here("scripts", "03_figures.R"))
