# --------------------------------------------------------------------------------

# Author: Allan Davids
# Email: allan.davids@uct.ac.za
# Date created: 1 December 2020
# Date last revised: 23 March 2021
# Purpose
# This script sets directories and variables
# Call scripts to create functions, figures and tables

# --------------------------------------------------------------------------------

# SET DIRECTORIES ----------------------------------------------------------------

setwd("/Users/Allan/Dropbox/Research/sabcom/sabcom-paper")

base_data_directory <- "/Users/Allan/Dropbox/Research/sabcom/sabcom-paper/simulation_output"

output_folder <- "/Users/Allan/Dropbox/Research/sabcom/sabcom-paper/results"

# PACKAGES -----------------------------------------------------------------------

libs <- c("tidyverse", "data.table", "RColorBrewer", "sf", "tmap", "knitr", "xtable", "rjson", "zoo", "gmodels", "here")

lapply(libs, require, character.only = TRUE)

# SET PARAMETERS -----------------------------------------------------------------

learning <- c("de_groot")

cities <- c("cape_town")

scenarios <- c("lockdown", "no_intervention")

model_population <- 100000

populations <- c(3740026)

cpt_parameters <- fromJSON(file = "~/Dropbox/Research/SABCoM/sabcom-paper/cape_town/parameters.json")
cpt_hospital_capacity <- cpt_parameters$health_system_capacity * populations[1]
cpt_excess_deaths <- cpt_parameters$empirical_fatalities[-c(1:19)]


# SCRIPTS ------------------------------------------------------------------------

run_01_tables <- T
run_02_figure_functions <- T
run_03_figures <- T

# RUN SCRIPTS --------------------------------------------------------------------

# Render tables
if (run_01_tables) source(paste(getwd(), "scripts", "01_tables.R", sep = "/"))

# Create functions to render figures
if (run_02_figure_functions) source(paste(getwd(), "scripts", "02_figure_functions.R", sep = "/"))

# Render figures
if (run_03_figures) source(paste(getwd(), "scripts", "03_figures.R", sep = "/"))
