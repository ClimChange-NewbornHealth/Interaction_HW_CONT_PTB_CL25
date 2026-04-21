# Code 2: Toy Model ----

## Settings ----
source("02 Code/0.1 Functions.R")
source("02 Code/0.2 Settings.R")

# Data path 
data_inp <- "01 Data/Input/"
data_out <- "01 Data/Output/"
models_output <- "03 Output/Models/"

## Load Data ----
births_hw_o3 <- rio::import(paste0(data_out, "births_2010_2020_last_week_hw_o3", ".RData"))
glimpse(births_hw_o3)

## Toy models example mediation ----

