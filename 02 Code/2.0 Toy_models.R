# Code 2: Toy Models ----

## Settings ----
source("02 Code/0.1 Functions.R")
source("02 Code/0.2 Settings.R")

# Data path 
data_inp <- "01 Data/Input/"
data_out <- "01 Data/Output/"
models_output <- "03 Output/Models/"

## Load Data ----
cont_hw <- rio::import(paste0(data_out, "births_2010_2020_last_week_summer_hw_cont", ".RData")) # 2010 - 2020
glimpse(cont_hw)

cont_metrics <- cont_hw |> 
  select(starts_with("pm25_"), starts_with("o3_")) |>
  colnames()

cont_metrics 

hw_metrics <- cont_hw |> 
  select(starts_with("HW_")) |>
  colnames()

hw_metrics

## Toy Models ----

# Base line functions estimate models for Cox and Logistic regression
# source("02 Code/0.1 Functions.R") # In this file include functions fit_cox_model and fit_logit_model for fitting models

# Examples of function calls:
# Fit COX model
m1_cox <- fit_cox_model(
  dependent = "birth_preterm", 
  predictor = "pm25_krg_full", 
  data = cont_hw, 
  adjustment = "Adjusted", 
  interaction = "HW_30C_3d_bin")

#View(m1_cox)

# Save excel file
m1_cox |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 4, decimal.mark = ","))) |> 
  rio::export(paste0(models_output, "Model_cox_pm25_krg_hw30C_3d.xlsx"))

# Fit LOGIT model
m1_logit <- fit_logit_model(
  dependent = "birth_preterm", 
  predictor = "pm25_krg_full", 
  data = cont_hw, 
  adjustment = "Adjusted", 
  interaction = "HW_30C_3d_bin")

#View(m1_logit)

# Save excel file
m1_logit |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 4, decimal.mark = ","))) |> 
  rio::export(paste0(models_output, "Model_logit_pm25_krg_hw30C_3d.xlsx"))

