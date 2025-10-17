# Code 1: Data Process ----

## Settings ----
source("02 Code/0.1 Functions.R")
source("02 Code/0.2 Settings.R")

# Data path 
data_inp <- "01 Data/Input/"
data_out <- "01 Data/Output/"

## Load Data ----
cont <- rio::import(paste0(data_inp, "births_exposition_pm25_o3_kriging_idw", ".RData")) # 2010 - 2020
hw <- rio::import(paste0(data_inp, "births_1992_2020_last_week_hw", ".RData")) # 1992 - 2020


## Explore Data ----

hw <- hw |> arrange(id)
cont <- cont |> arrange(id)

glimpse(hw)
glimpse(cont)

duplicated(hw$id) |> sum()
duplicated(cont$id) |> sum()

## Join data ----

hw <- hw |> select(id, HW_30C_2d_bin:P99_min_4d_count)
glimpse(hw)

cont_hw <- cont |> 
  left_join(hw, by = c("id")) |> 
  drop_na()

glimpse(cont_hw)
summary(cont_hw)

## Save data ----
rio::export(cont_hw, paste0(data_out, "births_2010_2020_last_week_summer_hw_cont", ".RData")) # 2010 - 2020