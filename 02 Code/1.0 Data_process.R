# Code 1: Data Process ----

## Settings ----
source("02 Code/0.1 Functions.R")
source("02 Code/0.2 Settings.R")

# Data path 
data_inp <- "01 Data/Input/"
data_out <- "01 Data/Output/"

## Load Data ----
births_long <- rio::import(paste0(data_inp, "births_2010_2020_weeks_long", ".RData"))
glimpse(births_long)

births <- births_long |> 
  group_by(id) |> 
  filter(week_gest_num == max(week_gest_num)) |> 
  ungroup() 

births_temp <- births_long |> 
  select(id, week_gest_num, date_start_week, date_end_week) 

glimpse(births) # 713918

rm(births_long)

## Process Heat Wave Data ----
hw <- rio::import(paste0(data_inp, "hw_data_1980_2021", ".RData")) 
glimpse(hw)

# Optimize with data.table
setDT(births)
setDT(hw)

# Join tables per range
births[, start := date_start_week]
births[, end := date_end_week]

# Interval join `hw_data` 
hw[, start := date]
hw[, end := date]

# Set keys 
setkey(hw, name_com, start, end)
setkey(births, name_com, start, end)

# Time estimation bw last month 
invisible(gc())                             # clean RAM
while (dev.cur() > 1) dev.off()             # close plots
closeAllConnections()            # close connections    

tic("Find hw in latest week of gestation") # Time estimation
hw_lw <- foverlaps(hw, births, type = "any", nomatch = 0) %>%
  .[, .(
    HW_p90_2d_bin = as.integer(any(HW_p90_2d > 0, na.rm = TRUE)),
    HW_p95_2d_bin = as.integer(any(HW_p95_2d > 0, na.rm = TRUE)),
    HW_p99_2d_bin = as.integer(any(HW_p99_2d > 0, na.rm = TRUE)),
    
    HW_p90_2d_count = sum(HW_p90_2d, na.rm = TRUE),
    HW_p95_2d_count = sum(HW_p95_2d, na.rm = TRUE),
    HW_p99_2d_count = sum(HW_p99_2d, na.rm = TRUE),

    HW_p90_3d_bin = as.integer(any(HW_p90_3d > 0, na.rm = TRUE)),
    HW_p95_3d_bin = as.integer(any(HW_p95_3d > 0, na.rm = TRUE)),
    HW_p99_3d_bin = as.integer(any(HW_p99_3d > 0, na.rm = TRUE)),
    
    HW_p90_3d_count = sum(HW_p90_3d, na.rm = TRUE),
    HW_p95_3d_count = sum(HW_p95_3d, na.rm = TRUE),
    HW_p99_3d_count = sum(HW_p99_3d, na.rm = TRUE),

    HW_p90_4d_bin = as.integer(any(HW_p90_4d > 0, na.rm = TRUE)),
    HW_p95_4d_bin = as.integer(any(HW_p95_4d > 0, na.rm = TRUE)),
    HW_p99_4d_bin = as.integer(any(HW_p99_4d > 0, na.rm = TRUE)),
    
    HW_p90_4d_count = sum(HW_p90_4d, na.rm = TRUE),
    HW_p95_4d_count = sum(HW_p95_4d, na.rm = TRUE),
    HW_p99_4d_count = sum(HW_p99_4d, na.rm = TRUE),

    HW_EHF_TAD_2d_bin = as.integer(any(HW_EHF_tad_2d > 0, na.rm = TRUE)),
    HW_EHF_TAD_3d_bin = as.integer(any(HW_EHF_tad_3d > 0, na.rm = TRUE)),
    HW_EHF_TAD_4d_bin = as.integer(any(HW_EHF_tad_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TAD_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TAD_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TAD_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE),

    HW_EHF_TMAX_2d_bin = as.integer(any(HW_EHF_tmax_2d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_3d_bin = as.integer(any(HW_EHF_tmax_3d > 0, na.rm = TRUE)),
    HW_EHF_TMAX_4d_bin = as.integer(any(HW_EHF_tmax_4d > 0, na.rm = TRUE)),
    
    HW_EHF_TMAX_2d_count = sum(HW_EHF_tad_2d, na.rm = TRUE),
    HW_EHF_TMAX_3d_count = sum(HW_EHF_tad_3d, na.rm = TRUE),
    HW_EHF_TMAX_4d_count = sum(HW_EHF_tad_4d, na.rm = TRUE)

  ), by = .(name_com, id, date_start_week, date_end_week)]
toc() # Time: ~8 sec elapsed 

glimpse(hw_lw)

births_hw <- births %>% 
  left_join(hw_lw, by=c("id", "name_com", "date_start_week", "date_end_week")) |> 
  select(-start, -end) |> 
  data.table::as.data.table() 

glimpse(births_hw)

## Process Ozone Data ----
o3 <- rio::import(paste0(data_inp, "pollution_2010_2020", ".RData")) # 2010 - 2020
glimpse(o3)

# Estimate O3 exposure in the last week of gestation
o3 <- o3 |> 
  rename(
        no2_idw = no2_idw_pred, 
        no2_krg = no2_ok_pred,
        pm25_idw = pm25_idw_pred,
        pm25_krg = pm25_ok_pred,
        o3_idw = o3_idw_pred,
        o3_krg = o3_ok_pred 
        ) |> 
  data.table::as.data.table() 

glimpse(o3)

calc_exposure_periods <- function(start_date, end_date, cont_data) {
  cont_data |>
    filter(date >= start_date, date <= end_date) |>
    dplyr::summarise(
      o3_krg      = mean(o3_krg,      na.rm = TRUE),
      o3_idw      = mean(o3_idw,      na.rm = TRUE)
    )
}

# Promedio de O3 entre date_start_week y date_end_week por comuna
births_dt <- data.table::copy(births_hw)
o3_dt <- data.table::copy(o3)

o3_week_avg <- o3[births_hw,
  on = .(com, date >= date_start_week, date <= date_end_week),
  .(o3_krg_7d = mean(o3_krg, na.rm = TRUE), o3_idw_7d = mean(o3_idw, na.rm = TRUE)),
  by = .EACHI]

births_hw_o3 <- births_hw |>
  dplyr::mutate(o3_krg_7d = o3_week_avg$o3_krg_7d, o3_idw_7d = o3_week_avg$o3_idw_7d)

glimpse(births_hw_o3)
summary(births_hw_o3)

## Add vulnerability index (VI) ----
sovi <- rio::import(paste0(data_inp, "sovi_datasets", ".RData")) |> 
  select(-name_comuna)  |> 
  rename(vulnerability=vulnerablidad) |> 
    mutate(vulnerability = fct_recode(vulnerability,
      "Low" = "Baja",
      "Medium-low" = "Medio-baja",
      "Medium-high" = "Medio-alta")) |> 
  rename(com=cod_com)

births_hw_o3 <- births_hw_o3 |> 
  left_join(sovi, by = "com") 

glimpse(births_hw_o3)


## Temperature exposure matrix (wide) ----

glimpse(births_temp)


## Save data ----
rio::export(births_hw_o3, paste0(data_out, "births_2010_2020_last_week_hw_o3", ".RData")) # 2010 - 2020