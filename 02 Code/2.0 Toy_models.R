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

control_vars <- c(
  "sex", "age_group_mom", "educ_group_mom", "job_group_mom",
  "age_group_dad", "educ_group_dad", "job_group_dad",
  "month_week1_f", "year_week1_f", "covid", "vulnerability"
)

# factor() inside formulas breaks some bootstrap mediators; use explicit factor columns
med_data <- births_hw_o3 |>
  dplyr::mutate(
    month_week1_f = factor(month_week1),
    year_week1_f = factor(year_week1)
  ) |>
  dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ droplevels(.x)))

rhs_controls <- paste(control_vars, collapse = " + ")

need <- c(
  "weeks", "birth_preterm",
  "HW_EHF_TAD_3d_count", "o3_krg_7d",
  control_vars
)
med_data <- med_data[stats::complete.cases(med_data[, need]), , drop = FALSE]

## ---- Mediation-style strategy (regression-based path, Cox for time-to-event) ----
# Exposure: consecutive heat-wave days (EHF, TAD-based, min. 3-day duration), HW_EHF_TAD_3d_count
# Mediator: mean O3 (kriging) over last gestational week, o3_krg_7d
# Outcome: preterm birth as event in Surv(weeks, birth_preterm)
#
# Total effect (TE): hazard for PTB as a function of HW (and covariates).
# Controlled direct effect (CDE): same model adding the mediator O3 (HW + O3 + covariates).
# Mediator model (path M): O3 as a function of HW and covariates (linear regression for a
# continuous O3 summary).
#
# Natural direct / indirect effects on the hazard scale with a Cox outcome typically need
# g-methods or specialized packages; this script reports the three fitted models and a
# simple contrast of the HW coefficient with vs without O3 (log-hazard scale). The ratio
# 1 - beta_cde/beta_te is only a rough heuristic when both coefficients are small or
# change sign (not bounded to [0,1]).

f_med <- stats::as.formula(paste("o3_krg_7d ~ HW_EHF_TAD_3d_count +", rhs_controls))
fit_mediator <- stats::lm(f_med, data = med_data)

f_tot <- stats::as.formula(paste("Surv(weeks, birth_preterm) ~ HW_EHF_TAD_3d_count +", rhs_controls))
fit_cox_total <- survival::coxph(f_tot, data = med_data, ties = "efron")

f_cde <- stats::as.formula(paste(
  "Surv(weeks, birth_preterm) ~ HW_EHF_TAD_3d_count + o3_krg_7d +", rhs_controls
))
fit_cox_cde <- survival::coxph(f_cde, data = med_data, ties = "efron")

tbl_med <- broom::tidy(fit_mediator, conf.int = TRUE)
tbl_te <- broom::tidy(fit_cox_total, exponentiate = TRUE, conf.int = TRUE)
tbl_cde <- broom::tidy(fit_cox_cde, exponentiate = TRUE, conf.int = TRUE)

hw_name <- "HW_EHF_TAD_3d_count"
beta_hw_te <- stats::coef(fit_cox_total)[hw_name]
beta_hw_cde <- stats::coef(fit_cox_cde)[hw_name]
prop_delta_log_hr <- if (abs(beta_hw_te) > .Machine$double.eps) {
  1 - beta_hw_cde / beta_hw_te
} else {
  NA_real_
}

mediation_summ <- tibble::tibble(
  description = c(
    "Log HR for heat wave (total; TE path)",
    "Log HR for heat wave | O3 (controlled direct path)",
    "Heuristic contrast on log-HR scale: 1 - (beta_HW | O3) / (beta_HW) (use with care)"
  ),
  value = c(beta_hw_te, beta_hw_cde, prop_delta_log_hr)
)

print(tbl_med)
print(tbl_te)
print(tbl_cde)
print(mediation_summ)

## ---- Interaction (effect modification; hazards on multiplicative scale) ----
# HW days * O3 (last week mean); O3 scaled to one interquartile range for interpretability
o3_iqr <- stats::IQR(med_data$o3_krg_7d, na.rm = TRUE, type = 7)
if (!is.finite(o3_iqr) || o3_iqr < .Machine$double.eps) o3_iqr <- 1
med_data$o3_krg_7d_iqrsc <- med_data$o3_krg_7d / o3_iqr

f_int <- stats::as.formula(paste(
  "Surv(weeks, birth_preterm) ~ HW_EHF_TAD_3d_count * o3_krg_7d_iqrsc +", rhs_controls
))
fit_cox_int <- survival::coxph(f_int, data = med_data, ties = "efron")
tbl_int <- broom::tidy(fit_cox_int, exponentiate = TRUE, conf.int = TRUE)
print(tbl_int)

# Binary cross-classification (for descriptive cells analogous to 2x2 strata; reference =
# no heatwave days & O3 at or below median), optional for stratified inspection
med_data <- med_data |>
  dplyr::mutate(
    hw_bin = as.integer(HW_EHF_TAD_3d_count > 0),
    o3_hi = as.integer(o3_krg_7d > stats::median(o3_krg_7d, na.rm = TRUE))
  )

f_4cell <- stats::as.formula(paste(
  "Surv(weeks, birth_preterm) ~ factor(interaction(hw_bin, o3_hi)) +", rhs_controls
))
fit_cox_4 <- survival::coxph(f_4cell, data = med_data, ties = "efron")
tbl_4cell <- broom::tidy(fit_cox_4, exponentiate = TRUE, conf.int = TRUE)
print(tbl_4cell)

# Note: relative excess risk due to interaction (RERI) is defined for binomial risk / linear
# odds or risk scales; with Cox hazards, report interaction on the log-hazard scale or use
# a binomial model for PTB as a sensitivity if RERI on the additive scale is required.

## Export results (Excel) ----
fmt_lm_tbl <- function(x) {
  x |>
    dplyr::transmute(
      Term = .data$term,
      `Coefficient` = round(.data$estimate, 5),
      `SE` = round(.data$std.error, 5),
      `95% CI lower` = round(.data$conf.low, 5),
      `95% CI upper` = round(.data$conf.high, 5),
      `p-value` = signif(.data$p.value, 4)
    )
}

fmt_cox_tbl <- function(x) {
  x |>
    dplyr::transmute(
      Term = .data$term,
      `Hazard ratio` = round(.data$estimate, 4),
      `SE (log HR)` = round(.data$std.error, 5),
      `95% CI lower` = round(.data$conf.low, 4),
      `95% CI upper` = round(.data$conf.high, 4),
      `p-value` = signif(.data$p.value, 4)
    )
}

overview <- tibble::tibble(
  Field = c(
    "Script",
    "Outcome",
    "Exposure (heat)",
    "Mediator (ozone)",
    "Adjustment (covariates)",
    "Complete-case sample size",
    "O3 interquartile range (kriging weekly mean, same units as o3_krg_7d)",
    "Interaction scaling",
    "Notes"
  ),
  Description = c(
    "Toy_models: mediation-style path + Cox interaction",
    "Gestational time to preterm birth: Surv(weeks, birth_preterm)",
    "HW_EHF_TAD_3d_count (EHF-TAD, minimum 3-day duration definition)",
    "o3_krg_7d (mean daily O3, kriging) over last gestational week window",
    paste(control_vars, collapse = ", "),
    as.character(nrow(med_data)),
    format(signif(o3_iqr, 6), scientific = FALSE),
    "In interaction model, o3_krg_7d_iqrsc = o3_krg_7d / IQR; HRs are per IQR difference in O3",
    paste0(
      "Cox TE = hazard of PTB vs heat exposure alone; CDE = same with O3 in model. ",
      "Log-HR contrast is heuristic (not formal natural direct/indirect decomposition on the hazard scale)."
    )
  )
)

mediation_table <- mediation_summ |>
  dplyr::transmute(
    `Summary measure` = .data$description,
    Value = round(as.numeric(.data$value), 6)
  )

primary_rows <- dplyr::bind_rows(
  tbl_med |>
    dplyr::filter(.data$term == "HW_EHF_TAD_3d_count") |>
    fmt_lm_tbl() |>
    dplyr::mutate(Block = "Mediator model: O3 ~ heat + covariates"),
  tbl_te |>
    dplyr::filter(.data$term == "HW_EHF_TAD_3d_count") |>
    fmt_cox_tbl() |>
    dplyr::mutate(Block = "Cox — total effect (PTB ~ heat + covariates)"),
  tbl_cde |>
    dplyr::filter(.data$term %in% c("HW_EHF_TAD_3d_count", "o3_krg_7d")) |>
    fmt_cox_tbl() |>
    dplyr::mutate(Block = "Cox — controlled direct (PTB ~ heat + O3 + covariates)"),
  tbl_int |>
    dplyr::filter(grepl("HW_EHF|o3_krg|:", .data$term)) |>
    fmt_cox_tbl() |>
    dplyr::mutate(Block = "Cox — interaction (multiplicative hazards)")
) |>
  dplyr::relocate("Block")

dir.create(models_output, showWarnings = FALSE, recursive = TRUE)
out_xlsx <- file.path(models_output, "Toy_models_mediation_interaction_results.xlsx")

sheet_list <- list(
  Overview = overview,
  Primary_estimates = primary_rows,
  Mediation_contrast = mediation_table,
  Mediator_model_full = fmt_lm_tbl(tbl_med),
  Cox_total_effect_full = fmt_cox_tbl(tbl_te),
  Cox_controlled_direct_full = fmt_cox_tbl(tbl_cde),
  Cox_interaction_full = fmt_cox_tbl(tbl_int),
  Cox_four_strata_full = fmt_cox_tbl(tbl_4cell)
)

openxlsx::write.xlsx(sheet_list, out_xlsx, overwrite = TRUE)
message("Saved: ", normalizePath(out_xlsx, winslash = "/", mustWork = FALSE))
