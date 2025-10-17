# Functions ----

# Ajustadas: permiten probar interacciones entre 'predictor' (ej: "pm25")
# y una o varias variables HW (ej: c("HW_30C_2d_bin", "HW_P99_min_4d_count"))

fit_cox_model <- function(dependent, predictor, data, adjustment = "Adjusted", interaction = NULL) {

  rhs <- if (identical(adjustment, "Adjusted")) {
    paste(
      predictor,
      "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
      "age_group_dad + educ_group_dad + job_group_dad +",
      "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"
    )
  } else {
    predictor
  }

  if (!is.null(interaction)) {
    interaction <- as.character(interaction)
    for (iv in interaction) {
      rhs <- paste(rhs, "+", iv, "+", paste0(predictor, ":", iv))
    }
  }

  form <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", rhs))
  model_fit <- coxph(form, data = data, ties = "efron")

  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    dplyr::select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    dplyr::mutate(dependent_var = dependent, predictor = predictor, adjustment = adjustment,
                  interaction = ifelse(is.null(interaction), NA, paste(interaction, collapse = ",")))

  rm(model_fit); gc()
  
  return(results)
}

fit_logit_model <- function(dependent, predictor, data, conf.level = 0.95, adjustment = "Adjusted", interaction = NULL) {

  rhs <- if (identical(adjustment, "Adjusted")) {
    paste(
      predictor,
      " + sex + age_group_mom + educ_group_mom + job_group_mom +",
      " age_group_dad + educ_group_dad + job_group_dad +",
      " factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"
    )
  } else {
    predictor
  }

  if (!is.null(interaction)) {
    interaction <- as.character(interaction)
    for (iv in interaction) {
      rhs <- paste(rhs, "+", iv, "+", paste0(predictor, ":", iv))
    }
  }

  fml <- as.formula(paste0(dependent, " ~ ", rhs))
  model_fit <- glm(fml, data = data, family = binomial(link = "logit"))

  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  z   <- qnorm(1 - (1 - conf.level) / 2)

  tbl <- tbl |>
    dplyr::mutate(
      or        = exp(estimate),
      conf.low  = exp(estimate - z * std.error),
      conf.high = exp(estimate + z * std.error),
      estimate  = or
    )

  results <- tbl |>
    dplyr::select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    dplyr::mutate(dependent_var = dependent, predictor = predictor, adjustment = adjustment,
                  interaction = ifelse(is.null(interaction), NA, paste(interaction, collapse = ",")))

  rm(model_fit); gc()

  return(results)
}

