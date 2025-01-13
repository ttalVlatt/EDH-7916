## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Bringing It All Together feat. Basic Models]
##' [INIT: Jan 16th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())


## install.packages(c("tidymodels", "estimatr", "stargazer", "gtsummary"))


## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(tidymodels)
library(estimatr)
library(stargazer)
library(gtsummary)
library(gt)
library(knitr)


## ---------------------------
##' [Data Prep]
## ---------------------------

data <- read_csv("data/hsls-small.csv") |>
  select(stu_id, x1sex, x1race, x1txmtscor, x1paredu, x1ses, x1poverty185, x1paredexpct)

data <- data |>
  filter(! if_any(.cols = everything(),
                  .fns = ~ . %in% c(-8, -9)))

data <- data |>
  mutate(across(.cols = c(stu_id, x1sex, x1race, x1paredu, x1poverty185),
                .fns = ~ factor(.)))

## ---------------------------
##' [t-tests]
## ---------------------------

t.test(x1txmtscor ~ x1sex, data = data)

## ---------------------------
##' [Regression]
## ---------------------------

regression <- lm(x1txmtscor ~ x1sex + x1poverty185 + x1paredu, data = data)
summary(regression)

stargazer(regression, type = "html")

tbl_regression(regression)

tbl_regression(regression,
               label = list(x1sex ~ "Sex",
                            x1poverty185 ~ "Below Poverty Line",
                            x1paredu ~ "Parental Education")) |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) |>
  add_glance_source_note(include = c(r.squared, nobs)) |>
  modify_column_unhide(std.error)

summary_object <- summary(regression)

summary_object[["coefficients"]] |>
  as.data.frame() |>
  mutate(sig = case_when(`Pr(>|t|)` < 0.001 ~ "***",
                         `Pr(>|t|)` < 0.01 ~ "**",
                         `Pr(>|t|)` < 0.05 ~ "*",
                         TRUE ~ "")) |>
  kable(col.names = c("estimate", "s.e.", "t", "p", ""),
        digits = 3)


## ---------------------------
##' [Predictions with Regression]
## ---------------------------

data <- data |>
  mutate(prediction = predict(regression))

data |> select(stu_id, x1txmtscor, prediction)

ggplot(data,
       aes(x = prediction,
           y = x1txmtscor)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_obs_pred()


regression_2 <- lm(x1txmtscor ~ x1sex + x1ses + x1paredu, data = data)

data <- data |>
  mutate(prediction_2 = predict(regression_2))

ggplot(data,
       aes(x = prediction_2,
           y = x1txmtscor)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  coord_obs_pred()


data_outcome_unknown <- data |>
  slice_sample(prop = 0.1) |>
  select(-x1txmtscor)

data_outcome_known <-  anti_join(x = data, y = data_outcome_unknown, by = "stu_id")

regression_3 <- lm(x1txmtscor ~ x1sex + x1ses + x1paredu, data = data_outcome_known)

data_outcome_unknown <- data_outcome_unknown |>
  mutate(prediction_3 = predict(regression_3, newdata = data_outcome_unknown))

cor(data_outcome_unknown$prediction_2, data_outcome_unknown$prediction_3)

## ---------------------------
##' [Extracting Residuals from Regression]
## ---------------------------

data <- data |>
  mutate(residual = regression_2[["residuals"]])

ggplot(data) +
  geom_histogram(aes(x = residual),
                 color = "black",
                 fill = "skyblue")

## ---------------------------
##' [Formula Objects]
## ---------------------------


regression_formula <- formula(x1txmtscor ~ x1sex + x1ses + x1paredu)

regression_4 <- lm(regression_formula, data = data)

tbl_regression(regression_4,
               label = list(x1sex ~ "Sex",
                            x1ses ~ "Socio-Economic Status",
                            x1paredu ~ "Parental Education")) |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) |>
  add_glance_source_note(include = c(r.squared, nobs)) |>
  modify_column_unhide(std.error)

regression_robust <- lm_robust(regression_formula, data = data, se_type = "stata")

tbl_regression(regression_robust,
               label = list(x1sex ~ "Sex",
                            x1ses ~ "Socio-Economic Status",
                            x1paredu ~ "Parental Education")) |>
  add_significance_stars(hide_ci = FALSE, hide_p = FALSE) |>
  add_glance_source_note(include = c(r.squared, nobs)) |>
  modify_column_unhide(std.error)


## ---------------------------
##' [Modelling with Loops]
## ---------------------------

outcomes <- c("x1txmtscor", "x1paredexpct")

for(i in outcomes) {
  
  print(i)
  
  loop_formula <- formula(paste0(i, "~ x1sex + x1ses + x1paredu"))
  
  loop_regression <- lm(loop_formula, data = data)
  
  tbl_regression(loop_regression,
                 label = list(x1sex ~ "Sex",
                              x1ses ~ "Socio-Economic Status",
                              x1paredu ~ "Parental Education")) |>
    add_significance_stars(hide_ci = FALSE, hide_p = FALSE) |>
    add_glance_source_note(include = c(r.squared, nobs)) |>
    modify_column_unhide(std.error) |>
    print()
  
}


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
