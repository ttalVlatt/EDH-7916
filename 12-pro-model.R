## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Modeling Basics]
##' [INIT: Jan 16th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(estimatr)


## ---------------------------
##' [Data Prep]
## ---------------------------

df <- read_csv(file.path("data", "hsls-small.csv"))

df <- df |>
  select(stu_id, x1sex, x1race, x1txmtscor, x1paredu, x1ses, x1poverty185) |>
  filter(! if_any(.cols = everything(),
                  .fns = ~ . %in% c(-8, -9))) |>
  mutate(across(.cols = ! c(x1txmtscor, x1ses),
                .fns = ~ as.factor(.)))


## ---------------------------
##' [t-tests]
## ---------------------------

t.test(x1txmtscor ~ x1sex, data = df)

## ---------------------------
##' [Regression]
## ---------------------------

lm_sex <- lm(x1txmtscor ~ x1sex + x1poverty185 + x1paredu, data = df)
summary(lm_sex)

## ---------------------------
##' [Predictions with Regression]
## ---------------------------

predictions <- predict(lm_sex)

df_unknown <- df |>
  slice_sample(prop = 0.1) |>
  select(-x1txmtscor)

df_train <- df |>
  anti_join(df_unknown, by = "stu_id")

lm_sex_train <- lm(x1txmtscor ~ x1sex + x1poverty185 + x1paredu, data = df_train)

predictions_new <- predict(lm_sex_train, newdata = df_unknown)


## ---------------------------
##' [Extracting Residuals from Regression]
## ---------------------------

residuals <- lm_sex[["residuals"]]

ggplot() +
  geom_histogram(aes(x = residuals),
                 color = "black",
                 fill = "skyblue")

## ---------------------------
##' [Formula Objects]
## ---------------------------


matts_form <- formula(x1txmtscor ~ x1sex + x1poverty185 + x1paredu)

lm_sex <- lm(matts_form, data = df)
summary(lm_sex)

lm_sex_robust <- lm_robust(matts_form, data = df)
summary(lm_sex_robust)


## ---------------------------
##' [Modelling with Loops]
## ---------------------------


outcomes <- c("x1txmtscor", "x1ses")

for(i in outcomes) {
  
  loop_form <- formula(paste0(i, "~ x1sex + x1poverty185 + x1paredu"))
  
  loop_lm <- lm(loop_form, data = df)
  
  print(summary(loop_lm))
  
}

