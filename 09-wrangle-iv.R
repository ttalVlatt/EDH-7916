## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Tidyverse Tricks & SQL]
##' [INIT: Jan 12th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(dbplyr)
library(RSQLite)


## ---------------------------
##' [Tidyverse Tricks Data]
## ---------------------------

df_18_pub <- read_csv(file.path("data", "ipeds-finance", "f1819_f1a_rv.csv"))
df_18_np <- read_csv(file.path("data", "ipeds-finance", "f1819_f2_rv.csv"))
df_18_fp <- read_csv(file.path("data", "ipeds-finance", "f1819_f3_rv.csv"))

df_18 <- bind_rows(df_18_pub, df_18_np, df_18_fp)

df_18 |>
  count(UNITID) |>
  filter(n > 1)


## ---------------------------
##' [coalesce()-ing Split Data]
## ---------------------------

df_18 <- df_18 |>
  select(UNITID,
         F1C011, F1C021, F1C061,
         F2E011, F2E021, F2E051,
         F3E011, F3E02A1, F3E03B1)

print(df_18[100:105,])
print(df_18[3000:3005,])


## Split back up into separate files
pub <- df_18 |> filter(!is.na(F1C011)) |>
  ## Rename the variable
  rename(inst_spend = F1C011) |>
  ## Drop the other variables
  select(UNITID, inst_spend)
np <- df_18 |> filter(!is.na(F2E011)) |>
  rename(inst_spend = F2E011) |>
  select(UNITID, inst_spend)
fp <- df_18 |> filter(!is.na(F3E011)) |>
  rename(inst_spend = F3E011) |>
  select(UNITID, inst_spend)
## Re-bind the colleges back up
rebind <- bind_rows(pub, np, fp)


all.equal(rebind, coalesce)

df_18_clean <- df_18 |>
  mutate(inst_spend = coalesce(F1C011, F2E011, F3E011),
         rsch_spend = coalesce(F1C021, F2E021, F3E02A1),
         serv_spend = coalesce(F1C061, F2E051, F3E03B1)) |>
  select(UNITID, inst_spend, rsch_spend, serv_spend)

print(df_18_clean[100:105,])
print(df_18_clean[3000:3005,])

## ---------------------------
##' [Finding if_any() Issues]
## ---------------------------

df_0_inst <- df_18_clean |> filter(inst_spend == 0)
df_0_rsch <- df_18_clean |> filter(rsch_spend == 0)
df_0_serv <- df_18_clean |> filter(serv_spend == 0)
df_0 <- bind_rows(df_0_inst, df_0_rsch, df_0_serv)

## Plus we end up with duplicates
df_0 |>
  count(UNITID) |>
  filter(n > 1)


df_0 <- df_18_clean |>
  filter(if_any(everything(), ~ . == 0)) ## h/t https://stackoverflow.com/questions/69585261/dplyr-if-any-and-numeric-filtering

print(df_0)

## ---------------------------
##' [Working across() Columns]
## ---------------------------

df_0 |>
  select(-UNITID) |>
  count(across(everything(), ~ . == 0))

## ---------------------------
##' [From ifelse() to case_when()]
## ---------------------------

df_18_clean |>
  mutate(highest_cat = case_when(inst_spend > rsch_spend & rsch_spend > serv_spend ~ "inst_rsch_serv",
                                 inst_spend > serv_spend & serv_spend > rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend > inst_spend & inst_spend > serv_spend ~ "rsch_inst_serv",
                                 rsch_spend > serv_spend & serv_spend > inst_spend ~ "rsch_serv_inst",
                                 serv_spend > inst_spend & inst_spend > rsch_spend ~ "serv_inst_rsch",
                                 serv_spend > rsch_spend & rsch_spend > inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ "You missed a condition Matt")) |>
  count(highest_cat)

df_18_clean |>
  mutate(highest_cat = case_when(inst_spend >= rsch_spend & rsch_spend >= serv_spend ~ "inst_rsch_serv",
                                 inst_spend >= serv_spend & serv_spend >= rsch_spend ~ "inst_serv_rsch",
                                 rsch_spend >= inst_spend & inst_spend >= serv_spend ~ "rsch_inst_serv",
                                 rsch_spend >= serv_spend & serv_spend >= inst_spend ~ "rsch_serv_inst",
                                 serv_spend >= inst_spend & inst_spend >= rsch_spend ~ "serv_inst_rsch",
                                 serv_spend >= rsch_spend & rsch_spend >= inst_spend ~ "serv_rsch_inst",
                                 TRUE ~ "You missed a condition Matt")) |>
  count(highest_cat)

## ---------------------------
##' [Data Wrangling II in SQL]
## ---------------------------

## ---------------------------
##' [dbplyr SQL Setup]
## ---------------------------


df <- read_csv(file.path("data", "sch-test", "all-schools.csv"))

microsoft_access <- simulate_access()

db <- memdb_frame(df)


## ---------------------------
##' [Create Summary Table]
## ---------------------------

# https://stackoverflow.com/questions/76724279/syntax-highlight-quarto-output
df_sum <- db |>
    ## grouping by year so average within each year
    group_by(year) |>
    ## get mean(<score>) for each test
    summarize(math_m = mean(math),
              read_m = mean(read),
              science_m = mean(science)) |>
  show_query()

## ---------------------------
##' [Left-Join]
## ---------------------------

df_joined <- db |>
    ## pipe into left_join to join with df_sum using "year" as key
    left_join(df_sum, by = "year") |>
  show_query()

## ---------------------------
##' [Pivot-Longer]
## ---------------------------

df_long <- db |>
    ## cols: current test columns
    ## names_to: where "math", "read", and "science" will go
    ## values_to: where the values in cols will go
    pivot_longer(cols = c("math","read","science"),
                 names_to = "test",
                 values_to = "score") |>
  show_query()

## ---------------------------
##' [Pivot-Wider]
## ---------------------------

df_wide <- df_long |>
    ## names_from: values in this column will become new column names
    ## values_from: values in this column will become values in new cols
    pivot_wider(names_from = "test",
                values_from = "score") |>
  show_query()

## ---------------------------
##' [Data Wrangling I in SQL]
## ---------------------------

df <- read_csv(file.path("data", "hsls-small.csv"))

df |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answered)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp))

## ---------------------------
##' [dbplyr SQL Setup]
## ---------------------------


microsoft_access <- simulate_access()

db <- memdb_frame(df)



db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answereed)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp)) |>
  show_query()


## ---------------------------
##' [Step-by-Step Breakdown]
## ---------------------------

db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  show_query()

db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  show_query()


db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  show_query()


db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  show_query()

db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answereed)
  filter(!is.na(high_exp)) |>
  show_query()

db |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(student_exp = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         parent_exp = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(student_exp > parent_exp, student_exp, parent_exp)) |>
  ## If one exp is NA but the other isn't, keep the value not the NA
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(student_exp), student_exp, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(parent_exp), parent_exp, high_exp)) |>
  ## Drop is high_exp is still NA (neither parent or student answered)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp)) |>
  show_query()

## ---------------------------
##' [Pivot-Longer with Compound Names]
## ---------------------------


df_3 <- read_csv(file.path("data", "sch-test", "all-schools-wide.csv"))

db_3 <- memdb_frame(df_3)

print(db_3)


## Note: change from DWII,dbplyr can't translate separate, or any stringr commands, so we have to be more sophisticated with our pivot_longer
df_long_fix <- db_3 |>
    ## NB: contains() looks for "19" in name: if there, it adds it to cols
    pivot_longer(cols = contains("19"),
                 names_to = c("test", "year"),
                 names_sep = "_",
                 values_to = "score") |>
  show_query()

