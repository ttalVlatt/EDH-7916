## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling I: Enter the tidyverse]
##' [INIT: 20 January 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [UPDT: 12 January 2025]
##
## -----------------------------------------------------------------------------

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

## ---------------------------
##' [Read in Data]
## ---------------------------

## data are CSV, so we use read_csv() from the readr library
df <- read_csv(file.path("data", "hsls-small.csv"))

## ---------------------------
##' [The Pipe |> Operator]
## ---------------------------

## Without |>
select(df, x1txmtscor)

## With |>
df |> select(x1txmtscor)

## Without |>
filter(select(df, x1txmtscor), x1txmtscor > 50)

## With |>
df |> select(x1txmtscor) |> filter(x1txmtscor > 50)

## Without |>
mutate(filter(select(df, x1txmtscor), x1txmtscor > 50), square_root = sqrt(x1txmtscor))

## With |>
df |> select(x1txmtscor) |> filter(x1txmtscor > 50) |> mutate(square_root = sqrt(x1txmtscor))

## Best to use  a new line for each pipe when code gets longer
df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

## Without the |>, we could technically break it down step by step
temp <- select(df, x1txmtscor)
temp <- filter(temp, x1txmtscor > 50)
temp <- mutate(temp, square_root = sqrt(x1txmtscor))
temp

## Always assign backwards
df_backward_pass <- df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

## You can think of the assignment as a continuation of the pipe like this
## but don't write it this way, it's then hard to find what you called something later
df |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor)) ->
  df_forward_pass

## Checking they are the same
all.equal(df_backward_pass, df_forward_pass)

## ---------------------------
##' [Select Variables]
## ---------------------------

df |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

df_small <- df |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

## ---------------------------
##' [Count Categorical Variables]
## ---------------------------

## see unique values for student expectation
df_small |> count(x1stuedexpct)

## see unique values for parental expectation
df_small |> count(x1paredexpct)

## ---------------------------
##' [Modify an Existing Variable with mutate & ifelse]
## ---------------------------

df_small <- df_small |>
  mutate(x1stuedexpct = ifelse(x1stuedexpct == -8, NA, x1stuedexpct))

print(df_small, n = 26)

## ---------------------------
##' [Being Efficient with %in% and c()]
## ---------------------------

df_small <- df_small |>
  mutate(x1stuedexpct = ifelse(x1stuedexpct %in% c(-8, -9, 11), NA, x1stuedexpct),
         x1paredexpct = ifelse(x1paredexpct %in% c(-8, -9, 11), NA, x1paredexpct))

print(df_small, n = 26)

df_small |> count(x1stuedexpct) 
df_small |> count(x1paredexpct)

## ---------------------------
##' [Create New Variable with mutate]
## ---------------------------

df_small <- df_small |>
  mutate(high_exp = ifelse(x1stuedexpct > x1paredexpct, x1stuedexpct, x1paredexpct))

print(df_small, n = 26)

mean(c(5, 6, 4, NA))

mean(c(5, 6, 4, NA), na.rm = T)

## ---------------------------
##' [Handling NA Values]
## ---------------------------

df_small <- df_small |>
  mutate(high_exp = ifelse(is.na(high_exp) & !is.na(x1stuedexpct), x1stuedexpct, high_exp),
         high_exp = ifelse(is.na(high_exp) & !is.na(x1paredexpct), x1paredexpct, high_exp))

print(df_small, n = 26)

## get summary of our new variable
df_small |> count(high_exp)

## filter out missing values
df_small_cut <- df_small |> filter(!is.na(high_exp))

df_small_cut |> count(high_exp)

## does the original # of rows - current # or rows == NA in count?
nrow(df_small) - nrow(df_small_cut)

## ---------------------------
##' [Summarizing Data]
## ---------------------------

## get average (without storing)
df_small_cut |> summarize(mean(high_exp))

df_small_cut |> summarize(mean_exp = mean(high_exp))

## ---------------------------
##' [Grouping Data]
## ---------------------------

## get grouped average
df_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp))

## ---------------------------
##' [Saving Data]
## ---------------------------

## write with useful name

df_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp)) |>
  write_csv(file.path("data", "region-expects.csv"))


## ---------------------------
##' [Appendix: All in One!]
## ---------------------------

## Let's redo the analysis above, but with a fully chained set of
## functions.

## start with original df
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
  ## Drop is high_exp is still NA (neither parent or student answereed)
  filter(!is.na(high_exp)) |>
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp)) |>
  ## Write that to a .csv file
  write_csv(file.path("data", "region-expects-chain.csv"))
  


non_chain <- read_csv(file.path("data", "region-expects.csv"))
chain <- read_csv(file.path("data", "region-expects-chain.csv"))
 
all.equal(non_chain, chain)
      


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
