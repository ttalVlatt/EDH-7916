## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling I: Enter the tidyverse]
##' [INIT: 20 January 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [EDIT: Jue Wu]
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
data <- read_csv("data/hsls-small.csv")

## alternatively, you can also use read_csv(file.path("data", "hsls-small.csv"))

## ---------------------------
##' [The Native Pipe |> Operator]
## ---------------------------

## Without |>
select(data, x1txmtscor)

## With |>
data |> select(x1txmtscor)

## Without |>
filter(select(data, x1txmtscor), x1txmtscor > 50)

## With |>
data |> select(x1txmtscor) |> filter(x1txmtscor > 50)

## Without |>
mutate(filter(select(data, x1txmtscor), x1txmtscor > 50), square_root = sqrt(x1txmtscor))

## With |>
data |> select(x1txmtscor) |> filter(x1txmtscor > 50) |> mutate(square_root = sqrt(x1txmtscor))

## Best to use  a new line for each pipe when code gets longer
data |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

## Without the |>, we could technically break it down step by step
temp <- select(data, x1txmtscor)
temp <- filter(temp, x1txmtscor > 50)
temp <- mutate(temp, square_root = sqrt(x1txmtscor))
temp

## Always assign backwards
data_backward_pass <- data |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor))

## You can think of the assignment as a continuation of the pipe like this
## but don't write it this way, it's then hard to find what you called something later
data |>
  select(x1txmtscor) |>
  filter(x1txmtscor > 50) |>
  mutate(square_root = sqrt(x1txmtscor)) ->
  data_forward_pass

## Checking they are the same
all.equal(data_backward_pass, data_forward_pass)

## ---------------------------
##' [Select Variables]
## ---------------------------

data |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

data_small <- data |> select(stu_id, x1stuedexpct, x1paredexpct, x1region)

## ---------------------------
##' [Count Categorical Variables]
## ---------------------------

## see unique values for student expectation
data_small |> count(x1stuedexpct)

## see unique values for parental expectation
data_small |> count(x1paredexpct)

## ---------------------------
##' [Modify an Existing Variable with mutate & ifelse]
## ---------------------------

data_small <- data_small |>
  mutate(x1stuedexpct = ifelse(x1stuedexpct == -8, NA, x1stuedexpct))

print(data_small, n = 26)

## ---------------------------
##' [Being Efficient with %in% and c()]
## ---------------------------

data_small <- data_small |>
  mutate(x1stuedexpct = ifelse(x1stuedexpct %in% c(-8, -9, 11), NA, x1stuedexpct),
         x1paredexpct = ifelse(x1paredexpct %in% c(-8, -9, 11), NA, x1paredexpct))

print(data_small, n = 26)

data_small |> count(x1stuedexpct) 
data_small |> count(x1paredexpct)

## ---------------------------
##' [Create New Variable with mutate]
## ---------------------------

data_small <- data_small |>
  mutate(high_exp = ifelse(x1stuedexpct > x1paredexpct, x1stuedexpct, x1paredexpct))

print(data_small, n = 26)

mean(c(5, 6, 4, NA))

mean(c(5, 6, 4, NA), na.rm = T)

## ---------------------------
##' [Handling NA Values and Filter NA Rows]
## ---------------------------

data_small_cut <- data_small |>
  filter(!is.na(x1stuedexpct) & !is.na(x1paredexpct))


data_small_cut |> count(x1stuedexpct) 
data_small_cut |> count(x1paredexpct)

## what's the difference between original # of rows and current # or rows?
nrow(data_small) - nrow(data_small_cut)

print(data_small_cut, n = 26)

data_small |> count(high_exp)
data_small_cut |> count(high_exp)

## ---------------------------
##' [Simply Dropping NAs]
## ---------------------------

data_small_drop <- data_small |> 
  drop_na(x1stuedexpct, x1paredexpct)

## compare two ways of dropping NAs
all.equal(data_small_cut, data_small_drop)

## ---------------------------
##' [Summarizing Data]
## ---------------------------

## get average (without storing)
data_small_cut |> summarize(mean(high_exp))

data_small_cut |> summarize(mean_exp = mean(high_exp))

## ---------------------------
##' [Grouping Data]
## ---------------------------

## get grouped average
data_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp))

## ---------------------------
##' [Saving Data]
## ---------------------------

## write with useful name

data_small_cut |>
  group_by(x1region) |>
  summarize(mean_exp = mean(high_exp)) |>
  write_csv("data/region-expects.csv")


## ---------------------------
##' [Appendix: All in One!]
## ---------------------------

## Let's redo the analysis above, but with a fully chained set of
## functions.

## start with original dataset
data |>
  ## select columns we want
  select(stu_id, x1stuedexpct, x1paredexpct, x1region) |>
  ## If expectation is -8, -9. or 11, make it NA
  mutate(x1stuedexpct = ifelse(x1stuedexpct %in% list(-8, -9, 11), NA, x1stuedexpct),
         x1paredexpct = ifelse(x1paredexpct %in% list(-8, -9, 11), NA, x1paredexpct)) |>
  ## Make a new variable called high_exp that is the higher or parent and student exp
  mutate(high_exp = ifelse(x1stuedexpct > x1paredexpct, x1stuedexpct, x1paredexpct)) |>
  ## Drop if either or both parent or student exp is NA
  filter(!is.na(x1stuedexpct) & !is.na(x1paredexpct)) |> 
  ## Group the results by region
  group_by(x1region) |>
  ## Get the mean of high_exp (by region)
  summarize(mean_exp = mean(high_exp)) |>
  ## Write that to a .csv file
  write_csv("data/region-expects-chain.csv")
  


non_chain <- read_csv("data/region-expects.csv")
chain <- read_csv("data/region-expects-chain.csv")
 
all.equal(non_chain, chain)
      


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
