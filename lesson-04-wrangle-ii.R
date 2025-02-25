## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data wrangling II: Appending, joining, and reshaping data]
##' [INIT: 31 January 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [EDIT: Jue Wu]
##' [UPDT: 15 January 2025]
##
## -----------------------------------------------------------------------------


## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

## -----------------------------------------------------------------------------
##' [Append data]
## -----------------------------------------------------------------------------

## read in data, storing in data_*, where * is a unique number
data_1 <- read_csv("data/sch-test/by-school/bend-gate-1980.csv")
data_2 <- read_csv("data/sch-test/by-school/bend-gate-1981.csv")
data_3 <- read_csv("data/sch-test/by-school/bend-gate-1982.csv")

## show each
data_1
data_2
data_3

## append files
data <- bind_rows(data_1, data_2, data_3)

## show
data


data_split_left <- data[,1:2]
data_split_right <- data[,3:5]

print(data_split_left)
print(data_split_right)

## Append them back together side-by-side


## -----------------------------------------------------------------------------
##' [Join data]
## -----------------------------------------------------------------------------

## read in all_schools data
data <- read_csv("data/sch-test/all-schools.csv")

## show
data

## get test score summary 
data_sum <- data |>
    ## grouping by year so average within each year
    group_by(year) |>
    ## get mean(<score>) for each test
    summarize(math_m = mean(math),
              read_m = mean(read),
              science_m = mean(science))

## show
data_sum

## start with our dataframe...
data_joined <- data |>
    ## pipe into left_join to join with data_sum using "year" as key
    left_join(data_sum, by = "year")

## show
data_joined

## Therefore 
left_join(x = data,
          y = data_sum,
          by = "year")

## Is exactly the same as
data |>
  left_join(x = _, ## If it helps to visualize, the _ is where the |> will go
            y = data_sum,
            by = "year")

## Is exactly the same as
data |>
  left_join(data_sum,
            by = "year")

## Note: if we want to keep the joined data, we should assign it to data_join
data_join <- data |>
  left_join(data_sum,
            by = "year")


## -----------------------------------------------------------------------------
##' [Reshape data]
## -----------------------------------------------------------------------------

## reading again just to be sure we have the original data
data <- read_csv("data/sch-test/all-schools.csv")

## print to see what the data structure looks like
print(data)

## wide to long
data_long <- data |>
    ## cols: current test columns
    ## names_to: where "math", "read", and "science" will go
    ## values_to: where the values in cols will go
    pivot_longer(cols = c("math","read","science"),
                 names_to = "test",
                 values_to = "score")

## show
data_long

## long to wide
data_wide <- data_long |>
    ## names_from: values in this column will become new column names
    ## values_from: values in this column will become values in new cols
    pivot_wider(names_from = "test",
                values_from = "score")

## show
data_wide

## read in very wide test score data
data <- read_csv("data/sch-test/all-schools-wide.csv")

## show
data

## wide to long
data_long <- data |>
    ## contains() looks for "19" in name: if there, it adds it to cols
    pivot_longer(cols = contains("19"),
                 names_to = c("test", "year"),
                 names_sep = "_",
                 values_to = "score")

## show
data_long

## wide to long
data_wide <- data_long |>
    pivot_wider(values_from = score,
                names_from = c(test, year),
                names_sep = "_")

## show
data_wide

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

data_info <- read_csv("data/hd2022.csv") |> 
  rename_all(tolower) # convert all column names to lowercase

data_enroll <- read_csv("data/effy2022.csv") |> 
  rename_all(tolower)

data_info

data_enroll

data_enroll <- data_enroll |>
  select(unitid, effylev, efytotlt, efynralt)

data_enroll


data_enroll <- data_enroll |>
  filter(effylev %in% c(2,4))
  
data_enroll



data_enroll <- data_enroll |>
  mutate(perc_intl = efynralt/efytotlt*100) |>
  select(-efytotlt, -efynralt) # - in select means drop this variable

data_enroll

data_enroll <- data_enroll |>
  pivot_wider(names_from = effylev,
              values_from = perc_intl,
              names_prefix = "perc_intl_")

data_enroll

data_enroll <- data_enroll |>
  mutate(perc_intl_diff = perc_intl_2 - perc_intl_4)

data_enroll

data_enroll |>
  drop_na() |>
  summarize(mean = mean(perc_intl_diff),
            min = min(perc_intl_diff),
            max = max(perc_intl_diff))
            

data_info <- data_info |>
  select(unitid, control)

data_joined <- left_join(data_enroll, data_info, by = "unitid")

data_joined

data_joined |>
  group_by(control) |>
  drop_na() |>
  summarize(mean = mean(perc_intl_diff))


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
