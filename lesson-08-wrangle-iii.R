## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data Wrangling III: Strings and Dates]
##' [INIT: 18 March 2024]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [EDIT: Jue Wu]
##' [UPDT: 12 February 2025]
##
## -----------------------------------------------------------------------------

## ---------------------------
##' [Libraries]
## ---------------------------

## NB: The stringr library is loaded with tidyverse, but
## lubridate is not, so we need to load it separately

library(tidyverse)
library(lubridate)

## ---------------------------
##' [Input]
## ---------------------------

## read in data and lower all names using rename_all(tolower)
data <- read_csv("data/hd2007.csv") |>
    rename_all(tolower)

## -----------------------------------------------------------------------------
##' [Working with strings]
## -----------------------------------------------------------------------------

## filter using state abbreviation (not saving, just viewing)
data |>
    filter(stabbr == "FL")

## see first few rows of distinct chief titles
data |>
    distinct(chftitle)

## return the most common titles
data |>
    ## get counts of each type
    count(chftitle) |>
    ## arrange in descending order so we see most popular at top
    arrange(desc(n))

## string vector example
fruits <- c("green apple", "banana", "red apple")

## search for "apple", which should be true for the first and third item
str_detect(fruits, "apple")

## how many use some form of the title president?
data |>
    ## still starting with our count
    count(chftitle) |>
    ## ...but keeping only those titles that contain "President"
    filter(str_detect(chftitle, "President")) |>
    ## arranging as before
    arrange(desc(n))

## solution 1: look for either P or p
data |>
    count(chftitle) |>
    filter(str_detect(chftitle, "[Pp]resident")) |>
    arrange(desc(n))


## solution 2: make everything lowercase so that case doesn't matter
data |>
    count(chftitle) |>
    filter(str_detect(str_to_lower(chftitle), "president")) |>
    arrange(desc(n))


## show first few zip code values
data |>
    select(unitid, zip)

## pull out first 5 digits of zip code
data <- data |>
    mutate(zip5 = str_sub(zip, start = 1, end = 5))

## show (use select() to subset so we can see new columns)
data |>
    select(unitid, zip, zip5)

## drop last four digits of extended zip code if they exist
data <- data |>
    mutate(zip5_v2 = str_replace(zip, "([0-9]+)(-[0-9]+)?", "\\1"))

## show (use select() to subset so we can see new columns)
data |>
    select(unitid, zip, zip5, zip5_v2)

## check if both versions of new zip column are equal
identical(data |> select(zip5), data |> select(zip5_v2))

## filter to rows where zip5 != zip5_v2 (not storing...just looking)
data |>
    filter(zip5 != zip5_v2) |>
    select(unitid, zip, zip5, zip5_v2)


## -----------------------------------------------------------------------------
##' [Working with Dates]
## -----------------------------------------------------------------------------

## subset to schools who closed during this period
data <- data |>
  filter(closedat != -2) |>
  select(unitid, instnm, closedat)

data


## create a new clean_date column 
data <- data |>
    mutate(clean_date = parse_date_time(closedat,
                                        orders = "mdy"))

## show
data

## Try adding another date format
data <- data |>
    mutate(clean_date = parse_date_time(closedat,
                                        orders = c("mdy", "my")))

## show
data

data |>
  filter(is.na(clean_date))

data <- data |>
  drop_na(clean_date)

data |> filter(clean_date == min(clean_date))

christmas_07 <- parse_date_time("Dec 25 2007", "mdy")

data |> filter(clean_date < christmas_07)

## Nested version
data |> filter(abs(time_length(interval(clean_date, christmas_07), "day")) < 30)

## Internal pipes version
data |> filter(interval(clean_date, christmas_07) |>
               time_length("day") |>
               abs() < 30)

data |> 
  mutate(quarter = quarter(clean_date)) |>
  count(quarter)

data |>
  mutate(day = wday(clean_date, label = TRUE)) |>
  count(day)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
