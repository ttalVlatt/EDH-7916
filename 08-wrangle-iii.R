## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: 08-wrangle-iii.R]
##' [INIT: 18 March 2024]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##
## -----------------------------------------------------------------------------

################################################################################
##
## <PROJ> EDH7916: Data Wrangling III: Working with strings and dates
## <FILE> dw_three.R 
## <INIT> 10 February 2020
## <AUTH> Benjamin Skinner (GitHub/Twitter: @btskinner)
##
################################################################################


## ---------------------------
## libraries
## ---------------------------

## NB: The stringr library is loaded with tidyverse, but
## lubridate is not, so we need to load it separately

library(tidyverse)
library(lubridate)

## ---------------------------
## input
## ---------------------------

## read in data and lower all names using rename_all(tolower)
df <- read_csv(file.path("data", "hd2007.csv")) |>
    rename_all(tolower)

## -----------------------------------------------------------------------------
## Working with strings
## -----------------------------------------------------------------------------

## filter using state abbreviation (not saving, just viewing)
df |>
    filter(stabbr == "FL")

## see first few rows of distinct chief titles
df |>
    distinct(chftitle)

## return the most common titles
df |>
    ## get counts of each type
    count(chftitle) |>
    ## arrange in descending order so we see most popular at top
    arrange(desc(n))

## string vector example
fruits <- c("green apple", "banana", "red apple")

## search for "apple", which should be true for the first and third item
str_detect(fruits, "apple")

## how many use some form of the title president?
df |>
    ## still starting with our count
    count(chftitle) |>
    ## ...but keeping only those titles that contain "President"
    filter(str_detect(chftitle, "President")) |>
    ## arranging as before
    arrange(desc(n))

## solution 1: look for either P or p
df |>
    count(chftitle) |>
    filter(str_detect(chftitle, "[Pp]resident")) |>
    arrange(desc(n))


## solution 2: make everything lowercase so that case doesn't matter
df |>
    count(chftitle) |>
    filter(str_detect(str_to_lower(chftitle), "president")) |>
    arrange(desc(n))


## show first few zip code values
df |>
    select(unitid, zip)

## pull out first 5 digits of zip code
df <- df |>
    mutate(zip5 = str_sub(zip, start = 1, end = 5))

## show (use select() to subset so we can set new columns)
df |>
    select(unitid, zip, zip5)

## drop last four digits of extended zip code if they exist
df <- df |>
    mutate(zip5_v2 = str_replace(zip, "([0-9]+)(-[0-9]+)?", "\\1"))

## show (use select() to subset so we can set new columns)
df |>
    select(unitid, zip, zip5, zip5_v2)

## check if both versions of new zip column are equal
identical(df |> select(zip5), df |> select(zip5_v2))

## filter to rows where zip5 != zip5_v2 (not storing...just looking)
df |>
    filter(zip5 != zip5_v2) |>
    select(unitid, zip, zip5, zip5_v2)


## -----------------------------------------------------------------------------
##' [Working with Dates]
## -----------------------------------------------------------------------------

## subset to schools who closed during this period
df <- df |>
  filter(closedat != -2) |>
  select(unitid, instnm, closedat)

df


## create a new clean_date column 
df <- df |>
    mutate(clean_date = parse_date_time(closedat,
                                        orders = "mdy"))

## show
df

## Try adding another date format
df <- df |>
    mutate(clean_date = parse_date_time(closedat,
                                        orders = c("mdy", "my")))

## show
df

df |>
  filter(is.na(clean_date))

df <- df |>
  drop_na(clean_date)

df |> filter(clean_date == min(clean_date))

christmas_07 <- parse_date_time("Dec 25 2007", "mdy")

df |> filter(clean_date < christmas_07)

## Nested version
df |> filter(abs(time_length(interval(clean_date, christmas_07), "day")) < 30)

## Internal pipes version
df |> filter(interval(clean_date, christmas_07) |>
               time_length("day") |>
               abs() < 30)

df |> 
  mutate(quarter = quarter(clean_date)) |>
  count(quarter)

df |>
  mutate(day = wday(clean_date, label = TRUE)) |>
  count(day)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
