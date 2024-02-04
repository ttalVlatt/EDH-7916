################################################################################
##
## <PROJ> EDH7916: Data wrangling II: Appending, joining, and reshaping data
## <FILE> dw_two.R 
## <INIT> 31 January 2020
## <AUTH> Benjamin Skinner (GitHub/Twitter: @btskinner)
##
################################################################################


## ---------------------------
## libraries
## ---------------------------

library(tidyverse)

## -----------------------------------------------------------------------------
## Append data
## -----------------------------------------------------------------------------

## ---------------------------
## input
## ---------------------------

## read in data, storing in df_*, where * is a unique number
df_1 <- read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1980.csv"))
df_2 <- read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1981.csv"))
df_3 <- read_csv(file.path("data", "sch-test", "by-school", "bend-gate-1982.csv"))

## ---------------------------
## process
## ---------------------------

## show each
df_1
df_2
df_3

## append files
df <- bind_rows(df_1, df_2, df_3)

## show
df


df_split_left <- df[,1:2]
df_split_right <- df[,3:5]

print(df_split_left)
print(df_split_right)

## Append them back together side-by-side


## -----------------------------------------------------------------------------
## Join data
## -----------------------------------------------------------------------------

## ---------------------------
## input
## ---------------------------

## read in all_schools data
df <- read_csv(file.path("data", "sch-test", "all-schools.csv"))

## show
df

## ---------------------------
## process
## ---------------------------

## get test score summary 
df_sum <- df |>
    ## grouping by year so average within each year
    group_by(year) |>
    ## get mean(<score>) for each test
    summarize(math_m = mean(math),
              read_m = mean(read),
              science_m = mean(science))

## show
df_sum

## start with data frame...
df_joined <- df |>
    ## pipe into left_join to join with df_sum using "year" as key
    left_join(df_sum, by = "year")

## show
df_joined

## We can be overly specific to make the point
left_join(x = df,
          y = df_sum,
          by = "year")

## Therefore 
left_join(x = df,
          y = df_sum,
          by = "year")

## Is exactly the same as
df |>
  left_join(x = _, ## If it helps to visualize, the _ is where the |> will go
            y = df_sum,
            by = "year")

## Is exactly the same as
df |>
  left_join(df_sum,
            by = "year")

## Note: if we want to keep the joined data, we should assign it to df_join
df_join <- df |>
  left_join(df_sum,
            by = "year")


## -----------------------------------------------------------------------------
## Reshape data
## -----------------------------------------------------------------------------

## ---------------------------
## input
## ---------------------------

## reading again just to be sure we have the original data
df <- read_csv(file.path("data", "sch-test", "all-schools.csv"))

## ---------------------------
## process
## ---------------------------

## wide to long
df_long <- df |>
    ## cols: current test columns
    ## names_to: where "math", "read", and "science" will go
    ## values_to: where the values in cols will go
    pivot_longer(cols = c("math","read","science"),
                 names_to = "test",
                 values_to = "score")

## show
df_long

## ---------------------------
## process
## ---------------------------

## long to wide
df_wide <- df_long |>
    ## names_from: values in this column will become new column names
    ## values_from: values in this column will become values in new cols
    pivot_wider(names_from = "test",
                values_from = "score")

## show
df_wide

## ---------------------------
## input
## ---------------------------

## read in very wide test score data
df <- read_csv(file.path("data", "sch-test", "all-schools-wide.csv"))

## show
df

## ---------------------------
## process
## ---------------------------

## wide to long
df_long <- df |>
    ## NB: contains() looks for "19" in name: if there, it adds it to cols
    pivot_longer(cols = contains("19"),
                 names_to = c("test", "year"),
                 names_sep = "_",
                 values_to = "score")

## show
df_long

## ---------------------------
## process
## ---------------------------

## wide to long
df_wide <- df_long |>
    pivot_wider(values_from = score,
                names_from = c(test, year),
                names_sep = "_")

## show
df_wide

## -----------------------------------------------------------------------------
## end script
## -----------------------------------------------------------------------------
