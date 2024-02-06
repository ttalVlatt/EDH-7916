## -----------------------------------------------------------------------------
##
##' [PROJ: 7916]
##' [FILE: Data Wrangling I Redux: Vanilla R Solution]
##' [INIT: February 5th 2024]
##' [AUTH: Matt Capaldi] @ttalVlatt
##
## -----------------------------------------------------------------------------

setwd(this.path::here())

## ---------------------------
##' [Libraries]
## ---------------------------

## None!

## ---------------------------
##' [Input]
## ---------------------------

df <- read.csv(file.path("data", "hsls-small.csv"))

## ---------------------------
##' [Q1]
## ---------------------------

## Part One
df$x1txmtscor[df$x1txmtscor == -8] <- NA
df_math <- na.omit(df) # No other NAs, so omit works fine

mean(df_math$x1txmtscor)

## Part Two
aggregate(df_math["x1txmtscor"],          # var of interest
          by = list(sex = df$x1sex), # by group
          FUN = mean)                # function to run

## ---------------------------
##' [Q2]
## ---------------------------

df_pov <- df[df$x1poverty185 == 1,]

paste("The median household income cat is", median(df_pov$x1famincome),
      "which represents incomes between $15k and $35k")

## ---------------------------
##' [Q3]
## ---------------------------

df_hs <- df[df$x4hscompstat %in% c(1,2),]

hs_counts <- table(df_hs$x4hscompstat)
ged <- hs_counts["2"]
total <- hs_counts["1"] + hs_counts["2"]
percent <- round(ged/total*100, 2)
paste(percent, "% of those with HS credential have a GED")

## ---------------------------
##' [Q4]
## ---------------------------

## Part One

df_col <- df[df$x4evratndclg != -8,]

col_counts <- table(df_col$x4evratndclg)
college <- col_counts["1"]
total <- col_counts["0"] + col_counts["1"]
percent <- round(college/total*100, 2)

paste(percent, "% of student ever attended college")

## Part Two

## Drop any missing household income and make new variable
df_col <- df_col[!df_col$x1famincome %in% c(-8,-9),]
df_col$below_35k <- ifelse(df_col$x1famincome %in% c(1,2), 1, 0)

## Now, we can use the aggregate function again, but use the table function
## we used 
## but inside the "by" list
## we can put both columns in there, a bit like we put two variables in
## tidyverse's group by
col_counts <- aggregate(df_col["x4evratndclg"],          # var of interest
                        by = list(b35k = df_col$below_35k, # by group(s)
                                  region = df_col$x1region), 
                        FUN = table)                # function to run

## The ouput of this is a little messy, so we can clean it up
col_counts$attend <- col_counts$x4evratndclg[,"1"]
col_counts$total <- col_counts$x4evratndclg[,"1"] + col_counts$x4evratndclg[,"0"]
col_counts$perc <- col_counts$attend/col_counts$total * 100

print(col_counts)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------
