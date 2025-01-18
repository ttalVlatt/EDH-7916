## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Extra Credit: Vanilla R]
##' [INIT: 18 July 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##
## -----------------------------------------------------------------------------

## ---------------------------
##' [Libraries]
## ---------------------------

## NONE


## Check working directory is correct
setwd(this.path::here())

## ---------------------------
##' [Input]
## ---------------------------

## data are CSV, so we use read.csv(), which is base R function
df <- read.csv(file.path("data", "hsls-small.csv"))

## show first 10 rows
head(df, n = 10)

## ---------------------------
##' [Process]
## ---------------------------

## show value at row 1, col 4
df[1, 4]

## show value at row 1, x1stdob column
df[1, "x1stdob"]

## show values at row 1, stu_id & x1stdob column
df[1, c("stu_id", "x1stdob")]

## -----------------
## select
## -----------------

## select columns we need and assign to new object
df_tmp <- df[, c("stu_id", "x1stuedexpct", "x1paredexpct", "x1region")]

## show 10 rows
head(df_tmp, n = 10)

## -----------------
##' [mutate]
## -----------------

## see unique values for student expectation
table(df_tmp$x1stuedexpct, useNA = "ifany")

## see unique values for parental expectation
table(df_tmp$x1paredexpct, useNA = "ifany")

## This will just print a bunch of -8s
df_tmp$x1stuedexpct[df_tmp$x1stuedexpct == -8]

## replace student expectation values
df_tmp$x1stuedexpct[df_tmp$x1stuedexpct == -8] <- NA
df_tmp$x1stuedexpct[df_tmp$x1stuedexpct == 11] <- NA


## replace parent expectation values
df_tmp$x1paredexpct[df_tmp$x1paredexpct %in% c(-8, -9, 11)] <- NA

## see unique values for student expectation (confirm changes)
table(df_tmp$x1stuedexpct, useNA = "ifany")

## see unique values for parental expectation (confirm changes)
table(df_tmp$x1paredexpct, useNA = "ifany")

## add new column
df_tmp$high_expct <- ifelse(df_tmp$x1stuedexpct > df_tmp$x1paredexpct, # test
                            df_tmp$x1stuedexpct,                       # if TRUE
                            df_tmp$x1paredexpct)                       # if FALSE

## show first 10 rows
head(df_tmp, n = 10)

## correct for NA values

## NB: We have to include [is.na(df_tmp$high_expct)] each time so that
## everything lines up

## step 1 student
df_tmp$high_expct[is.na(df_tmp$high_expct)] <- ifelse(
    ## test
    !is.na(df_tmp$x1stuedexpct[is.na(df_tmp$high_expct)]), 
    ## if TRUE do this...
    df_tmp$x1stuedexpct[is.na(df_tmp$high_expct)],
    ## ... else do that
    df_tmp$high_expct[is.na(df_tmp$high_expct)]
)

## step 2 parent
df_tmp$high_expct[is.na(df_tmp$high_expct)] <- ifelse(
    ## test
    !is.na(df_tmp$x1paredexpct[is.na(df_tmp$high_expct)]),
    ## if TRUE do this...
    df_tmp$x1paredexpct[is.na(df_tmp$high_expct)],
    ## ... else do that
    df_tmp$high_expct[is.na(df_tmp$high_expct)]
)

## show first 10 rows
head(df_tmp, n = 10)

## -----------------
##' [filter]
## -----------------

## get summary of our new variable
table(df_tmp$high_expct, useNA = "ifany")

## filter in values that aren't missing
df_tmp <- df_tmp[!is.na(df_tmp$high_expct),]

## show first 10 rows
head(df_tmp, n = 10)

## is the original # of rows - current # or rows == NA in count?
nrow(df) - nrow(df_tmp)

## -----------------
##' [summarize]
## -----------------

## get average (without storing)
mean(df_tmp$high_expct)

## check our census regions
table(df_tmp$x1region, useNA = "ifany")

## get average (assigning this time)
df_tmp <- aggregate(df_tmp["high_expct"],                # var of interest
                    by = list(region = df_tmp$x1region), # by group
                    FUN = mean)                          # function to run

## show
df_tmp

## ---------------------------
##' [output]
## ---------------------------

## write with useful name
write.csv(df_tmp, file.path("data", "high_expct_mean_region.csv"))

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
