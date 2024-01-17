################################################################################
##
## [ PROJ ] EDH7916: Introduction
## [ FILE ] intro_r.R 
## [ INIT ] 6 January 2020
## [ REVN ] 4 January 2022
## [ AUTH ] Benjamin Skinner (GitHub/Twitter: @btskinner)
##
################################################################################

## ---------------------------------------------------------
## Welcome!
## ---------------------------------------------------------

print("Hello, World!")

1 + 1

## ---------------------------------------------------------
## Assignment
## ---------------------------------------------------------

## assign value to object x using <-
x <- 1

## what's in x?
x

##' @Matt needs to work on this  
##' Matt needs to work on [this]
##' Matt *needs* to work on this
##' [Matt: needs to work on this]

## Try commenting/uncommenting the below line

# Matt <- "Hi"

typeof(x)

x <- "1"
typeof(x)



install.packages("tidyverse")



## load library (note quirk that you don't need quotes here)
library(tidyverse)

## ---------------------------------------------------------
## Help
## ---------------------------------------------------------

## get help file for function
?median

## search for function in CRAN
??median
