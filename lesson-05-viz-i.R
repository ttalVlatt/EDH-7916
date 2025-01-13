## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data visualization with ggplot2 - Basics]
##' [INIT: 9 March 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##
## -----------------------------------------------------------------------------



## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)

## ---------------------------
##' [Input data]
## ---------------------------

## read_dta() ==> read in Stata (*.dta) files
df_hs <- haven::read_dta(file.path("data", "hsls-small.dta"))
## read_csv() ==> read in comma separated value (*.csv) files
df_ts <- read_csv(file.path("data", "sch-test", "all-schools.csv"))

## -----------------------------------------------------------------------------
##' [Base R graphics]
## -----------------------------------------------------------------------------

##' [Histogram]

## histogram of math scores (which should be normal by design)
hist(df_hs$x1txmtscor)

##' [Density]

## density plot of math scores
density(df_hs$x1txmtscor, na.rm = TRUE) |>
  plot()

##' [Box Plot]

## box plot of math scores against student expectations
boxplot(x1txmtscor ~ x1stuedexpct, data = df_hs)

##' [Scatter]

## scatter plot of math against SES
plot(df_hs$x1ses, df_hs$x1txmtscor)

## -----------------------------------------------------------------------------
##' [Graphics with ggplot2]
## -----------------------------------------------------------------------------

##' [Histogram]

## init ggplot 
ggplot(data = df_hs)

## add histogram instruction (notice we can add pieces using +)
ggplot(data = df_hs) +
  geom_histogram(mapping = aes(x = x1txmtscor))

##' [Density]

## density
ggplot(data = df_hs) +
  geom_density(mapping = aes(x = x1txmtscor))

ggplot(data = df_hs) +
  geom_histogram(mapping = aes(x = x1txmtscor)) +
  geom_density(mapping = aes(x = x1txmtscor))

## histogram with density plot overlapping
ggplot(data = df_hs) +
  geom_histogram(mapping = aes(x = x1txmtscor, y = after_stat(density))) +
  geom_density(mapping = aes(x = x1txmtscor))

## histogram with density plot overlapping (add color to see better)
ggplot(data = df_hs) +
  geom_histogram(mapping = aes(x = x1txmtscor, y = after_stat(density)),
                 color = "black",
                 fill = "white") +
  geom_density(mapping = aes(x = x1txmtscor),
               fill = "red",
               alpha = 0.2)


##' [Two-Way]

## see the counts for each group
df_hs |> count(x1paredu)

## need to set up data
plot_df <- df_hs |>
  ## select the columns we need
  select(x1paredu, x1txmtscor) |>
  ## can't plot NA so will drop
  drop_na() |>
  ## create new variable that == 1 if parents have any college, then make it a factor
  mutate(pared_coll = ifelse(x1paredu >= 3, 1, 0),
         pared_coll = factor(pared_coll)) |>
  ## drop (using negative sign) the original variable we don't need now
  select(-x1paredu) 

## show
head(plot_df)

## two way histogram
ggplot(plot_df) +
  geom_histogram(aes(x = x1txmtscor,
                     fill = pared_coll),
                 alpha = 0.5,
                 color = "black")

## two way histogram
ggplot(plot_df) +
  geom_density(aes(x = x1txmtscor,
                   fill = pared_coll),
               alpha = 0.5,
               color = "black")

##' [Box Plot]

## box plot using both factor() and as_factor()
ggplot(data = df_hs,
       mapping = aes(x = factor(x1paredu),
                     y = x1txmtscor,
                     fill = factor(x1paredu))) +
  geom_boxplot()

##' [Scatter]

## sample 10% to make figure clearer
df_hs_10 <- df_hs |>
  ## drop observations with missing values for x1stuedexpct
  drop_na(x1stuedexpct) |>
  ## sample
  sample_frac(0.1)

## scatter
ggplot(data = df_hs_10) +
  geom_point(mapping = aes(x = x1ses, y = x1txmtscor))

## see student base year plans
df_hs |>
  count(x1stuedexpct)

## create variable for students who plan to graduate from college
df_hs_10 <- df_hs_10 |>
  mutate(plan_col_grad = ifelse(x1stuedexpct >= 6 & x1stuedexpct < 11,
                                1,        # if T: 1
                                0),       # if F: 0
         plan_col_grad = factor(plan_col_grad,
                                levels = c(0, 1),
                                labels = c("No", "Yes")))      

## scatter
ggplot(data = df_hs_10,
       mapping = aes(x = x1ses, y = x1txmtscor)) +
  geom_point(mapping = aes(color = plan_col_grad), alpha = 0.5)


##' [Fitted Lines]

## add fitted line with linear fit
ggplot(data = df_hs_10, mapping = aes(x = x1ses, y = x1txmtscor)) +
  geom_point(mapping = aes(color = factor(plan_col_grad)), alpha = 0.5) +
  geom_smooth(method = lm, color = "black")

## add fitted line with loess
ggplot(data = df_hs_10, mapping = aes(x = x1ses, y = x1txmtscor)) +
  geom_point(mapping = aes(color = factor(plan_col_grad)), alpha = 0.5) +
  geom_smooth(method = loess, color = "black")

##' [Line Graph]

## show test score data
df_ts

## line graph
ggplot(data = df_ts |> filter(school == "Spottsville"),
       mapping = aes(x = year, y = math)) +
  geom_line()

## line graph for math scores at every school over time
ggplot(data = df_ts,
       mapping = aes(x = year, y = math, colour = school)) +
  geom_line()

## facet line graph
ggplot(data = df_ts,
       mapping = aes(x = year, y = math)) +
  facet_wrap(~ school) +
  geom_line()

## reshape data long
df_ts_long <- df_ts |>
  pivot_longer(cols = c("math","read","science"), # cols to pivot long
               names_to = "test",                 # where col names go
               values_to = "score")               # where col values go

## show
df_ts_long

## facet line graph, with colour = test and ~school
ggplot(data = df_ts_long) +
  geom_line(mapping = aes(x = year, y = score, colour = test)) +
  facet_wrap(~school)

df_ts_long_std <- df_ts_long |>
  group_by(test, school) |>
  arrange(year) |> 
  mutate(score_year_one = first(score),
         ## note that we're using score_year_one instead of mean(score)
         score_std_sch = (score - score_year_one) / sd(score)) |>
  ungroup()

print(df_ts_long_std, n = 13)

## facet line graph, with colour = test and ~school
ggplot(data = df_ts_long_std) +
  geom_line(mapping = aes(x = year, y = score_std_sch, colour = test)) +
  facet_wrap(~school)


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
