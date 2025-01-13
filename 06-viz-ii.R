## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Data visualization with ggplot2 - Customization]
##' [INIT: 9 March 2020]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##
## -----------------------------------------------------------------------------

## ---------------------------
##' [Libraries]
## ---------------------------

library(tidyverse)
library(patchwork)

## ---------------------------
##' [Input data]
## ---------------------------

df <- read_csv(file.path("data", "hsls-small.csv"))

## -----------------------------------------------------------------------------
##' [Initial plain plot]
## -----------------------------------------------------------------------------

## Drop missing values for math test score
df <- df |>
  filter(x1txmtscor != -8)

## create histogram using ggplot
p <- ggplot(data = df) +
  geom_histogram(mapping = aes(x = x1txmtscor))

## show
p

## -----------------------------------------------------------------------------
##' [Titles and captions]
## -----------------------------------------------------------------------------

## Add placeholder titles/labels/captions
p <- p +
  labs(title = "Title",
       subtitle = "Subtitle",
       caption = "Caption",
       x = "X axis label",
       y = "Y axis label")

## show 
p

## create histogram using ggplot
p <- p +
  labs(title = "Math test scores",
       caption = "Data: High School Longitudinal Study, 2009",
       x = "Math score",
       y = "Count")

## show 
p



## -----------------------------------------------------------------------------
##' [Axis formatting]
## -----------------------------------------------------------------------------

## create histogram using ggplot
p <- p +
    scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5),
                     minor_breaks = seq(from = 0, to = 100, by = 1)) +
  scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 100),
                     minor_breaks = seq(from = 0, to = 2500, by = 50))

## show 
p

p <- p +
    scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 500),
                     minor_breaks = seq(from = 0, to = 2500, by = 100))


## -----------------------------------------------------------------------------
##' [Legend labels]
## -----------------------------------------------------------------------------

## add indicator that == 1 if either parent has any college
df <- df |>
  mutate(pared_coll = ifelse(x1paredu >= 3, 1, 0),
         pared_coll = factor(pared_coll,
                             levels = c(0,1),
                             labels = c("No college", "College")))


p2 <- ggplot(data = df) +
  geom_histogram(mapping = aes(x = x1txmtscor,
                               fill = factor(pared_coll)),
                 alpha = 0.66) +
  ## Below here is just what we had added to p in previous steps
  labs(title = "Math test scores",
       caption = "Data: High School Longitudinal Study, 2009",
       x = "Math score",
       y = "Count") +
      scale_x_continuous(breaks = seq(from = 0, to = 100, by = 5),
                     minor_breaks = seq(from = 0, to = 100, by = 1)) +
    scale_y_continuous(breaks = seq(from = 0, to = 2500, by = 500),
                     minor_breaks = seq(from = 0, to = 2500, by = 100))

## show 
p2



## ---------------------------
##' [Color Scales]
## ---------------------------

## create histogram using ggplot
p2 <- p2 +
  scale_fill_manual(values = c("blue4", "orange2"))

## show 
p2

## Add a facet wrap for region
p2 <- p2 +
  facet_wrap(~x1sex)

## show 
p2





## -----------------------------------------------------------------------------
##' [Preset themes]
## -----------------------------------------------------------------------------

## create histogram using ggplot
p2 <- p2 +
  theme_bw()

## show 
p2


p2 <- p2 +
  theme_linedraw()

## show 
p2


p2 <- p2 +
  theme(axis.ticks.x = element_line(linewidth = 0.8,
                                    lineend = "round"))

## show 
p2

## -----------------------------------------------------------------------------
##' [Multiple plots with patchwork]
## -----------------------------------------------------------------------------

## Make a nice looking second plot of math scores by by parental education
p3 <- ggplot(df) +
  geom_boxplot(mapping = aes(x = pared_coll,
                             y = x1txmtscor,
                             fill = pared_coll),
               alpha = 0.66) +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  labs(x = NULL,
       y = "Math Score",
       fill = "Parental Education") +
  theme_linedraw()

## show
p3

## use plus sign for side by side
p2 + p3

p2 / p3

p2 <- p2 +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8)

p2 / p3

p2 / p3 + plot_layout(design = "AAAAA
                                #BBB#")

p2 / p3 + guide_area() + plot_layout(design = "AAAAA
                                               BBBCC",
                                     guides= "collect")



p2 / p3 + guide_area() + plot_layout(design = "AAAAA
                                               BBBCC",
                                     guides= "collect") +
  plot_annotation(title = "Math Test Scores Differences by Parental Education",
                  caption = "Data: High School Longitudinal Study, 2009")

patch <- p2 / p3 + guide_area() + plot_layout(design = "AAAAA
                                               BBBCC",
                                     guides= "collect") +
  plot_annotation(title = "Math Test Scores Differences by Parental Education",
                  caption = "Data: High School Longitudinal Study, 2009")


## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
