1+1

library(tidyverse)
library(haven)

data <- read_dta("data/hsls-small.dta")

ggplot(data) +
  geom_histogram(aes(x = x1txmtscor,
                     fill = as_factor(x1sex)),
                 alpha = 0.5) +
  labs(x = "Math Score",
       fill = "Sex") +
  theme_minimal() +
  scale_fill_viridis_d(option = "magma") 

ggsave("plot.png")



library(knitr)

data |> 
  drop_na(x1txmtscor)|>
  summarize(mean(x1txmtscor)) |>
  kable()

