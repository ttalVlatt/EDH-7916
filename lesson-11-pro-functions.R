## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Functions & Loops]
##' [INIT: 5 March 2024]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##
## -----------------------------------------------------------------------------

library(tidyverse)

df <- haven::read_dta("data/hsls-small.dta")

plot <- ggplot(df) +
  geom_histogram(aes(x = x1txmtscor))

df_sum <- df |>
  summarize(mean = mean(x1txmtscor, na.rm = T))

uf_age <- 2024 - 1853

## -----------------------------------------------------------------------------
##' [For Loops]
## -----------------------------------------------------------------------------

matts_list <- c("Let's", "go", "Gators", "!")

for(i in matts_list) { print(i) }

for(word in matts_list) { print(word) }

for(gator_egg in matts_list) { print(gator_egg) }

gators_points_23 <- c(11, 49, 29, 22, 14, 38, 41, 20, 36, 35, 31, 15)

for(i in gators_points_23) { print(i) }

for(billy_napier in gators_points_23) { print(billy_napier) }

for(i in gators_points_23) {
  if(i > 30) {
    print(i)
  }
}

for(i in gators_points_23) {
  if(i > 30) {
    print(i)
  } else {
    print(i)
  }
}

for(i in gators_points_23) {
  if(i > 30) {
    paste("Yay the Gators scored", i, "points, which is more than 30!") |> print()
  } else {
    print(i)
  }
}

for(i in gators_points_23) {
  if(i > 30) {
    paste("Yay, the Gators scored", i, "points, which is more than 30!") |> print()
  } else {
    paste("Sad times, the Gators only scored", i, "points...") |> print()
  }
}

## -----------------------------------------------------------------------------
##' [Functions]
## -----------------------------------------------------------------------------

welcome <- function() { print("Welcome to UF!") }

welcome()

fake_data <- tribble(~ufid, ~name, ~dorm, ~first_class, ~meal_plan, ~roommate,
                     1853, "Jack", "Cyprus", "BIO-1001", 1, "Mike",
                     1854, "Hailey", "Simpson", "BIO-1001", 0, "Jessica",
                     1855, "Tamika", "Simpson", "CHEM-1002", 1, "Hannah",
                     1856, "Jessica", "Simpson", "ARCH-1003", 1, "Hailey",
                     1857, "Mike", "Cyrpus", "STA-1002", 0, "Jack",
                     1858, "Hannah", "Simpson", "EDF-1005", 1, "Tamika")

welcome <- function(id) { print("Welcome to UF!") }

welcome()

welcome <- function(id) {
  
  student <- fake_data |> filter(ufid == id)
  
  print(student)
  
}

welcome(1853)

welcome <- function(id) {
  
  student <- fake_data |> filter(ufid == id)
  
  name <- student |> pull(name)
  
  paste("Welcome to UF", name)
  
}

welcome(1853)

welcome <- function(id) {
  
  student <- fake_data |> filter(ufid == id)
  
  name <- student |> pull(name)
  dorm <- student |> pull(dorm)
  first_class <- student |> pull(first_class)
  
  paste("Welcome to UF", name, "you will be living in", dorm, "and your first class is", first_class)
  
}

welcome(1853)

## -----------------------------------------------------------------------------
##' [Functions]
## -----------------------------------------------------------------------------


files <- list.files("data/sch-test/by-school",
                    full.names = T)


for(i in files) {
  print(i)
}

for(i in files) {
  read_csv(i)
}

for(i in files) {
  file <- read_csv(i)
  name <- paste0("df_", i)
  assign(name, file)
}

for(i in files) {
  school <- str_extract(i, "niagara|bend|east|spot")
  year <- str_extract(i, "\\d+")
  name <- paste0("df_", school, year)
  file <- read_csv(i)
  assign(name, file)
}

df_bind <- tibble()

for(i in files) {
  file <- read_csv(i)
  df_bind <- bind_rows(df_bind, file)
}

files_niagara <- list.files("data/sch-test/by-school",
                            full.names = T,
                            pattern = "niagara")

df_niagara <- tibble()

for(i in files_niagara) {
  file <- read_csv(i)
  df_niagara <- bind_rows(df_niagara, file)
}


print(df_niagara)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

## NA
