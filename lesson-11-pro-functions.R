## -----------------------------------------------------------------------------
##
##' [PROJ: EDH 7916]
##' [FILE: Functions & Loops]
##' [INIT: 5 March 2024]
##' [AUTH: Benjamin Skinner @btskinner]
##' [EDIT: Matt Capaldi @ttalVlatt]
##' [EDIT: Jue Wu]
##' [UPDT: 12 February 2025]
##
## -----------------------------------------------------------------------------

library(tidyverse)

data <- haven::read_dta("data/hsls-small.dta")

plot <- ggplot(data) +
  geom_histogram(aes(x = x1txmtscor))

data_sum <- data |>
  summarize(mean = mean(x1txmtscor, na.rm = T))

uf_age <- 2024 - 1853

## -----------------------------------------------------------------------------
##' [For Loops]
## -----------------------------------------------------------------------------

class_list <- c("Let's", "go", "Gators", "!")

for(i in class_list) { print(i) }

for(word in class_list) { print(word) }

for(gator_egg in class_list) { print(gator_egg) }

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
  name <- paste0("data_", i)
  assign(name, file)
}

for(i in files) {
  school <- str_extract(i, "niagara|bend|east|spot")
  year <- str_extract(i, "\\d+")
  name <- paste0("data_", school, year)
  file <- read_csv(i)
  assign(name, file)
}

data_bind <- tibble()

for(i in files) {
  file <- read_csv(i)
  data_bind <- bind_rows(data_bind, file)
}

files_niagara <- list.files("data/sch-test/by-school",
                            full.names = T,
                            pattern = "niagara")

data_niagara <- tibble()

for(i in files_niagara) {
  file <- read_csv(i)
  data_niagara <- bind_rows(data_niagara, file)
}


print(data_niagara)

## -----------------------------------------------------------------------------
##' *END SCRIPT*
## -----------------------------------------------------------------------------

# NA
