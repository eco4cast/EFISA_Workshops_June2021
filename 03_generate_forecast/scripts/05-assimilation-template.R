set.seed(8675309)

library(conflicted)
library(here)

wd <- here("03_generate_forecast")

library(dplyr)
library(ggplot2)
library(glue)
library(lubridate)
library(readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

sitename <- "HARV"
today <- as_date("2021-05-16")
yesterday <- today - days(1)

# Read old forecast
forecast_file <- file.path(wd, "data", glue("forecast-{yesterday}.csv"))
old_forecast <- read_csv(forecast_file)

# Read phenology data
phenofile <- file.path(wd, "data", "phenology-targets.csv.gz")
pheno_dat <- read_csv(phenofile, guess_max = 1e6)

# Select new data for "today"

# Particle filter: Assign each ensemble member probabilities according to today's results

# Resample ensembles according to probability

# Compare the forecasts
