library(conflicted)
library(here)

wd <- here("03_generate_forecast", "scripts")

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

source(file.path(wd, "functions/logistic-growth.R"))
source(file.path(wd, "functions/sim-temperatures.R"))

#' Dynamic logistic growth model
#'
#' @param p Population at current time step
#' @param r Population growth rate
#' @param k Carrying capacity
#' @return Population at next time step
logistic_growth <- function(p, r, k) {
  (k * p * exp(r)) / (k + p * (exp(r) - 1))
}

pheno_dat <- read_csv("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", guess_max = 1e6)

# Pretend it's April 1
today <- "2021-04-01"

state_today <- pheno_dat %>%
  filter(time == today, siteID == "DELA")
stopifnot(nrow(state_today) == 1)

# Simulate some temperature data
# TODO: Replace with real temperature data

nens <- 500

# Variability in growth rate is a function of temperature
# TODO: Replace with real temperature data
ntime <- 15
temperatures <- simulate_airtemp("2021-01-01", as_date(today) + days(ntime-1)) %>%
  # Calculate growing degree days
  mutate(gdd = cumsum(pmax(airtemp_c - 10, 0))) %>%
  filter(time >= today)
gdd <- temperatures$gdd
times <- temperatures$time

# Variability in parameters
rslope <- 1 / rnorm(nens, 2000, 500)
rint <- rnorm(nens, 1e-1, 5e-2)
r <- matrix(NA_real_, ntime, nens)
for (i in seq_len(nens)) {
  r[,i] <- rint[i] + rslope[i] * gdd
}

# Ensemble of input values
gcc_obs <- with(state_today, rnorm(nens, gcc_90, gcc_sd))

p <- matrix(NA_real_, ntime, nens)
gcc_pred <- matrix(NA_real_, ntime, nens)
gcc_pred[1,] <- gcc_obs
gcc_obs <- with(state_today, rnorm(nens, gcc_90, gcc_sd))

# Back-calculate the current p (at t = 1) from GCC
gccmin <- 0.35
gccmax <- 0.44
p[1,] <- (gcc_pred[1,] - gccmin) / (gccmax - gccmin)

# Run the model for the next few time steps
for (t in seq(2, ntime)) {
  for (i in seq_len(nens)) {
    p[t,i] <- logistic_growth(p[t-1,i], r[t-1,i], 1)
    gcc_pred[t,i] <- gccmin + p[t,i] * (gccmax - gccmin)
  }
}

matplot(gcc_pred, type = "l", xlab = "Forecast day")

# Tidy the forecast and save in a CSV
gcc_tidy <- as_tibble(gcc_pred, .name_repair = "unique") %>%
  bind_cols(time = times) %>%
  pivot_longer(
    starts_with(".."),
    names_to = "ensemble",
    values_to = "gcc_pred",
    names_prefix = "\\.{3}",
    names_transform = list(ensemble = as.integer)
  )
fname <- file.path(wd, "..", "data", sprintf("forecast-%s.csv", today))
write_csv(gcc_tidy, fname)
