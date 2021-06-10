library(conflicted)
library(here)

wd <- here("03_generate_forecast")

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

source(file.path(wd, "scripts/functions/logistic-growth.R"))

phenofile <- file.path(wd, "data", "phenology-targets.csv.gz")
if (!file.exists(phenofile)) {
  download.file("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", phenofile)
}
pheno_dat <- read_csv(phenofile, guess_max = 1e6)

# Pretend it's April 1
today <- "2021-04-01"

# Read in temperature data, and use it to calculate growing degree days
# TODO: Replace with real temperature data
ntime <- 15
temperatures <- read_csv(file.path(wd, "data", "simulated-airtemp.csv")) %>%
  # Calculate growing degree days
  mutate(gdd = cumsum(pmax(airtemp_c - 10, 0))) %>%
  filter(time >= today)
gdd <- temperatures$gdd
times <- temperatures$time

# Plot growing degree days
ggplot(temperatures) +
  aes(x = time, y = gdd) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Time", y = "Growing degree days")

# Variability in parameters
nens <- 500
rslope <- 1 / rnorm(nens, 2000, 500)
rint <- rnorm(nens, 1e-1, 5e-2)
r <- matrix(NA_real_, ntime, nens)
for (i in seq_len(nens)) {
  r[,i] <- rint[i] + rslope[i] * gdd
}

# Set our constants
gccmin <- 0.35
gccmax <- 0.44

# Get the current phenology state -- for "today" (April 1)
state_today <- pheno_dat %>%
  filter(time == today, siteID == "DELA")
stopifnot(nrow(state_today) == 1)
state_today

# Ensemble of input values
gcc_obs <- with(state_today, rnorm(nens, gcc_90, gcc_sd))
hist(gcc_obs, xlab = "Observed GCC")

gcc_pred <- matrix(NA_real_, ntime, nens)
gcc_pred[1,] <- gcc_obs

# Back-calculate the current p (at t = 1) from GCC
p <- matrix(NA_real_, ntime, nens)
p[1,] <- (gcc_pred[1,] - gccmin) / (gccmax - gccmin)
hist(p[1,], xlab = "Initial phenology state")

# Run the model for the next few time steps
for (t in seq(2, ntime)) {
  for (i in seq_len(nens)) {
    p[t,i] <- logistic_growth(p[t-1,i], r[t-1,i], 1)
    gcc_pred[t,i] <- gccmin + p[t,i] * (gccmax - gccmin)
  }
}

matplot(p, type = "l", xlab = "Forecast day", ylab = "Phenology state")
matplot(gcc_pred, type = "l", xlab = "Forecast day", ylab = "Predicted GCC")

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
fname <- file.path(wd, "data", sprintf("forecast-%s.csv", today))
write_csv(gcc_tidy, fname)
