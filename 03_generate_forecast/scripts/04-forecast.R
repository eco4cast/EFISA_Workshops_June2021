set.seed(8675309)

library(conflicted)   # Manage package conflicts
library(here)         # Manage working directories

wd <- here("03_generate_forecast")

library(distributions3)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

source(file.path(wd, "scripts/functions/logistic-growth.R"))

# Read phenology data
phenofile <- file.path(wd, "data", "phenology-targets.csv.gz")
if (!file.exists(phenofile)) {
  download.file("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", phenofile)
}
pheno_dat_all <- read_csv(phenofile, guess_max = 1e6)

# Read driver data
gdd_data_all <- read_csv(here("01_neon_data_access", "data", "qaqc-phenology-driver.csv.gz")) %>%
  # Recalculate GDD (should be cumulative by year!)
  arrange(time) %>%
  rename(GDD_orig = GDD) %>%
  filter(year(time) > 2014) %>%
  group_by(year = year(time)) %>%
  mutate(GDD = cumsum(GDD_orig)) %>%
  ungroup()

# Pretend it's May 1
today <- as_date("2021-05-15")
sitename <- "HARV"

# Number of time steps -- today (1) + 14 days into the future
ntime <- 15

# Subset GDD data to forecast time steps
gdd_data <- gdd_data_all %>%
  filter(time >= today, time < today + days(ntime))

# Plot growing degree days
ggplot(gdd_data) +
  aes(x = time, y = GDD) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(x = "Time", y = "Growing degree days")

# Set parameter distributions and constants
gccmin <- 0.33
gccmax <- 0.45
rslope_inv_d <- Normal(2000, 500)
rint_d <- Normal(0.1, 0.05)

# Variability in parameters
nens <- 1000
rslope <- 1 / random(rslope_inv_d, nens)
rint <- random(rint_d, nens)
r <- matrix(NA_real_, ntime, nens)
for (i in seq_len(nens)) {
  r[,i] <- pmax(rint[i] + rslope[i] * gdd_data$GDD, 0)
}

# Get the current phenology state -- for "today" and selected site
state_today <- pheno_dat_all %>%
  filter(time == today, siteID == sitename)
stopifnot(nrow(state_today) == 1)
state_today
gcc_today <- with(state_today, Normal(gcc_90, gcc_sd))

# Generate ensemble of input values and assign to the prediction matrix.
gcc_obs <- random(gcc_today, nens)
hist(gcc_obs, xlab = "Observed GCC")
gcc_pred <- matrix(NA_real_, ntime, nens)
gcc_pred[1,] <- gcc_obs

# Back-calculate the current p (at t = 1) from GCC
p <- matrix(NA_real_, ntime, nens)
p[1,] <- (gcc_pred[1,] - gccmin) / (gccmax - gccmin)
hist(p[1,], xlab = "Initial phenology state")

# Perform an ensemble simulation for the next few time steps
for (t in seq(2, ntime)) {
  for (i in seq_len(nens)) {
    p[t,i] <- logistic_growth(p[t-1,i], r[t-1,i], 1)
    gcc_pred[t,i] <- gccmin + p[t,i] * (gccmax - gccmin)
  }
}

# Plot the results
matplot(p, type = "l", lty = "solid", col = "gray",
        xlab = "Forecast day", ylab = "Phenology state")
matplot(gcc_pred, type = "l", lty = "solid", col = "gray",
        xlab = "Forecast day", ylab = "Predicted GCC")

# Tidy the forecast and save in a CSV
gcc_tidy <- as_tibble(gcc_pred, .name_repair = "unique") %>%
  bind_cols(gdd_data) %>%
  pivot_longer(
    starts_with(".."),
    names_to = "ensemble",
    values_to = "gcc_pred",
    names_prefix = "\\.{3}",
    names_transform = list(ensemble = as.integer)
  )
fname <- file.path(wd, "data", sprintf("forecast-%s.csv", today))
write_csv(gcc_tidy, fname)
