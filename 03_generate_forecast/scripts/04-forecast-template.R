set.seed(8675309)

library(conflicted)   # Manage package conflicts
library(here)         # Manage working directories

library(distributions3)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

wd <- here("03_generate_forecast")
source(file.path(wd, "scripts/functions/logistic-growth.R"))

# Read phenology data
phenofile <- here("01_neon_data_access", "data", "phenology-targets.csv.gz")
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

# Pretend it's May 15
today <- as_date("2021-05-15")
sitename <- "HARV"

# Number of time steps -- today (1) + 14 days into the future
ntime <- 15

# Subset the GDD data to forecast time steps

# Plot growing degree days as a test

# Set parameter distributions and constants
gccmin <- 0.33
gccmax <- 0.45
rslope_inv_d <- Normal(2000, 500)
rint_d <- Normal(0.1, 0.05)

# Variability in parameters

# Get the current phenology state -- for "today" and selected site

# Back-calculate the current p (at t = 1) from GCC.
# Recall that: gcc <- gccmin + p * (gccmax - gccmin)

# Perform an ensemble simulation for the next few time steps

# Plot the results

# Tidy the forecast and save in a CSV