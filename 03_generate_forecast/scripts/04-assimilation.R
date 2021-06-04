library(conflicted)
library(here)

wd <- here("03_generate_forecast/scripts")

library(dplyr)
library(ggplot2)
library(readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Read old forecast
forecast_dir <- file.path(wd, "..", "data")
forecasts <- list.files(forecast_dir, "forecast-.*.csv", full.names = TRUE)
old_forecast <- read_csv(tail(forecasts, 1))

# Read phenology data
pheno_dat <- read_csv("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", guess_max = 1e6)

today <- "2021-04-02"
state_today <- pheno_dat %>%
  filter(time == today, siteID == "DELA")

# Particle filter: Assign each ensemble member probabilities according to today's results
yesterday <- "2021-04-01"
ens_probs <- old_forecast %>%
  filter(time == yesterday) %>%
  mutate(pval = with(state_today, dnorm(gcc_pred, gcc_90, gcc_sd))) %>%
  select(ensemble, pval)

arrange(ens_probs, desc(pval))

# Resample ensembles according to probability
ensembles_resamp <- with(ens_probs, sample(ensemble, prob = pval, replace = TRUE)) %>%
  as_tibble()
new_forecast <- old_forecast %>%
  inner_join(ensembles_resamp, c("ensemble" = "value"))

# Compare the forecasts
ggplot() +
  aes(x = time, y = gcc_pred, group = ensemble) +
  geom_line(aes(color = "old"), data = old_forecast, alpha = 0.4) +
  geom_line(aes(color = "new"), data = new_forecast, alpha = 0.4) +
  scale_color_manual(values = c("old" = "gray70", "new" = "red4"))
