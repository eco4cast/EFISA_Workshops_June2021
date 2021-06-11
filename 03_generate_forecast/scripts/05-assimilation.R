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

# Read old forecast
sitename <- "HARV"
today <- as_date("2021-05-16")
yesterday <- today - days(1)
forecast_file <- file.path(wd, "data", glue("forecast-{yesterday}.csv"))
old_forecast <- read_csv(forecast_file)

# Read phenology data
phenofile <- file.path(wd, "data", "phenology-targets.csv.gz")
pheno_dat <- read_csv(phenofile, guess_max = 1e6)

state_today <- pheno_dat %>%
  filter(time == today, siteID == sitename)

# Particle filter: Assign each ensemble member probabilities according to today's results
ens_probs <- old_forecast %>%
  filter(time == yesterday) %>%
  mutate(pval = with(state_today, dnorm(gcc_pred, gcc_90, gcc_sd))) %>%
  select(ensemble, pval)

arrange(ens_probs, desc(pval))

# Resample ensembles according to probability
ensembles_resamp <- with(ens_probs, sample(ensemble, prob = pval, replace = TRUE)) %>%
  as_tibble()
count(ensembles_resamp, value, sort = TRUE)
new_forecast <- old_forecast %>%
  inner_join(ensembles_resamp, c("ensemble" = "value"))

# Compare the forecasts
ggplot() +
  aes(x = time, y = gcc_pred, group = ensemble) +
  geom_line(aes(color = "old"), data = old_forecast, alpha = 0.4) +
  geom_line(aes(color = "new"), data = new_forecast, alpha = 0.6) +
  scale_color_manual(values = c("old" = "gray60", "new" = "red3")) +
  labs(x = "Time", y = "Predicted GCC") +
  theme_bw()
