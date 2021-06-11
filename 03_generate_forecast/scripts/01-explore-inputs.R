##################################################
# 01 -- Explore the data
##################################################

# Look at the input data
library(conflicted)

library(dplyr)
library(ggplot2)
library(glue)
library(here)
library(lubridate)
library(readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

heredir <- here("03_generate_forecast")
datadir <- file.path(heredir, "data")
phenofile <- file.path(datadir, "phenology-targets.csv.gz")
sitename <- "HARV"

if (!file.exists(phenofile)) {
  download.file("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", phenofile)
}
pheno_dat <- read_csv(phenofile, guess_max = 1e6)

pheno_dat

# Look at site-level data
plt <- pheno_dat %>%
  filter(siteID == sitename, year(time) < 2021) %>%
  ggplot() +
  aes(
    x = time,
    y = gcc_90,
    ymin = gcc_90 - 1.96*gcc_sd,
    ymax = gcc_90 + 1.96*gcc_sd
  ) +
  geom_ribbon(fill = "deepskyblue") +
  geom_line() +
  # Add horizontal line showing the min and max
  geom_hline(yintercept = c(0.33, 0.45), linetype = "dashed") +
  theme_bw() +
  labs(x = "Time", y = "GCC (Mean +/- 1.96SD)")
plt

ggsave(file.path(heredir, "plots", glue("{sitename}-phenology.png")), plt,
       width = 10, height = 7, units = "in", dpi = 300)
