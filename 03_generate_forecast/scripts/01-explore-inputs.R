##################################################
# 01 -- Explore the data
##################################################

# Look at the input data
library(conflicted)

library(dplyr)
library(ggplot2)
library(here)
library(lubridate)
library(readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

heredir <- here("03_generate_forecast")
datadir <- file.path(heredir, "data")
phenofile <- file.path(datadir, "phenology-targets.csv.gz")

if (!file.exists(phenofile)) {
  download.file("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", phenofile)
}
pheno_dat <- read_csv(phenofile, guess_max = 1e6)

pheno_dat

# Look at site-level data
plt <- pheno_dat %>%
  filter(siteID == "DELA") %>%
  ggplot() +
  aes(x = time, ymin = gcc_90 - 1.96*gcc_sd, ymax = gcc_90 + 1.96*gcc_sd,
      y = gcc_90) +
  geom_ribbon(fill = "deepskyblue") +
  geom_line() +
  # Add horizontal line showing the min and max
  geom_hline(yintercept = c(0.35, 0.44), linetype = "dashed") +
  theme_bw() +
  labs(x = "Time", y = "GCC (Mean +/- 1.96SD)")

ggsave(file.path(heredir, "plots", "dela-phenology.png"), plt,
       width = 10, height = 7, units = "in", dpi = 300)
