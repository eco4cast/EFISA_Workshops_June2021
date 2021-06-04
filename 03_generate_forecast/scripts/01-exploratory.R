##################################################
# 01 -- Explore the data
##################################################

# Look at the input data
library(conflicted)

library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

pheno_dat <- read_csv("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", guess_max = 1e6)

pheno_dat

# Look at site-level data
pheno_dat %>%
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
