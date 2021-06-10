library(conflicted)

library(ggplot2)
library(here)
library(lubridate)
library(readr)

wd <- here("03_generate_forecast")
source(file.path(wd, "scripts/functions/sim-temperatures.R"))

# Simulate temperatures for "today" (April 1, 2021) + 14 days into the future
ntime <- 15
today <- "2021-04-01"
temperatures <- simulate_airtemp("2021-01-01", as_date(today) + days(ntime-1))

plt <- ggplot(temperatures) +
  aes(x = time, y = airtemp_c) +
  geom_line() +
  labs(x = "Time", y = expression("Air temperature" ~ (degree * C))) +
  theme_bw()

write_csv(temperatures, file.path(wd, "data/simulated-airtemp.csv"))

ggsave(file.path(wd, "plots/simulated-airtemp.png"), plt,
       width = 7, height = 7, units = "in", dpi = 300)
