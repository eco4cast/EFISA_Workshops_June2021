library(conflicted)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(stringr)
library(tidyr)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

#' Dynamic logistic growth model
#'
#' @param p Population at current time step
#' @param r Population growth rate
#' @param k Carrying capacity
#' @return Population at next time step
logistic_growth <- function(p, r, k) {
  (k * p * exp(r)) / (k + p * (exp(r) - 1))
}

# Example simulation: growth
nt <- 50
p <- numeric(nt)
r <- numeric(nt)
k <- 1
p[1] <- 0.001
r[1] <- 0.3
for (i in seq(2, nt)) {
  p[i] <- logistic_growth(p[i-1], r[i-1], k)
  r[i] <- abs(rnorm(1, r[i - 1], 0.05))
}
plot(p, type = "o")

# Simulate some air temperatures
dates_all <- seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), "days")
nd <- length(dates_all)
airtemp_c_all <- 20 * sin(seq(0, 2 * pi, len = nd) - pi/2) + 15 + rnorm(nd, 0, 5)
plot(dates_all, airtemp_c_all, type = "l")

temperatures <- tibble(time = dates_all, airtemp_c = airtemp_c_all) %>%
  # Calculate growing-degree days
  mutate(gdd = cumsum(pmax(airtemp_c - 10, 0)))

# Let's grab only the ones up to July 1
temperatures_sub <- temperatures %>%
  filter(time < "2021-07-01")
dates <- temperatures_sub$time
airtemp_c <- temperatures_sub$airtemp_c
gdd <- temperatures_sub$gdd

plot(dates, gdd, type = "l")

# Assume a simple linear relationship between GDD and r
rslope <- 1 / 300
rint <- 1e-2
# Scale rate by GDD
r <- rint + rslope * gdd
nt <- length(dates)
p <- numeric(nt)
p[1] <- 1e-5
for (i in seq(2, nt)) {
  p[i] <- logistic_growth(p[i-1], r[i], k)
}
plot(dates, p, type = 'l')

# Ensemble simulation -- introduce variability into the gdd-r scaling relationship
nens <- 500
rslope <- 1 / rnorm(nens, 300, 20)
rint <- rnorm(nens, 1e-1, 5e-2)
p <- matrix(0, nt, nens)
r <- matrix(0, nt, nens)
p[1,] <- 1e-5
for (i in seq_len(nens)) {
  r[,i] <- gdd * rslope[i] + rint[i]
  for (t in seq(2, nt)) {
    p[t,i] <- logistic_growth(p[t-1,i], r[t,i], k)
  }
}
op <- par(mfrow = c(1, 2))
matplot(dates, r, type = "l", xlab = "time", ylab = "Rate parameter")
matplot(dates, p, type = "l", xlab = "time", ylab = "Phenology state")

# Read phenology data
pheno_dat <- readr::read_csv("https://data.ecoforecast.org/targets/phenology/phenology-targets.csv.gz", guess_max = 1e6)

dela_dat <- pheno_dat %>%
  filter(siteID == "DELA")

gccmin <- 0.35
gccmax <- 0.44

dela_dat %>%
  filter(year(time) < 2021) %>%
  ggplot() +
  aes(x = time, y = gcc_90, ymin = gcc_90 - gcc_sd, ymax = gcc_90 + gcc_sd) +
  geom_ribbon(fill = "deepskyblue") +
  geom_line() +
  geom_hline(yintercept = c(gccmin, gccmax), linetype = "dashed")


# TODO: Get real temperature data

# Generate a phenology forecast for first few months of 2021
nens <- 500
rslope <- 1 / rnorm(nens, 2000, 500)
rint <- rnorm(nens, 1e-1, 5e-2)
p <- matrix(0, nt, nens)
r <- matrix(0, nt, nens)
gcc_pred <- matrix(0, nt, nens)
p[1,] <- 1e-5
for (i in seq_len(nens)) {
  r[, i] <- gdd * rslope[i] + rint[i]
  for (t in seq(2, nt)) {
    p[t, i] <- logistic_growth(p[t-1,i], r[t,i], k)
  }
}
gcc_pred <- gccmin + p * (gccmax - gccmin)
colnames(gcc_pred) <- sprintf("gcc_%02d", seq_len(nens))

gcc_tidy <- as_tibble(gcc_pred) %>%
  bind_cols(time = dates) %>%
  pivot_longer(
    -time,
    names_to = "ensemble",
    values_to = "gcc_pred",
    names_prefix = "gcc_",
    names_transform = list(ensemble = as.integer)
  )

dela_2021 <- dela_dat %>%
  filter(year(time) == 2021)

dela_obs <- dela_2021 %>%
  filter(time < "2021-04-01") %>%
  arrange(time)

ggplot(dela_obs) +
  aes(x = time, y = gcc_90) +
  geom_line()

forecast <- gcc_tidy %>%
  left_join(dela_dat %>% filter(year(time) == 2021), "time") %>%
  mutate(obs_gcc_min = gcc_90 - 1.96 * gcc_sd,
         obs_gcc_max = gcc_90 + 1.96 * gcc_sd)

ggplot() +
  aes(x = time) +
  geom_line(aes(y = gcc_pred, group = ensemble), data = gcc_tidy, color = "gray80") +
  geom_ribbon(aes(x = time, ymin = gcc_90 - 1.96 * gcc_sd, ymax = gcc_90 + 1.96 * gcc_sd),
              fill = "deepskyblue", data = dela_2021)

ntime <- 14
p <- matrix(NA_real_, ntime, nens)

# Variability in growth rate
rslope <- 1 / rnorm(nens, 2000, 500)
rint <- rnorm(nens, 1e-1, 5e-2)
r <- matrix(NA_real_, ntime, nens)
temp_sub <- temperatures %>%
  filter(time >= "2021-04-01") %>%
  arrange(time) %>%
  head(14)
gdd_sub <- pull(temp_sub, gdd)
for (i in seq_len(nens)) {
  r[,i] <- rint[i] + rslope[i] * gdd_sub  # Calculate R from GDD and parameters
}

gcc_pred <- matrix(NA_real_, ntime, nens)
# Sample the current state from the observation
dela_now <- dela_2021 %>% filter(time == "2021-04-01")
gcc_pred[1,] <- rnorm(nens, dela_now$gcc_90, dela_now$gcc_sd)
# Back-calculate the current p (at t = 1) from GCC
p[1,] <- (gcc_pred[1,] - gccmin) / (gccmax - gccmin)
# Run the model for the next few time steps
for (t in seq(2, ntime)) {
  for (i in seq_len(nens)) {
    p[t,i] <- logistic_growth(p[t-1,i], r[t-1,i], 1)
    gcc_pred[t,i] <- gccmin + p[t,i] * (gccmax - gccmin)
  }
}

matplot(gcc_pred, type = "l", xlab = "Forecast day")

gcc_pred_tidy <- as_tibble(gcc_pred) %>%
  bind_cols(temp_sub) %>%
  pivot_longer(
    starts_with("V"),
    names_to = "ensemble",
    values_to = "gcc_pred",
    names_prefix = "V",
    names_transform = list(ensemble = as.integer)
  )

##################################################
# Assimilation
dela_tp1 <- dela_2021 %>% filter(time == "2021-04-02")

gcc_pred_probs <- gcc_pred_tidy %>%
  filter(time == "2021-04-02") %>%
  mutate(pval = dnorm(gcc_pred, dela_tp1$gcc_90, dela_tp1$gcc_sd)) %>%
  select(ensemble, pval)

gcc_pred_tidy2 <- gcc_pred_tidy %>%
  left_join(gcc_pred_probs, "ensemble")

ggplot() +
  aes(x = time) +
  geom_line(aes(y = gcc_pred, group = ensemble),
            data = gcc_pred_tidy,
            color = "gray60", alpha = 0.5) +
  geom_pointrange(aes(
    y = gcc_90,
    ymin = gcc_90 - 1.96 * gcc_sd,
    ymax = gcc_90 + 1.96 * gcc_sd
  ), data = delta_tp1, color = "red4") +
  geom_pointrange(aes(
    y = gcc_90,
    ymin = gcc_90 - 1.96 * gcc_sd,
    ymax = gcc_90 + 1.96 * gcc_sd
  ), data = dela_now, color = "blue4")

ggplot() +
  aes(x = time) +
  geom_line(aes(y = gcc_pred, group = ensemble, color = pval),
            data = gcc_pred_tidy2, alpha = 0.5) +
  geom_pointrange(aes(
    y = gcc_90,
    ymin = gcc_90 - 1.96 * gcc_sd,
    ymax = gcc_90 + 1.96 * gcc_sd
  ), data = delta_tp1, color = "red4") +
  geom_pointrange(aes(
    y = gcc_90,
    ymin = gcc_90 - 1.96 * gcc_sd,
    ymax = gcc_90 + 1.96 * gcc_sd
  ), data = dela_now, color = "blue4") +
  scale_color_viridis_c()

ensembles_resamp <- with(gcc_pred_probs, sample(ensemble, prob = pval, replace = TRUE)) %>%
  as_tibble()

gcc_updated <- gcc_pred_tidy2 %>%
  inner_join(ensembles_resamp, c("ensemble" = "value"))

ggplot() +
  aes(x = time, y = gcc_pred, group = ensemble) +
  geom_line(data = gcc_pred_tidy2, alpha = 0.5, color = "gray60") +
  geom_line(data = gcc_updated, alpha = 0.5, color = "red4")
