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

# Example simulation: decay
p2 <- numeric(nt)
r2 <- numeric(nt)
p2[1] <- 0.9
r2[1] <- -0.4
for (i in seq(2, nt)) {
  p2[i] <- logistic_growth(p2[i-1], r2[i-1], 1)
  r2[i] <- rnorm(1, r2[i-1], 0.05)
}
plot(p2, type = "o")

# Simulate some air temperatures
nd <- 365
airtemp_c <- 20 * sin(seq(0, 2 * pi, len = nd) - pi/2) + 15 + rnorm(nd, 0, 5)
dates <- seq.Date(as.Date("2019-01-01"), as.Date("2019-12-31"), "days")
plot(dates, airtemp_c, type = "l")

gdd <- cumsum(pmax(airtemp_c, 5))
cdd_raw <- pmax(20 - airtemp_c, 0)
cdd_raw[0:150] <- 0
cdd <- cumsum(cdd_raw)
plot(dates, gdd, type = "l", col = "red")
lines(dates, cdd, type = "l", col = "blue")

# Scale growth and decay rates
rgreen <- sqrt(gdd) / 140
rsen <- -(cdd)^(1/6) / 20

# Single simulation of combined phenology model
pgreen <- numeric(nd)
psen <- numeric(nd)
pgreen[1] <- 1e-5
psen[1] <- 1 - 1e-5
for (i in seq(2, nd)) {
  pgreen[i] <- logistic_growth(pgreen[i-1], rgreen[i], k)
  psen[i] <- logistic_growth(psen[i-1], rsen[i], k)
}
p <- pmin(pgreen, psen)
plot(dates, p, type = "l")

# Alternate implementation: Switch phases
p <- numeric(nd)
pgreen <- 1e-5
psen <- 1 - 1e-5
p[1] <- pgreen
greenup <- TRUE
for (i in seq(2, nd)) {
  if (greenup) {
    r <- rgreen[i]
  } else {
    r <- rsen[i]
  }
  p[i] <- logistic_growth(p[i-1], r, k)
  # Swtich phases when we flatten out near 1
  if (greenup && p[i] > 0.9 && abs(p[i] - p[i-1]) < 1e-5) {
    greenup <- FALSE
    message("Switch to senescence, step ", i)
    p[i] <- psen
  }
  # Switch back when we flatten out near 0 (for multi-year simulations)
  if (!greenup && p[i] < 0.1 && abs(p[i] - p[i-1]) < 1e-5) {
    greenup <- TRUE
    message("Switch to greenup, step ", i)
    p[i] <- pgreen
  }
}
plot(dates, p, type = 'l')

# Ensemble simulation
nsamp <- 500
# Some variation around greenup rate-temperature relationship
rgreen <- matrix(numeric(), nd, nsamp)
rgreen_t <- rnorm(nsamp, 140, 10)
rsen_t <- rnorm(nsamp, 20, 3)
rsen <- matrix(numeric(), nd, nsamp)
for (i in seq_len(nsamp)) {
  rgreen[,i] <- sqrt(gdd) / rgreen_t[i]
  rsen[,i] <- -(cdd)^(1/6) / rsen_t[i]
}

pgreen <- 1e-5
psen <- 1 - 1e-5
p <- matrix(numeric(), nd, nsamp)
p[1,] <- pgreen
greenup <- rep(TRUE, nsamp)
r <- numeric(nsamp)
for (i in seq(2, nd)) {
  r[greenup] <- rgreen[i, greenup]
  r[!greenup] <- rsen[i, !greenup]
  p[i,] <- logistic_growth(p[i-1,], r, k)
  # Swtich phases when we flatten out near 1
  for (s in seq_len(nsamp)) {
    if (greenup[s] && p[i,s] > 0.9 && abs(p[i,s] - p[i-1,s]) < 1e-4) {
      greenup[s] <- FALSE
      p[i,s] <- psen
    }
  }
}
matplot(dates, p, type = 'l', lty = "solid", col = "gray")
