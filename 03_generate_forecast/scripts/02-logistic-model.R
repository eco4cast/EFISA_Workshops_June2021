library(here)

wd <- here("03_generate_forecast", "scripts")
source(file.path(wd, "functions", "logistic-growth.R"))

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

# Simulate some
