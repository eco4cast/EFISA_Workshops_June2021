library(conflicted)
library(here)

library(dplyr)
library(ggplot2)
library(purrr)

wd <- here("03_generate_forecast")
source(file.path(wd, "scripts", "functions", "logistic-growth.R"))

simulate_logistic_growth <- function(nt, p0, r, k = 1, ...) {
  p <- numeric(nt)
  p[1] <- p0
  for (i in seq(2, nt)) {
    p[i] <- logistic_growth(p[i-1], r, k)
  }
  tibble::tibble(t = seq(1, nt), p = p, r = r, p0 = p0, k = k, ...)
}

sims <- map_dfr(
  seq(0.1, 0.5, length.out = 10),
  simulate_logistic_growth,
  p0 = 0.001,
  nt = 50
)

plt <- ggplot(sims) +
  aes(x = t, y = p, group = r, color = r) +
  geom_line() +
  scale_color_gradient(low = "gray70", high = "red3") +
  theme_bw()

ggsave(file.path(wd, "plots", "logistic-model-basic.png"),
       width = 6, height = 6, units = "in", dpi = 300)
