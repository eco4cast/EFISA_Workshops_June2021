logistic_growth_dt <- function(p, t, mg, g, ms, s) {
  le <- exp(-mg * (t - g))
  re <- exp(ms * (t - s))
  dp_dt <- mg * le / (1 + le)^2 - ms * re / (1 + re)^2
  max(p + dp_dt, 0)
}

nt <- 50
p <- numeric(nt)
mg <- 0.5
g <- 20
ms <- 0.5
s <- 42

p[1] <- 0
for (t in seq(2, nt)) {
  p[t] <- logistic_growth_dt(p[t-1], t-1, mg, g, ms, s)
}

simulate_growth <- function(nt=365, p0=0, mg=0.2, g=80, ms=0.1, s=306) {
  p <- numeric(nt)
  p[1] <- p0
  for (t in seq(2, nt)) {
    p[t] <- logistic_growth_dt(p[t-1], t-1, mg, g, ms, s)
  }
  p
}
nt <- 365
mg_sim <- tibble(mg = seq(0.04, 0.5, 0.01)) %>%
  mutate(t = list(seq_len(nt)), p = map(mg, ~simulate_growth(mg = .x, nt = nt))) %>%
  unnest(c(t, p))
ggplot(mg_sim) +
  aes(x = t, y = p, color = mg, group = mg) +
  geom_line() +
  scale_color_viridis_c()

tibble(ms = seq(0.04, 0.5, 0.01)) %>%
  mutate(t = list(seq_len(nt)), p = map(ms, ~simulate_growth(ms = .x, nt = nt))) %>%
  unnest(c(t, p)) %>%
  ggplot() +
  aes(x = t, y = p, color = ms, group = ms) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red")

tibble(g = seq(20, 40, 5)) %>%
  mutate(t = list(seq_len(nt)), p = map(g, ~simulate_growth(g = .x, nt = nt))) %>%
  unnest(c(t, p)) %>%
  ggplot() +
  aes(x = t, y = p, color = g, group = g) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red")

tibble(s = seq(250, 320, 5)) %>%
  mutate(t = list(seq_len(nt)), p = map(s, ~simulate_growth(s = .x, nt = nt))) %>%
  unnest(c(t, p)) %>%
  ggplot() +
  aes(x = t, y = p, color = s, group = s) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red")
