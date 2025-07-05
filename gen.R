library(tidyverse)

set.seed(999)

t_uni <- tibble(
  distribution = "uniform",
  n = 1:1000,
  data = runif(1000, min = 0, max = 1)
)

t_norm <- tibble(
  distribution = "normal",
  n = 1:1000,
  data = rnorm(1000, mean = 0, sd = 1)
)

t_binom <- tibble(
  distribution = "binomial",
  n = 1:1000,
  data = rbinom(1000, size = 10, prob = 0.25)
)

t_dexp <- tibble(
  distribution = "dexp",
  n = 1:1000,
  data = dexp(1:0.1:1000, rate = 0.1)
)

t <- rbind(
  t_uni,
  t_norm,
  t_binom,
  t_dexp
)

ggplot(t, aes(x = n, y = data)) +
  geom_line() +
  geom_point() +
  facet_grid(
    rows = vars(distribution),
    scales = "free_y"
  )

ggplot(t, aes(x = data, y = distribution)) +
  # geom_histogram()
  geom_boxplot()

write_tsv(t, "distribution.tsv")

write_tsv(t, "distribution_copy.tsv")
