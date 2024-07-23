library(TidyDensity)

set.seed(123)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(.value_type = "cum_sum") |>
  tidy_random_walk_autoplot()


# random walk with sampling
set.seed(123)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(.value_type = "cum_sum", .sample = TRUE) |>
  tidy_random_walk_autoplot()

# random walk with sampling and replacement
set.seed(123)
tidy_normal(.num_sims = 25, .n = 100) |>
  tidy_random_walk(
    .value_type = "cum_sum", 
    .sample = TRUE, 
    .replace = TRUE
  ) |>
  tidy_random_walk_autoplot()

# comparing different random sampling methods
library(ggplot2)
library(dplyr)

set.seed(123)
df <- rbind(
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum") |>
    mutate(type = "No_Sample"),
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum", .sample = TRUE) |>
    mutate(type = "Sample_No_Replace"),
  tidy_normal(.num_sims = 25, .n = 100) |>
    tidy_random_walk(.value_type = "cum_sum", .sample = TRUE, .replace = TRUE) |>
    mutate(type = "Sample_Replace")
) |>
  select(sim_number, x, random_walk_value, type) |>
  mutate(
    low_ci = -1.96 * sqrt(x),
    hi_ci = 1.96 * sqrt(x)
  )

atb <- attributes(df)

df |>
  ggplot(aes(
    x = x, 
    y = random_walk_value, 
    group = sim_number, 
    color = factor(type))
  ) +
  geom_line(aes(alpha = 0.382)) +
  geom_line(aes(y = low_ci, group = sim_number), 
            linetype = "dashed", linewidth = 0.6, color = "black") +
  geom_line(aes(y = hi_ci, group = sim_number), 
            linetype = "dashed", linewidth = 0.6, color = "black") +
  theme_minimal() +
  theme(legend.position="none") +
  facet_wrap(~type) +
  labs(
    x = "Time",
    y = "Random Walk Value",
    title = "Random Walk with Different Sampling Methods",
    subtitle = paste0("Simulations: ", atb$all$.num_sims, 
                      " | Steps: ", atb$all$.n,
                      " | Distribution: ", atb$all$dist_with_params
    )
  )
