library(tidyverse)
library(broom)
library(scales)
library(rdrobust)

# Function for building the "δ = X; t = X; p = X" string from a model
extract_t_p <- function(x) {
  tidied <- tidy(x) %>% 
    filter(term == "aboveTRUE")
  
  paste0("δ = ", round(tidied$estimate, 2), 
         "; t = ", round(tidied$statistic, 3),
         "; p = ", pvalue(tidied$p.value))
}

# Generate data
set.seed(1234)
too_graphical <- tibble(id = 1:2000) %>% 
  mutate(score = runif(n(), -100, 100)) %>% 
  mutate(treatment_big = ifelse(score < 0, 0, 0.6)) %>% 
  mutate(outcome_big = treatment_big + rnorm(n())) %>% 
  mutate(treatment_medium = ifelse(score < 0, 0, 0.3)) %>% 
  mutate(outcome_medium = treatment_medium + rnorm(n())) %>% 
  mutate(treatment_small = ifelse(score < 0, 0, 0.155)) %>% 
  mutate(outcome_small = treatment_small + rnorm(n())) %>% 
  mutate(above = score >= 0) %>% 
  mutate_at(vars(starts_with("outcome_")), list(~. * 100))

model_big <- lm(outcome_big ~ above, data = too_graphical)
model_medium <- lm(outcome_medium ~ above, data = too_graphical)
model_small <- lm(outcome_small ~ above, data = too_graphical)

# Use rdrobust to build the binned plots, but don't actually plot them since
# they don't return a manipulatable ggplot object
rd_binned_big <- rdplot(y = too_graphical$outcome_big, x = too_graphical$score, 
                        c = 0, p = 1, hide = TRUE)
rd_binned_medium <- rdplot(y = too_graphical$outcome_medium, x = too_graphical$score, 
                           c = 0, p = 1, hide = TRUE)
rd_binned_small <- rdplot(y = too_graphical$outcome_small, x = too_graphical$score, 
                          c = 0, p = 1, hide = TRUE)

# Combine all the binned plots into a big data frame
rd_binned_all <- tribble(
  ~size, ~binned_data, ~poly_data, ~model,
  "Big", rd_binned_big$vars_bins, rd_binned_big$vars_poly, model_big,
  "Medium", rd_binned_medium$vars_bins, rd_binned_medium$vars_poly, model_medium,
  "Small", rd_binned_small$vars_bins, rd_binned_small$vars_poly, model_small
) %>% 
  mutate(letter = LETTERS[1:n()]) %>% 
  mutate(stats = map_chr(model, extract_t_p)) %>% 
  mutate_at(vars(size, stats, letter), list(fct_inorder))

plot_binned <- rd_binned_all %>% 
  unnest(binned_data)

plot_poly <- rd_binned_all %>% 
  unnest(poly_data)

# Plot everything with letters as titles
ggplot(plot_binned, aes(x = rdplot_mean_bin, y = rdplot_mean_y)) +
  geom_point(size = 5, pch = 21, color = "white", alpha = 0.7,
             aes(fill = rdplot_mean_bin <= 0)) +
  geom_vline(xintercept = 0, size = 1, linetype = "21") +
  scale_fill_manual(values = c("#0074D9", "#FF4136"), guide = FALSE) +
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(letter), nrow = 1) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# Plot everything with stats as titles
ggplot(plot_binned, aes(x = rdplot_mean_bin, y = rdplot_mean_y)) +
  geom_point(size = 5, pch = 21, color = "white", alpha = 0.7,
             aes(fill = rdplot_mean_bin <= 0)) +
  geom_line(data = filter(plot_poly, rdplot_x < 0), 
            aes(x = rdplot_x, y = rdplot_y),
            color = "black", size = 3) +
  geom_line(data = filter(plot_poly, rdplot_x > 0), 
            aes(x = rdplot_x, y = rdplot_y),
            color = "black", size = 3) +
  geom_vline(xintercept = 0, size = 1, linetype = "21") +
  scale_fill_manual(values = c("#0074D9", "#FF4136"), guide = FALSE) +
  labs(x = NULL, y = NULL) +
  facet_wrap(vars(stats), nrow = 1) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())
