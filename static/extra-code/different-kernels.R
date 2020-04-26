library(tidyverse)

# Formulas from https://en.wikipedia.org/wiki/Kernel_(statistics)
rec <- function(x) (abs(x) <= 1) * 0.5
tri <- function(x) (abs(x) < 1) * (1 - abs(x))
epan <- function(x) (abs(x) < 1) * 0.75 * (1 - x^2)

ggplot(mapping = aes(x = seq(-1.25, 1.25, 0.1))) + 
  geom_vline(xintercept = 0, size = 1, color = "grey40") +
  stat_function(fun = rec, aes(color = "Uniform"), size = 3) + 
  stat_function(fun = tri, aes(color = "Triangular"), size = 3) +
  stat_function(fun = epan, aes(color = "Epanechnikov"), size = 3) +
  scale_color_manual(values = c("Uniform" = "#FF851B", 
                                "Triangular" = "#0074D9", 
                                "Epanechnikov" = "#85144b"), 
                     breaks = c("Uniform", "Triangular", "Epanechnikov")) + 
  labs(x = "Distance from cutoff", y = "Weight", color = NULL) + 
  theme_bw(base_family = "Fira Sans Condensed") +
  theme(legend.position = "bottom",
        panel.grid = element_blank())


set.seed(1234)
df_kernels <- tibble(x = runif(100, -1, 1),
                     y = runif(100, 0, 100)) %>% 
  mutate(y = ifelse(x > 0, y + 50, y)) %>% 
  mutate(wt_rec = rec(x),
         wt_tri = tri(x),
         wt_epan = epan(x))

df_kernels_long <- df_kernels %>% 
  pivot_longer(cols = starts_with("wt_"), values_to = "weight") %>% 
  mutate(name = recode(name, wt_epan = "Epanechnikov", 
                       wt_tri = "Triangular", 
                       wt_rec = "Rectangular")) %>% 
  mutate(name = fct_inorder(name))

ggplot(df_kernels_long, aes(x = x, y = y)) + 
  geom_point(aes(size = weight, fill = weight), pch = 21, color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0) +
  scale_size_continuous(range = c(2, 10)) +
  scale_fill_viridis_c(option = "viridis", name = "Weight   ", limits = c(0, 1)) +
  guides(size = FALSE, fill = guide_colorbar(barwidth = 20, barheight = 0.5)) +
  facet_wrap(vars(name)) +
  labs(x = "Distance from cutoff", y = NULL) + 
  theme_bw(base_family = "Fira Sans Condensed") +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
