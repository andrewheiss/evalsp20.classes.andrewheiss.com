library(tidyverse)
library(scales)
library(broom)
library(gganimate)

# Original by David Robinson at http://varianceexplained.org/files/loess.html


# Basic static plot -------------------------------------------------------

economics_filtered <- economics %>% 
  # Only look after 2000
  filter(date > "2000-01-01") %>% 
  # Make this numeric
  mutate(date_num = as.numeric(date)) %>% 
  # Scale this down to 0-1
  mutate(unemployment = uempmed / 100)

# Standard loess curve with span - 0.5
ggplot(economics_filtered, 
       aes(x = as.Date(date_num, origin = "1970-01-01"), 
           y = unemployment)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, 
              span = 0.5, se = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Unemployment") +
  theme_bw(base_family = "Fira Sans Condensed")


# Single loess window at 0.5 span -----------------------------------------

# Building the line
data_at_each_x <- economics_filtered %>%
  # Expand the data so all the variables are repeated for every possible x
  crossing(center = unique(economics_filtered$date_num)) %>%
  # Do all these calculations for each possible x
  group_by(center) %>%
  # Find the distance between each x and the current group's x
  mutate(dist = abs(date_num - center)) %>% 
  # Scale the distance by the maximum of the distance
  mutate(scaled_distance_full_data = rank(dist) / n()) %>% 
  # Only look at values that are within the span
  filter(scaled_distance_full_data <= 0.5) %>% 
  # Calculate the triple cubic weight
  # See https://www.itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm and
  # https://www.itl.nist.gov/div898/handbook/pmd/section1/dep/dep144.htm
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3) %>% 
  # Put each of these groups into a nested list column
  nest() %>% 
  # Run a weighted linear model on each nested data frame and find the predicted
  # value from the model for that x
  mutate(output = map(data, ~lm(unemployment ~ date_num, data = ., weight = weight))) %>% 
  mutate(model = map2(output, center, ~augment(.x, newdata = tibble(date_num = .y))))

# Unnest the full data
full_data <- data_at_each_x %>% 
  select(center, data) %>% 
  unnest(data)

# Unnest just the predicted unemployment values
point_data <- data_at_each_x %>% 
  select(center, model) %>% 
  unnest(model)

# Calculate a single loess fit for the regular data
single_fit <- augment(loess(unemployment ~ date_num, 
                            data = economics_filtered, 
                            degree = 1, span = 0.5))

# Plot everything!
plot_loess_window <- ggplot(full_data, aes(x = as.Date(date_num, origin = "1970-01-01"), 
                                           y = unemployment)) +
  # Faded points from original data
  geom_point(data = economics_filtered, 
             size = 1, alpha = 0.5, color = "grey75") +
  # Points with transparency based on loess weight
  geom_point(aes(alpha = weight), size = 1, color = "#0074D9") +
  # Linear model for each possible x value
  geom_smooth(aes(group = center, weight = weight), color = "#001f3f",
              formula = y ~ x, method = "lm", se = FALSE) +
  # Predicted point from linear model for each possible x value
  geom_point(aes(y = .fitted, group = date_num),
             data = point_data, color = "#FF4136", size = 3) +
  # Vertical line showing current x value
  geom_vline(aes(xintercept = center), lty = 2) +
  # Single loess fit
  geom_line(aes(y = .fitted), data = single_fit, color = "#85144b") +
  # Formatting stuff
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Unemployment", alpha = "Weight") +
  theme_bw(base_size = 14, base_family = "Fira Sans Condensed") +
  theme(legend.position = "bottom") +
  # gganimate stuff
  transition_manual(center)

animated_loess_window_gif <- animate(plot_loess_window, 
                                     width = 1200, height = 720, res = 150)
anim_save(animated_loess_window_gif, filename = "loess_window.gif")
animated_loess_window_gif

# Can't use anim_save() with videos?
# https://github.com/thomasp85/gganimate/issues/340
animated_loess_window_mp4 <- animate(plot_loess_window, 
                                     width = 1200, height = 720, res = 150,
                                     renderer = av_renderer("loess_window.mp4"))


# Different spans ---------------------------------------------------------

different_spans <- tibble(span = seq(0.1, 1, by = 0.1)) %>% 
  mutate(fitted_line = map(span, 
                           ~augment(loess(unemployment ~ date_num, 
                                          data = economics_filtered,
                                          degree = 1, span = .)))) %>% 
  unnest(fitted_line)

plot_diff_spans_single <- ggplot(economics_filtered, 
                                 aes(x = as.Date(date_num, origin = "1970-01-01"), 
                                     y = unemployment)) +
  geom_point() +
  geom_line(aes(y = .fitted), data = different_spans, size = 1.5, color = "#2ECC40") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Unemployment", 
       title = "Span: {closest_state}") +
  theme_bw(base_size = 14, base_family = "Fira Sans Condensed") +
  theme(plot.title = element_text(face = "bold", size = rel(1.5))) +
  # gganimate stuff
  transition_states(span) +
  ease_aes('cubic-in-out')

animated_diff_spans_single_gif <- animate(plot_diff_spans_single, 
                                          width = 1200, height = 720, res = 150)
anim_save(animated_diff_spans_single_gif, filename = "diff_spans_single.gif")
animated_diff_spans_single_gif

animated_diff_spans_single_mp4 <- animate(plot_diff_spans_single, 
                                          width = 1200, height = 720, res = 150,
                                          renderer = av_renderer("diff_spans_single.mp4"))


# Different spans all at once! --------------------------------------------

spans <- c(0.25, 0.5, 0.75, 1)

diff_spans <- tibble(span = spans) %>% 
  mutate(fit = map(span, ~augment(loess(unemployment ~ date_num, 
                                        data = economics_filtered, 
                                        degree = 1, span = .)))) %>% 
  unnest(fit)

data_at_each_x_diff_spans <- economics_filtered %>%
  crossing(span = spans, center = unique(economics_filtered$date_num)) %>%
  group_by(span, center) %>%
  mutate(dist = abs(date_num - center)) %>% 
  mutate(scaled_distance_full_data = rank(dist) / n()) %>% 
  filter(scaled_distance_full_data <= span) %>% 
  mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3) %>% 
  nest() %>% 
  mutate(output = map(data, ~lm(unemployment ~ date_num, data = ., weight = weight))) %>% 
  mutate(model = map2(output, center, ~augment(.x, newdata = tibble(date_num = .y)))) %>% 
  ungroup()

full_data_diff_spans <- data_at_each_x_diff_spans %>% 
  select(span, center, data) %>% 
  unnest(data)

point_data_diff_spans <- data_at_each_x_diff_spans %>% 
  select(span, center, model) %>% 
  unnest(model)

# Make mega plot
plot_diff_spans <- ggplot(full_data_diff_spans, 
                          aes(x = as.Date(date_num, origin = "1970-01-01"), 
                              y = unemployment)) +
  # Faded points from original data
  geom_point(data = economics_filtered, size = 1, alpha = 0.5, color = "grey75") +
  # Points with transparency based on loess weight
  geom_point(aes(alpha = weight), size = 1, color = "#0074D9") +
  # Linear model for each possible x value
  geom_smooth(aes(group = center, weight = weight), color = "#001f3f",
              formula = y ~ x, method = "lm", se = FALSE) +
  # Predicted point from linear model for each possible x value
  geom_point(aes(y = .fitted),
             data = point_data_diff_spans, color = "#FF4136", size = 3) +
  # Vertical line showing current x value
  geom_vline(aes(xintercept = center), lty = 2) +
  # Single loess fit
  geom_line(aes(y = .fitted), data = diff_spans, color = "#85144b") +
  # Formatting stuff
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Unemployment", alpha = "Weight", title = "Different spans") +
  facet_wrap(vars(span)) +
  theme_bw(base_size = 14, base_family = "Fira Sans Condensed") +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold")) +
  # gganimate stuff
  transition_manual(center)

animated_diff_spans_gif <- animate(plot_diff_spans, 
                                   width = 1200, height = 720, res = 150)
anim_save(animated_diff_spans_gif, filename = "diff_spans.gif")
animated_diff_spans_gif

animated_diff_spans_mp4 <- animate(plot_diff_spans, 
                                   width = 1200, height = 720, res = 150,
                                   renderer = av_renderer("diff_spans.mp4"))
