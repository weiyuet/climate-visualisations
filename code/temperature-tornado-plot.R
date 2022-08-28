# Load libraries
library(tidyverse)

# Create grid labels
grid_labels <- tibble(x = c(-5, -4, 0, 1),
                      y = 2028,
                      labels = c("+1\u00B0 C", "0\u00B0 C", "0\u00B0 C", "+1\u00B0 C"))

# Create year labels
year_labels <- tibble(x = -2,
                      y = c(seq(1880, 2000, by = 20), 2021))

# Load data
t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na ="***") %>% 
  select(year = Year, all_of(month.abb)) %>% 
  pivot_longer(-year, names_to = "month", values_to = "t_diff") %>% 
  drop_na()

# Plot data
t_data %>% 
  filter(month == "Apr" | month == "Oct") %>% 
  pivot_wider(names_from = "month", values_from = "t_diff") %>% 
  mutate(ave_t = (Oct + Apr) / 2) %>% 
  ggplot(aes(x = -4 - Oct, xend = Apr, y = year, yend = year, colour = ave_t)) +
  geom_vline(xintercept = c(-5, -4, 0, 1), colour = "#FFEC8B") +
  geom_label(data = grid_labels, aes(x = x, y = y, label = labels),
             inherit.aes = FALSE,
             fill = "black", colour = "#FFEC8B", label.size = 0, size = 5) +
  geom_segment(size = 0.9, lineend = "round") +
  geom_text(data = year_labels, aes(x = x, y = y, label = y),
            inherit.aes = FALSE,
            colour = "#0000FF", fontface = "bold", size = 5) +
  scale_colour_gradient2(low = "darkblue", high = "darkred", mid = "white",
                         midpoint = 0, guide = "none") +
  scale_y_continuous(limits = c(NA, 2030), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "", y = "") +
  theme(plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black", colour = "black"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Save png
ggsave("figures/temperature-tornado-plot.png", width = 6, height = 6)
