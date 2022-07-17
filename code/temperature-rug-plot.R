library(tidyverse)

# Load data
zone_data <- read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv")

# Wrangle data
bands <- rev(c("64N-90N", "44N-64N", "24N-44N", "EQU-24N", "24S-EQU", "44S-24S", "64S-44S", "90S-64S"))

zone_data <- read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/ZonAnn.Ts+dSST.csv") %>%
  select(year = Year, all_of(bands)) %>%
  pivot_longer(-year, names_to = "zone", values_to = "t_diff") %>%
  mutate(zone = factor(zone, levels = bands),
         zone_position = as.numeric(zone))

current_year <- zone_data %>% filter(year == 2021)

# Plot
zone_data %>%
  ggplot(aes(x = t_diff, xend = t_diff,
             y = zone_position - 0.25, yend = zone_position + 0.25)) +
  geom_segment(colour = "white", alpha = 0.25) +
  geom_segment(data = current_year,
               aes(colour = t_diff), size = 2, lineend = "round") +
  scale_y_continuous(breaks = 1:8,
                     labels = bands) +
  scale_x_continuous(breaks = seq(-3, 4, 1),
                     labels = seq(-3, 4, 1),
                     limits = c(-3, 4)) +
  scale_colour_gradient2(low = "darkblue", mid = "white", high = "darkred",
                         midpoint = 0, guide = "none") +
  labs(x = "Temperature anomaly (\u00B0C)", y = "",
       title = "Temperature Anomaly by Latitude (1880 - 2021)",
       subtitle = "Bars for 2021 are coloured by the size of the anomaly",
       caption = "Source: GISS Surface Temperature Analysis (GISTEMP)/Model - NASA/GISS") +
  theme(plot.background = element_rect(fill = "black", colour = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(colour = "white", face = "bold"),
        plot.subtitle = element_text(colour = "gray", size = 9),
        plot.title.position = "plot",
        axis.text = element_text(colour = "white"),
        axis.title = element_text(colour = "white"),
        plot.caption = element_text(colour = "white"),
        panel.grid.major.x = element_line(colour = "gray", size = 0.25),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_line(colour = "white"),
        axis.ticks = element_blank())

# Save png
ggsave("figures/temperature-rug-plot.png", width = 6, height = 4)
