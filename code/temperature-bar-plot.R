library(tidyverse)
library(scales)
library(glue)


t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na ="***") %>%
  select(year = Year, t_diff = `J-D`) %>% 
  drop_na()

annotation <- t_data %>% 
  arrange(year) %>% 
  slice(1, n()) %>% 
  mutate(t_diff = 0,
         x = year + c(-5,5))

t_data %>% 
  ggplot(aes(x = year, y = t_diff, fill = t_diff)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = annotation, aes(x = x, label = year), colour = "white") +
  geom_text(x = 1880, y = 1, hjust = 0,
            label = glue("Global temperatures have increased by over {max(t_data$t_diff)}\u00B0C since {min(t_data$year)}"),
            colour = "white") +
  scale_fill_stepsn(colours = c("darkblue", "white", "darkred"),
                    values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
                    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
                    n.breaks = 9) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"))

ggsave("figures/temperature-bar-plot.png", width = 7, height = 4)