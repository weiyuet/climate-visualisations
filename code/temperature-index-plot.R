library(tidyverse)


read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>% 
  select(year = Year, t_diff = `J-D`) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = t_diff)) +
  geom_line(aes(colour = "1"), size = 0.5, show.legend = FALSE) +
  geom_point(fill = "white", aes(colour = "1"), shape = 21, show.legend = TRUE) +
  geom_smooth(se = FALSE, aes(colour = "2"), size = 0.5, span = 0.2, show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(1880, 2023, 20), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0,0)) +
  scale_colour_manual(name = NULL, breaks = c(1,2), values = c("gray", "black"), labels = c("Annual mean", "Loess smoothing"),
                      guide = guide_legend(override.aes = list(shape = 15, size = 5))) +
  labs(x = "", y = "Temperature Anomaly w.r.t 1951-1980 (\u00B0C)",
       title = "Global Annual Mean Surface Air Temperature Change",
       subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS).\nCredit: NASA/GISS") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        axis.title.y = element_text(size = 9),
        plot.title.position = "plot",
        plot.title = element_text(margin = margin(b = 10), colour = "red", face = "bold"),
        plot.subtitle = element_text(size = 8, margin = margin(b = 10)),
        legend.position = c(0.15, 0.9),
        legend.title = element_text(size = 0),
        legend.key.height = unit(10, "pt"),
        legend.margin = margin(0,0,0,0))

ggsave("figures/temperature-index-plot.png", width = 7, height = 5)