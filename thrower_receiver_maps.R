# Make thrower-receiver mpas

# Load libraries
library(tidyverse)

# Load data
all_passes <- read_csv("final_csvs/all_passes.csv")

# Field map theme
field_map_theme <- list(
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1),
            fill = "white", color = "black"),
  geom_segment(x = 0, y = 90/110,
               xend = 1, yend = 90/110, color = "black"),
  geom_segment(x = 0, y = 20/110,
               xend = 1, yend = 20/110, color = "black"),
  labs(x = "", y = ""),
  coord_fixed(ratio = 2.75,
              xlim = c(0, 1),
              ylim = c(0, 1)),
  theme(strip.text = element_text(vjust = -2),
        strip.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        strip.clip = "off"))

make_thrower_graph_faceted_receiver <- function(thrower_input) {
  thrower_graph <- all_passes %>% 
    mutate(`Thrower error?` = as_factor(`Thrower error?`)) %>% 
    filter(grepl(as.character(thrower_input), Thrower)) %>% 
    ggplot() + 
    field_map_theme + 
    theme(strip.text = element_text(size = 7, angle = 90, hjust = 1),
          plot.subtitle = element_text(size = 9)) + 
    geom_segment(aes(x = pass.start.x, y = 1-pass.start.y,
                     xend = pass.end.x, yend = 1-pass.end.y,
                     color = `Thrower error?`),
                 arrow = arrow(length = unit(0.03, "npc"))) +
    scale_color_manual(values = c("0" = "#B1B3B3", "1" = "#971B2F"),
                       labels = c("no", "yes")) +
    labs(x = "", y = "", title = paste0("Thrower ", thrower_input),
         subtitle = "Receiver listed on side of each map") + 
    facet_wrap(~Receiver, nrow = 2, strip.position = "left")
  ggsave(filename = paste0("Thrower ", as.character(thrower_input), ".png"), 
         plot = thrower_graph, device='png',
         dpi=700, bg = "white", limitsize = FALSE, 
         units = "in", width = 10, height = 5, path = "finished_graphs/passes_by_player")
}


make_receiver_graph_faceted_thrower <- function(receiver_input) {
  receiver_graph <- all_passes %>% 
    mutate(`Thrower error?` = as_factor(`Thrower error?`)) %>% 
    filter(grepl(as.character(receiver_input), Receiver)) %>% 
    ggplot() + 
    field_map_theme + 
    theme(strip.text = element_text(size = 7, angle = 90, hjust = 1),
          plot.subtitle = element_text(size = 9)) + 
    geom_segment(aes(x = pass.start.x, y = 1-pass.start.y,
                     xend = pass.end.x, yend = 1-pass.end.y,
                     color = `Thrower error?`),
                 arrow = arrow(length = unit(0.03, "npc"))) +
    scale_color_manual(values = c("0" = "#B1B3B3", "1" = "#971B2F"),
                       labels = c("no", "yes")) +
    labs(x = "", y = "", title = paste0("Receiver ", receiver_input), 
         subtitle = "Thrower listed on side of each map") + 
    facet_wrap(~Thrower, nrow = 2, strip.position = "left") 
  ggsave(filename = paste0("Receiver ", as.character(receiver_input), ".png"), 
         plot = receiver_graph, device='png',
         dpi=700, bg = "white", limitsize = FALSE, 
         units = "in", width = 10, height = 5, path = "finished_graphs/passes_by_player")
}

list_of_throwers <- unique(all_passes$Thrower)

# Export all pictures for all players 
#map(list_of_throwers, make_receiver_graph_faceted_thrower)
#map(list_of_throwers, make_thrower_graph_faceted_receiver)

