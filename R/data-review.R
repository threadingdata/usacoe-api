library(tidyverse)
usacoe_df <- readRDS('data/usacoe-pivot-raw-not-cleaned-yet-20200526.rds')

usacoe_df %>%
  count(station)

sha <- usacoe_df %>%
  filter(station %in% c('sha', 'oro', 'fol'),
         name == 'Storage (ac-ft)')
sha %>%
  ggplot(aes(date, value, color = station)) +
  geom_point()

# Plot major reservoirs ----
usacoe_df %>%
  filter(str_detect(name, 'Storage'),
         !str_detect(name, 'notes'),
         duration == 'd') %>%
  ggplot(aes(date, value)) +
  geom_line() +
  labs(title = 'Army Corps, Bitches!') +
  theme_minimal() +
  facet_wrap(~ name + station + duration, scales = 'free_y', ncol = 4)
