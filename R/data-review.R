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
