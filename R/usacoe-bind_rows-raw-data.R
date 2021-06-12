library(tidyverse)
library(lubridate)
library(furrr)

# Group files by station and duration - bind_rows() together
saved_files <- dir('data/usacoe-history/success/')
directory <- str_c('data/usacoe-history/success/', saved_files)
acoe <- tibble(station = saved_files,
               directory = directory) %>%
  separate(station, into = c('year', 'station', 'duration', 'extenstion'), sep = '_|\\.') %>%
  group_nest(station, duration, keep = TRUE, .key = 'raw')
acoe

row_bind_acoe <- function(dir_file){
  map(dir_file %>%
        pull(directory),
      readRDS) %>%
    bind_rows
}
#dir_file <- acoe[1, ]$data[[1]]
#row_bind_acoe(dir_file)

# Pivot columns to avoid non-match
pivot_cols <- function(.df){
  .df %>%
    pivot_longer(cols = c(-contains('Date'), -year, -duration, -station))
}

plan(multisession(workers = 32))
aceo_combine <- acoe %>%
  mutate(data = future_map(raw, row_bind_acoe,
                           .progress = TRUE),
         pivoted_data = map(data, pivot_cols))
aceo_combine

pivoted_data <- aceo_combine %>%
  select(pivoted_data) %>%
  unnest(pivoted_data) %>%
  mutate(year = year %>% as.integer,
         value = value %>% as.numeric,
         date = ymd_hms(`ISO 8601 Date Time`))

# Save data as RDS file ----
#pivoted_data %>%
#  saveRDS('data/usacoe-pivot-raw-not-cleaned-yet-20200526.rds')

# Inspect the data ----
pivoted_data %>%
  slice(1:200) %>%
  View

# Shasta, Oroville, Folsom ----
major_res <- pivoted_data %>%
  filter(station %in% c('sha', 'oro', 'fol')) %>%
  drop_na

# Plot major reservoirs ----
major_res %>%
  filter(str_detect(name, 'Storage'),
         !str_detect(name, 'notes'),
         duration == 'd') %>%
  ggplot(aes(date, value)) +
  geom_line() +
  labs(title = 'Army Corps, Bitches!') +
  theme_minimal() +
  facet_wrap(~ name + station + duration, scales = 'free_y', ncol = 1)

pew_gong()
