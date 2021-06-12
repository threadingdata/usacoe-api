library(tidyverse)
library(janitor)
library(httr)
library(furrr)
source('global.R')

#cf <- config::get()
# Metadata ----
# https://www.spk-wc.usace.army.mil/plots/california_new.html
station <- c('sha', 'blb', 'oro', 'bul', 'eng', 'inv', 'fol',
             'cmn', 'nhg', 'frm', 'nml', 'tul', 'dnp', 'exc',
             'lbn', 'bur', 'bar', 'own', 'mar', 'buc', 'hid',
             'mil', 'bdc', 'pnf', 'trm', 'scc', 'isb', 'coy',
             'wrs', 'dlv', 'mrt', 'prs', 'stp', 'boc')
duration <- c('h', 'd')
year <- 1995:2021
station_params <- crossing(station, duration, year) %>%
  arrange(year, station, duration)

# URL building ----
#  https://www.spk-wc.usace.army.mil/plots/california_new.html?name=sha&year=1995&interval=d&tab=data&window=wy
new_urls <- function(station, duration, year){
  url <- paste0('https://www.spk-wc.usace.army.mil/plots/csv/',
         station,
         duration,
         '_',
         year,
         '.plot')

  Sys.sleep(0.75)

  print(str_c('Station: ', station, ' Duration: ', duration, ' Year: ', year))

  df <- read_csv(url) %>%
    mutate(station,
           duration,
           year) %>%
    mutate_all(as.character)

  # Connection error handler ----
  if(nrow(df) < 10) {
    failed_dir <- paste0(cf$usacoe_failed_download,'/', year, '_',
                         station, '_', duration, '.rds')
    print(failed_dir)
    print(df)
    tibble(station = station,
           duration = duration,
           year = year,
           fail = 'yes') %>%
      mutate_all(as.character) %>%
      saveRDS(failed_dir)
  } else{
    success_dir <- paste0(cf$usacoe_success_download,'/', year, '_',
                          station, '_', duration, '.rds')
    print(success_dir)
    print(df)
    df %>%
      mutate_all(as.character) %>%
      saveRDS(success_dir)
  }
}
#new_urls('sha', 'd', 1995)
usacoe_list <- pmap(station_params,
                    safely(new_urls))
