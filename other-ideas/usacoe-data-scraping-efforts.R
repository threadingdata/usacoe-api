# ==============================================================================

usacoe_list <- map(usacoe_urls[1:10], read_csv)


# Determine how many stations this is available for!!  Iterate through all and store.
# 'https://www.spk-wc.usace.army.mil/plots/csv/shad_2021.plot'
years <- c(1995:1998)
2020:2021
acoe_urls <- paste0('https://www.spk-wc.usace.army.mil/plots/csv/shad_', years,'.plot')
sha_data <- map(acoe_urls, read_csv)
sha_clean <- sha_data %>%
  map(~ .x %>% as_tibble()) %>%
  keep(~ nrow(.x) > 10) %>%
  map(~ .x %>% mutate_all(as.character)) %>%
  bind_rows %>%
  janitor::clean_names() %>%
  mutate_at(vars(top_of_conservation_ac_ft:gross_pool_elev), as.numeric) %>%
  pivot_longer(cols = c(top_of_conservation_ac_ft:gross_pool_elev)) %>%
  set_names(c('date', 'name', 'value'))

sha_clean %>%
  ggplot(aes(date, value)) +
  geom_point() +
  facet_wrap(~ name, scales = 'free_y')

sha_storage <- blb_clean %>%
  filter(name == 'storage') %>%
  mutate(source = 'acoe')
sha_acoe <- sha_storage %>%
  select(date, value, source)

shasta_storage <- conn %>%
  tbl('cdec_daily') %>%
  filter(station == 'sha',
         sensor == '15') %>%
  collect()

sha_cdec <- shasta_storage %>%
  select(date, value, source)

sha_compare <- sha_cdec %>%
  bind_rows(sha_acoe) %>%
  filter(between(as_date(date), ymd('2018-01-01'), today()))

sha_compare %>%
  ggplot(aes(date, value, color = source)) +
  geom_point()

# Corps and Section 7 Projects in CA - Hourly and Daily Data & Plots ----
# https://www.spk-wc.usace.army.mil/plots/california.html
# https://www.spk-wc.usace.army.mil/fcgi-bin/getplottext.py?plot=blbqr&length=wy&wy=2019&interval=h
corps_data_urls <- read_html('https://www.spk-wc.usace.army.mil/plots/california.html') %>%
  html_nodes('#plottable > tr > td > a') %>%
  html_attr('href') %>%
  as_tibble() %>%
  rename(urls = value) %>%
  filter(str_detect(urls, 'fcgi-bin/'))
corps_wy_urls <- corps_data_urls %>%
  filter(str_detect(urls, 'length=wy'))
corps_wy_urls %>%
  View

# Determine how many months/years and for which stations.  January 1999 for project COY...below.
https://www.spk-wc.usace.army.mil/fcgi-bin/getplottext.py?archive=true&plot=oror&length=wy&interval=d&wy=1995
read_csv('https://www.spk-wc.usace.army.mil/fcgi-bin/monthly.py?month=jan&year=2021&project=coy')
https://www.spk-wc.usace.army.mil/fcgi-bin/monthly.py?month=jan&year=2021&project=wrs

library(rvest)
daily_ops_url <- function(months, years, project) {
  paste0('https://www.spk-wc.usace.army.mil/fcgi-bin/monthly.py?month=', months,
         '&year=', years,
         '&project=',project)
}

daily_ops_coy <- function(month, year, project){
  Sys.sleep(0.3)
  print(str_c(year, month, project))
  url <- daily_ops_url(month, year, project)

  html_obj <- read_html(url)
  table_text <- html_obj %>%
    html_nodes('pre') %>%
    html_text
  df <- table_text %>%
    .[[1]] %>%
    str_split('\n') %>%
    .[[1]] %>%
    .[16:52] %>%
    str_split('[ ]+') %>%
    map(~ .x[2:17] %>%
          set_names(paste0('row_', 1:16))) %>%
    bind_rows %>%
    filter(!is.na(row_2)) %>%
    mutate(month, year, project)
  df
}
df <- daily_ops_coy('jan', '2005', 'coy')

# Iterate through each month and year...download all ----
month <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct',
           'nov', 'dec')
year <- 1999:2021
mo_years <- crossing(month, year) %>%
  add_column(project = 'coy') %>%
  arrange(year)
coyote_list <- pmap(mo_years, safely(daily_ops_coy))
coyote_df <- coyote_list %>%
  map('result') %>%
  bind_rows %>%
  mutate_at(vars(row_1:row_16), as.numeric) %>%
  mutate(date = ymd(str_c(year, month, row_1))) %>%
  pivot_longer(row_2:row_16)
coyote_df %>%
  ggplot(aes(date, value, group = name)) +
  #  geom_point() +
  geom_line() +
  labs(title = 'USACE Data') +
  facet_wrap(~ name, scales = 'free_y')

#
# html_obj <- read_html('https://www.spk-wc.usace.army.mil/fcgi-bin/monthly.py?month=jan&year=2021&project=coy')
# table_text <- html_obj %>%
#   html_nodes('pre') %>%
#   html_text
# df <- table_text %>%
#   .[[1]] %>%
#   str_split('\n') %>%
#   .[[1]] %>%
#   .[16:52] %>%
#   str_split('[ ]+') %>%
#   map(~ .x[2:17] %>%
#         set_names(paste0('row_', 1:16))) %>%
#   bind_rows %>%
#   filter(!is.na(row_2))

df %>%
  mutate_at(vars(row_1:row_16), as.numeric) %>%
  pivot_longer(row_2:row_16) %>%
  ggplot(aes(row_1, value, group = name)) +
  geom_point() +
  geom_line() +
  labs(title = 'USACE Data') +
  facet_wrap(~ name, scales = 'free_y')

