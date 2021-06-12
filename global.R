library(plumber)
library(tidyverse)
library(lubridate)
#library(redoc)
library(pool)
library(rapidoc)
library(yaml)
library(config)
library(RPostgres)
library(pdftools)
source('set-env-vars.R')
source('usbr/usbr_cron_scripts.R')

# Helpers ----
`%nin%` <- Negate(`%in%`)

# Pull in configs ----
cf <- config::get()

# Connect to the database ----
conn <- pool::dbPool(drv = RPostgres::Postgres(),
                     host = cf$db_host,
                     port = cf$db_port,
                     user = cf$db_user,
                     password = cf$db_password,
                     dbname = cf$db_name,
                     minSize = cf$db_pool_minSize)

#,
#                     maxSize = cf$db_pool_maxSize,
#                     idleTimeout = cf$db_pool_idleTimeout)
#conn %>% pool::poolClose()

#conn
#conn %>%
#  dbListTables
#conn %>% pool::poolClose()

users <- tibble(
  id = 1:3,
  username = c('joe', 'kim', 'derek'),
  groups = c('users', 'admin,users', 'hmfic'))
users

# Database Index/Upsert Functionality ----
dbCreateIndexedTable <- function(conn, table_name, .df, constraint_name,
                                 constraint_cols, fields,
                                 row.names = NULL, temporary = FALSE, ...){
  dbExecute(conn, 'start transaction;')
  dbWriteTable(conn,
               table_name,
               .df,
               temporary = temporary,
               overwrite = TRUE,
               append = FALSE)
  dbExecute(conn, 'commit;')
  if(dbExistsTable(conn, table_name)){
    dbAddUniqueConstraint(conn,
                          table_name,
                          constraint_name,
                          constraint_cols)
    constraint_cols <- paste0(constraint_cols, collapse = ', ')
    print(paste('Unique constraint table created with',
                paste0('(', constraint_cols, ')'), 'indexes.'))
  } else{
    return(paste('Failed to create table.'))
  }
}

dbAddUniqueConstraint <- function(conn, table_name, constraint_name, constraint_cols){
  # Assemble the index columns in a vector to feed to a SQL Statment
  constraint_cols <- paste0(constraint_cols, collapse = ', ')
  # Build SQL Statement to ALTER the existing table, adding UNIQUE CONSTRAINTS
  statement <- paste('ALTER TABLE',
                     table_name,
                     'ADD CONSTRAINT',
                     constraint_name,
                     'UNIQUE')
  # Correct for SQL Syntax
  statement <- paste0(statement, ' (', constraint_cols ,');')
  pool::dbExecute(conn, 'start transaction;')
  pool::dbExecute(conn, statement)
  pool::dbExecute(conn, 'commit;')
}


dbDropUniqueConstraint <- function(conn, table_name, constraint_name){
  # Assemble SQL Statement to ALTER table and DROP index constraints
  statement <- paste('ALTER TABLE',
                     table_name,
                     'DROP INDEX',
                     constraint_name)
  # Correct SQL Syntax
  statement <- paste0(statement,';')
  pool::dbExecute(conn, 'start transaction;')
  pool::dbExecute(conn, statement)
  pool::dbExecute(conn, 'commit;')
}


sql_upsert_statement <- function(table_name,
                                 table_cols,
                                 select_cols,
                                 conflict_cols,
                                 on_conflict_action = 'do_nothing',
                                 update_cols = NULL){

  # Build the beginning of the SQL Statement
  table_cols <- paste0(table_cols, collapse = ', ')
  table_name_cols <- paste0(table_name, '(', table_cols, ')', collapse = '')
  select_cols <- paste0(select_cols, collapse = ', ')
  conflict_cols <- paste0(conflict_cols, collapse = ', ')
  conflict_cols <- paste0('(', conflict_cols, ')', collapse = '')
  sql_statement_head <- paste('INSERT INTO', table_name_cols,
                              'SELECT', select_cols,
                              'FROM temp_table',
                              'ON CONFLICT', conflict_cols)

  if(is.null(update_cols)){
    update_cols <- 'none_specified'
  } else {
    update_cols <- paste0(update_cols,'=excluded.', update_cols, collapse = ', ')
  }

  sql_statement_tail <- switch(on_conflict_action,
                               'do_nothing' = 'DO NOTHING;',
                               'update_values' = paste0(paste('DO UPDATE SET',
                                                              update_cols),';'),
                               return(
                                 print(
                                   paste('Please specify the correct on_conflict_action parameter.',
                                         'Valid options include do_nothing and update_values')
                                 )
                               )
  )
  paste(sql_statement_head, sql_statement_tail)
}

dbUpsertMechanics <- function(.df, statement){
  dbExecute(conn, 'start transaction;')
  dbExecute(conn, 'drop table if exists temp_table')
  dbWriteTable(conn, 'temp_table', .df)
  dbExecute(conn, statement)
  dbExecute(conn, 'drop table if exists temp_table')
  dbExecute(conn, 'commit;')
}


dbUpsert <- function(conn,
                     table_name,
                     .df,
                     table_cols,
                     select_cols,
                     conflict_cols,
                     on_conflict_action = 'do_nothing',
                     update_cols){
  # Prepare the SQL statement for Upser, INSERT ON CONFLICT
  statement <- sql_upsert_statement(table_name,
                                    table_cols,
                                    select_cols,
                                    conflict_cols,
                                    on_conflict_action,
                                    update_cols)
  print(statement)
  # Initiate, load .df into temporary table, upsert ON CONFLICT,
  # remove temporary table and commit to the database.
  dbUpsertMechanics(.df, statement)
}

#'   collect
slice.tbl_sql <- function(.data, ...) {
  rows <- c(...)

  .data %>%
    mutate(...row_id = row_number()) %>%
    filter(...row_id %in% !!rows) %>%
    select(-...row_id)
}

# CDEC ----
cdec <- function(station, sensor, duration, start_date, end_date){
  Sys.sleep(1)
  url <- httr::parse_url('https://cdec.water.ca.gov/dynamicapp/req/JSONDataServlet')
  url$scheme <- 'http'
  url$query <- list(Stations = toupper(paste0(station, collapse = ',')),
                    SensorNums = paste0(sensor, collapse = ','),
                    dur_code = toupper(duration),
                    Start = start_date,
                    End = end_date)
  built_url <- build_url(url)
  print(built_url)

  df <- fromJSON(built_url) %>%
    as_tibble()
  df
}

# Water conversions ----
#  Add Water Year
add_wy <- function(df){
  df %>%
    mutate(date = ymd(date),
           month = month(date),
           year = year(date),
           water_year = ifelse(month %in% c(10:12), year + 1, year))
}
#tibble(date = sample(c(today()-1000:today()), 10)) %>% add_wy()

af_to_gal <- function(x){
  stopifnot(is.numeric(x))
  x * 325851.4286
}
#af_to_gal(1)

gal_to_af <- function(x){
  stopifnot(is.numeric(x))
  x / 325851.4286
}
#gal_to_af(325851.4286)

cfs_to_af <- function(x){
  stopifnot(is.numeric(x))
  x * 1.9834710990151
}
#cfs_to_af(350)

af_to_cfs <- function(x){
  stopifnot(is.numeric(x))
  x / 1.9834710990151
}
#af_to_cfs(694.2149)

ms3_to_af <- function(x){
  stopifnot(is.numeric(x))
  x * 70.0457
}
#ms3_to_af(10)

af_to_ms3 <- function(x){
  stopifnot(is.numeric(x))
  x / 70.0457
}

