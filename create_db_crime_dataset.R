library(data.table)
library(tidyverse)
library(lubridate)



setwd('~/Desktop/Delray Beach Police Department/')

df <- fread('raw_data/db_crime.csv') %>%
  as_tibble()

cols_to_keep <- c(
  'incident_datetime',
  'address_1',
  'city',
  'state',
  'zip',
  'latitude',
  'longitude',
  'hour_of_day',
  'day_of_week',
  'parent_incident_type'
)

df <- select(df, one_of(cols_to_keep))

meta_crime_cats <- c(
  'Property',
  'Violence',
  'Violence',
  'Theft',
  'Other',
  'Other',
  'Impaired',
  'Impaired',
  'Other',
  'Other',
  'Property',
  'Violence',
  'Violence',
  'Impaired',
  'Other',
  'Other',
  'Violence',
  'Other',
  'Other',
  'Property',
  'Other',
  'Theft',
  'Violence',
  'Violence',
  'Theft',
  'Theft',
  'Theft',
  'Other',
  'Other',
  'Other',
  'Violence'
)


crime_table <-
  tibble(parent_incident_type = sort(unique(df$parent_incident_type)),
         crime_cat = meta_crime_cats)


dataset <- left_join(df,crime_table)
date_features <- function(df, dt_col) {
  
  dt_col <- enquo(dt_col)
  df <- mutate(
    df,
    day = day(!!dt_col),
    day_week = wday(!!dt_col, label = TRUE),
    week = week(!!dt_col),
    month = month(!!dt_col, label = TRUE),
    year = year(!!dt_col),
    hour = hour(!!dt_col),
    minute = minute(!!dt_col)
  )
  df
}

dataset <- date_features(dataset, mdy_hms(incident_datetime))

# quantile(dataset$longitude, seq(0,1,.1),na.rm = TRUE)
# quantile(dataset$latitude, seq(0,1,.1),na.rm = TRUE)


min_lon <- -80.12
max_lon <- -80.07

min_lat <- 26.42
max_lat <- 26.48

dataset <- dataset %>% 
  filter(between(longitude,min_lon,max_lon),
         between(latitude,min_lat,max_lat))

dataset <- dataset %>% 
  mutate(day_segment = case_when(
    between(hour, 0, 5) ~ 'Overnight',
    between(hour, 6, 11) ~ 'Early Day',
    between(hour, 12, 17) ~ 'Afternoon',
    TRUE ~ 'Night'
  ))


dataset <- dataset %>% 
  mutate(
    season = case_when(
      month %in% c('Dec','Jan','Feb') ~ 'Winter',
      month %in% c('Mar','Apr','May') ~ 'Spring',
      month %in% c('Jun','Jul','Aug') ~ 'Summer',
      TRUE ~ 'Fall'
    )
  )

crime_cols <-
  c('crime_cat',
    'day',
    'day_week',
    'season',
    'day_segment',
    'latitude',
    'longitude')


crime_df <- select(dataset, crime_cols)

crime_grp <- crime_df %>% 
  group_by(crime_cat, day_week, season, day_segment, latitude, longitude) %>% 
  count(sort=TRUE) %>% 
  ungroup()

crime_grp %>% 
  write_csv('processed_data/db_crime_data_by_dow.csv')


rm(list = ls())
