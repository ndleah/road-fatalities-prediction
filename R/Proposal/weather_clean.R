library(tidyverse)
library(here)


# get column names
df <- read_csv(here('data', 'weather_data', 'weather_2006.csv'))
col_names <- colnames(df)

# define new column names
added_cols <- c("Postcode", "State", "Country")
final_cols <- paste0(added_cols, col_names)

# create vector of years in dataset
years <- c('2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013',
           '2014', '2015', '2016', '2017', '2018', '2019', '2020A', '2020B')

# create appending_df
final_weather <- data.frame(matrix(nrow = 0, ncol = 27))
# apply column names
names(final_weather) <- final_cols

# loop over years and split postcode column and concat to 1 df
for (year in as.list(years)){
  csv_file <- sprintf('weather_%s.csv', year)
  
  # Read csv
  weather <- read_csv(here('data', 'weather_data', csv_file))
  # split column
  weather <- weather %>%
    separate(Address, c("Postcode", "State", "Country"), sep = ',')
  
  final_weather <- rbind(final_weather, weather)
}


write_csv(final_weather, here('data', 'weather_data', "clean_weather.csv"))
          