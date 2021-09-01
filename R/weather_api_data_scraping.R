# Libraries ----
library(httr)
library(tidyverse)
library(rjson)

# Load postcodes and variables ----
postcodes <- fromJSON(file = "sample.json") # json with list of postcodes
RAPIDAPI_KEY = 'd1d5ff8ef9msh03f3fb1acd367a2p14e523jsnf314364cf14f' # can vary with different account

# Create functions to use ----
request_by_postcode_and_year <- function(postcode, yearStart){
  base_url <- "https://visual-crossing-weather.p.rapidapi.com/"
  path <- "history"
  query_string <- list(
    startDateTime = sprintf('%s-01-01T00:00:00', yearStart),
    aggregateHours = '24',
    location = sprintf('%s,VIC,AUS', postcode),
    endDateTime = sprintf('%s-12-31T00:00:00', yearStart),
    contentType = 'csv',
    shortColumnNames = '0'
  )

  response <- GET(
    url = base_url,
    path = path,
    add_headers(
      'x-rapidapi-host' = 'visual-crossing-weather.p.rapidapi.com',
      'x-rapidapi-key' = RAPIDAPI_KEY
    ),
    query = query_string,
    content_type('application/octet-stream'))

  text <- content(response, "text")
  return(text);
}

gather_by_postcodes_and_year <- function(postcodes, year){
  final_df <- data.frame(matrix(ncol = 0, nrow = 0)) # initialize empty data frame
  for(postcode in postcodes){
    csv_response_text = request_by_postcode_and_year(postcode, year)
    df_from_response <- read_csv(csv_response_text)
    final_df <- bind_rows(final_df, df_from_response)
    cat("|") # some feedback on console
    Sys.sleep(1) # go easy on the api just in case
  }
  return(final_df)
}

create_csv <- function(df, file_name){
  if(!dir.exists("data")){
    dir.create("data")
  }

  write_csv(df, sprintf("data/%s", file_name))
}

create_csv_data_for_year <- function(year){
  combined_postcodes_df = gather_by_postcodes_and_year(postcodes, year)

  new_file_name = sprintf("sample_weather_%s.csv", year)
  create_csv(combined_postcodes_df, new_file_name)
}

# MAIN LOGIC STARTS HERE ----

# for(year in 2006:2020){
#   create_csv_data_for_year(year)
# }

create_csv_data_for_year(2020)

