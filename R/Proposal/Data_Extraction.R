# -----------------------------------
# Libraries
#------------------------------------

library(tidyverse)
library(foreign)
library(httr)
library(rjson)
library(here)
library(lubridate)

#------------------------------------------------
# Source weather and postcode data from API
#------------------------------------------------

# Source Post Code Data ---- 
nodes <- read_csv(here("data/ACCIDENT/NODE.csv"))
postcodes<-unique(nodes$POSTCODE_NO)
postcodesJson <- toJSON(postcodes, indent = 0, method = "C")
write(postcodesJson, "data/ACCIDENT/postcodes.json")


# Load postcodes  ----
postcodes <- fromJSON(
  file = "data/ACCIDENT/postcodes.json") # json with list of postcodes

# API Key ---
RAPIDAPI_KEY = 'd1d5ff8ef9msh03f3fb1acd367a2p14e523jsnf314364cf14f' 

# Create a function to be used to call API ----
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

# Send GET request to weather API ----
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

# Create function to combine API requests ----
gather_by_postcodes_and_year <- function(postcodes, year){
  final_df <- data.frame(
    matrix(ncol = 0, nrow = 0)) # initialize empty data frame
  for(postcode in postcodes){
    csv_response_text = request_by_postcode_and_year(postcode, year)
    df_from_response <- read_csv(csv_response_text)
    final_df <- bind_rows(final_df, df_from_response)
    cat("|") # some feedback on console
    Sys.sleep(1) # go easy on the api just in case
  }
  return(final_df)
}


# Create a function to create csv from df, data folder if it doesn't exist ----
create_csv <- function(df, file_name){
  if(!dir.exists("data")){
    dir.create("data")
  }
  
  write_csv(df, sprintf("data/%s", file_name))
}



# Create a function to extract data by postcode and year ----
create_csv_data_for_year <- function(year){
  combined_postcodes_df = gather_by_postcodes_and_year(postcodes, year)
  
  new_file_name = sprintf("sample_weather_%s.csv", year)
  create_csv(combined_postcodes_df, new_file_name)
}



# Extract weather API data by year and place into csv ----
if(!dir.exists("data/weather_data")){ # don't run to gather weather data 
                                      # if folder already exists
 for(year in 2006:2020){
    create_csv_data_for_year(year)
 }
}

#------------------------------------------------
# Consolidate and clean weather data
#------------------------------------------------

# Read consolidated API weather data place into one csv file ----
# setwd()
weather_data_path <- here("data", "weather_data")
setwd(weather_data_path)
WEATHER_DATA <- list.files(path = weather_data_path, pattern = '*.csv') %>%
                            map_df(~read_csv(.))

# Remove Spaces from heading names ----
names(WEATHER_DATA) <- gsub(' ', '', names(WEATHER_DATA))

# Split the Address into three ----
WEATHER_DATA <- WEATHER_DATA %>% separate(Address, c("Postcode", "State", "Country"), ",")

# Convert columns with numerical data to correct data type ----
WEATHER_DATA[,c(1,5:20)] <- sapply(WEATHER_DATA[,c(1,5:20)], as.numeric)


# Select relevant columns by index number ----
WEATHER_DATA <-  WEATHER_DATA[,c(1,4,7:9,11,15,18,19,21,27)]


# Clean and convert date column ----
WEATHER_DATA$Datetime <- str_extract(WEATHER_DATA$Datetime, "\\d+/\\d+/\\d+")
WEATHER_DATA$Datetime <- as.Date(WEATHER_DATA$Datetime, format =  "%m/%d/%Y")


# Weather API data is ready to be added to Car Accident data ----
WEATHER_DATA


#-----------------------------------------
# Source Car Accident Data from Vicroads
#-----------------------------------------

# Set where zip file will be saved locally ----
if(!dir.exists(here("data/ACCIDENT"))){
  dir.create(here("data/ACCIDENT"))
}
file_path <- here("data", "ACCIDENT")
setwd(file_path)

# Download and extract data  ----
url <- 
  'https://vicroadsopendatastorehouse.vicroads.vic.gov.au/opendata/Road_Safety/ACCIDENT.zip'
download.file(url, 'CarAccidentsData.zip')
unzip('CarAccidentsData.zip')


# Place selected files into variables  ----
f <- file.path(file_path, c("ACCIDENT.csv","ACCIDENT_LOCATION.csv", 
                            'NODE.csv','PERSON.csv',"ATMOSPHERIC_COND.csv", 
                            'ROAD_SURFACE_COND.csv','VEHICLE.csv'))

# Create names for the variables ----
names(f) <- gsub(".*/(.*)\\..*", "\\1", f)

# Read files into variables ready for analysis
for (i in 1:length(f)){
  x= read_csv(f[i])
  names(x)<- gsub(' ','_', names(x))
  assign(names(f[i]),x)
  remove(x)
}

#----------------------------------------------
# Combined All Data To Begin Analysis
#----------------------------------------------

# Combine Car Accident Data ----
BASE <-  left_join(PERSON, ACCIDENT, by='ACCIDENT_NO') %>% 
          left_join(x=., ROAD_SURFACE_COND, by='ACCIDENT_NO') %>% 
           left_join(x=., ACCIDENT_LOCATION, by='ACCIDENT_NO')  %>% 
            left_join(x=., NODE, by='ACCIDENT_NO') %>% 
              left_join(x=.,ATMOSPHERIC_COND,by='ACCIDENT_NO') %>% 
                left_join(x=., VEHICLE %>% 
                                  select(ACCIDENT_NO,VEHICLE_ID, 
                                         VEHICLE_YEAR_MANUF, 
                                         Road_Surface_Type_Desc, 
                                         VEHICLE_BODY_STYLE, 
                                         VEHICLE_MAKE, VEHICLE_MODEL, 
                                         NO_OF_CYLINDERS, 
                                         TOTAL_NO_OCCUPANTS), 
                          by= c('ACCIDENT_NO'='ACCIDENT_NO',
                                'VEHICLE_ID'='VEHICLE_ID' ))


# Clean and convert date column ----
BASE$ACCIDENTDATE <- str_extract(BASE$ACCIDENTDATE, "\\d+/\\d+/\\d+")
BASE$ACCIDENTDATE <- dmy(BASE$ACCIDENTDATE)

# Combine base data with API Weather data ----
glimpse(BASE)
# Note that POSTCODE_NO is the POSTCODE of where the accident is and POSTCODE is probably where the person lives
data <- left_join(BASE, WEATHER_DATA, by= c('POSTCODE_NO'='Postcode',
                                            'ACCIDENTDATE'='Datetime'))
glimpse(data)

# Clean up unused variables ----
remove(PERSON, ACCIDENT, ROAD_SURFACE_COND, ACCIDENT_LOCATION,NODE,
       ATMOSPHERIC_COND,VEHICLE, WEATHER_DATA, BASE)


# Reorder columns ----
data <- data[,c(1,18,19,22,23,2,4:9,12,34:39,21,25,42:44,72,
                74:76,15,50:53,62,73,46,71,77,78,31,32,79:87)]

# Create Target Variable ----
data<- data %>% 
        mutate(FATAL_ACCIDENT = if_else(NO_PERSONS_KILLED>0,"Y","N"), 
              FATAL_ACCIDENT = factor(FATAL_ACCIDENT, levels = c("Y", "N"))) %>% 
        relocate(FATAL_ACCIDENT, .after = ACCIDENT_NO )


# data is ready for EDA -----
data 

# write a csv file for data storage
write_csv(data, here('data', "Car_Accident_Data.csv"))


