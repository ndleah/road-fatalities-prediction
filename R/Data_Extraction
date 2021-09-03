library(tidyverse)
library(foreign)

#-------------------------------------------------------------------------------------------------
# Source Car Accident Data 
#-------------------------------------------------------------------------------------------------

# Set where zip file is to be saved
file_path <- 'XXXXXXXX'
setwd(file_path)

# Download and extract data
url <- 'https://vicroadsopendatastorehouse.vicroads.vic.gov.au/opendata/Road_Safety/ACCIDENT.zip'
download.file(url, 'CarAccidentsData.zip')
unzip('CarAccidentsData.zip')
  
# Place selected files into variables
f <- file.path(file_path, 
           c("ACCIDENT.csv","ACCIDENT_EVENT.csv", "ACCIDENT_LOCATION.csv", "ATMOSPHERIC_COND.csv",
             'NODE.csv','NODE_ID_COMPLEX_INT_ID.csv','PERSON.csv', 'ROAD_SURFACE_COND.csv',
             'SUBDCA.csv','VEHICLE.csv'))


# Create names for the variables and remove any characters after '.' 
names(f) <- gsub(".*/(.*)\\..*", "\\1", f)

# Read files into variables ready for analysis
for (i in 1:length(f)){
    x= read_csv(f[i])
    names(x)<- gsub(' ','_', names(x))
    assign(names(f[i]),x)
    remove(x)
  }


#-------------------------------------------------------------------------------------------------
# Combined Data to begin Analysis
#-------------------------------------------------------------------------------------------------

# Base Table
PERSON <- PERSON %>% 
          select(-LICENCE_STATE, -PEDEST_MOVEMENT, -POSTCODE, -TAKEN_HOSPITAL, -EJECTED_CODE)

ACCIDENT <- ACCIDENT %>%  
              select(-DIRECTORY, -EDITION, -PAGE, -GRID_REFERENCE_X, 
                     -GRID_REFERENCE_Y, -POLICE_ATTEND, -ROAD_GEOMETRY)
BASE <- left_join(PERSON, ACCIDENT, by='ACCIDENT_NO') %>% 
          left_join(x=., ROAD_SURFACE_COND, by='ACCIDENT_NO')

# Location
LOCATION <- left_join(NODE,ACCIDENT_LOCATION %>% 
                        select(ACCIDENT_NO,NODE_ID, ROAD_NAME, ROAD_TYPE, ROAD_TYPE_INT),
                      by='ACCIDENT_NO' ) 
# Weather


# Final Dataset for Analysis
data <- left_join(BASE, LOCATION,by='ACCIDENT_NO') %>% 
          left_join(x=.,ATMOSPHERIC_COND,by='ACCIDENT_NO')


data
# remove(PERSON, BASE, ACCIDENT_LOCATION, NODE,LOCATION, ROAD_SURFACE_COND,ACCIDENT)


#-------------------------------------------------------------------------------------------------
# EDA
#-------------------------------------------------------------------------------------------------
