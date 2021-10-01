library(tidyverse)
library(here)
library(caret)
library(dplyr)
library(mltools)
library(VIM)
library(summarytools)

df <- read_csv(here('data', 'Car_Accident_Data.csv'))

################################################################################
# Filter and mutate data for basic analysis ----
################################################################################
# Convert fatality to numerical
df <- df %>%
  mutate(FATAL_ACCIDENT =  case_when(
    FATAL_ACCIDENT == 'Y' ~ 1,
    FATAL_ACCIDENT == 'N' ~ 0)
  )

# convert speed zone to numeric
df <- transform(df, SPEED_ZONE = as.numeric(SPEED_ZONE))

# Filter df - filter to driver based data and remove outliers
# drop values where speed zone has incorrect data e.g. > 200km/hr
df <- df[df$SPEED_ZONE < 200, ]
df <- df[df$NO_OF_CYLINDERS < 25, ]

# Convert no_of_cylinders to factor
df$NO_OF_CYLINDERS <- as.factor(df$NO_OF_CYLINDERS)

target <- c('Drivers', 'Motorcyclists')
df <- df %>%
  filter(Road_User_Type_Desc %in% target)

dfSummary(df)
################################################################################
# Define numerical and categorical columns ----
################################################################################

numerical_cols <- c('NO_OF_VEHICLES', 'NO_PERSONS','SPEED_ZONE',
                    'VEHICLE_YEAR_MANUF', 'TOTAL_NO_OCCUPANTS', 
                    'LIGHT_CONDITION', 'CloudCover', 'WindSpeed', 
                    'Temperature', 'DewPoint', 'RelativeHumidity',
                    'Precipitation')


cat_cols <- c('SEX', 'SEATING_POSITION', 'Accident_Type_Desc',
              'Road_Surface_Type_Desc', 'Surface_Cond_Desc', 
              'Atmosph_Cond_Desc', 'Light_Condition_Desc','Conditions',
              'Age_Group', 'Day_Week_Description', 'NO_OF_CYLINDERS')

indexing_cols <- c('ACCIDENT_NO', 'FATAL_ACCIDENT', 'ACCIDENTDATE',
                   'ACCIDENTTIME' , 'Road_User_Type_Desc')

log_cols <- c('Precipitation', 'CloudCover', 'RelativeHumidity', 'NO_PERSONS',
              'TOTAL_NO_OCCUPANTS', 'NO_OF_VEHICLES', 'SPEED_ZONE',
              'LIGHT_CONDITION')

all_features_cols <- c(numerical_cols, cat_cols)
################################################################################
# Handle missing values-----
################################################################################
################################################################################
# Replace missing values at same frequency they appear in column
################################################################################
# which(myV>7)[1]
idx_greater_than <- function(value, list){
#Find the first index of vector 'list' that has a corresponding value greater 
#than 'value'
  
  for(i in 1:length(list)){
    if(list[i] > value){
      return(i)
    }
  }
}

# MAIN LOGIC STARTS HERE ----
replace_nan_df <- df[all_features_cols]

for(name in names(replace_nan_df)){
  column_vector <- pull(replace_nan_df, name)
  
  # Get index of nans
  nan_idxs <- which(is.na(column_vector))
  
  # If no nans, don't worry
  if(length(nan_idxs)==0){
    next
  }
  
  srs_notnull <- column_vector[!is.na(column_vector)]
  
  # Get unique labels and counts for the non-nan features
  unique_frequency_df <- as.data.frame(table(srs_notnull))
  labels <- as.character(unique_frequency_df$srs_notnull)
  counts <- unique_frequency_df$Freq
  cum_counts <- cumsum(counts)
  
  # Generate random numbers of size len(nan_idxs)
  set.seed(1)
  rand_vals <- floor(runif(length(nan_idxs), min=0, max=length(srs_notnull)))
  
  new_vals <- c()
  for(x in rand_vals){
    #Find out the largest number in cum_counts that each rand_val is less than
    larger_value_index <- idx_greater_than(x, cum_counts)
    # Get values corresponding to above index
    new_vals <- append(new_vals, labels[larger_value_index])
  }
  # Update the df with the new vals
  df[nan_idxs, name] = new_vals
}

################################################################################
# NEED TO PERFORM LOG FUNCTION ON SKEWED NUMERICAL DATA HERE
################################################################################
log_df <- df[log_cols]

for(name in names(log_df)){
  column_vector <- pull(log_df, name)
  column_vector <- as.numeric(column_vector)
  new_vals <- log(column_vector + 1)
  # Update the df with the new vals
  df[name] = as.numeric(new_vals)
}

################################################################################
# Transform Categorical Data Based on EDA ----
################################################################################
#Use dummyVars function to create binary variables.
category_df <- df[cat_cols]
variables <- dummyVars("~.", data = category_df, sep = "_")

category_df <- data.frame(predict(variables, newdata = category_df))

category_df <- category_df %>%
     mutate_if(is.double, as.factor)

base_df <- df[indexing_cols]
numerical_df <- df[numerical_cols]

################################################################################
# Standardize/ scale numeric values
################################################################################
numerical_df <- numerical_df %>%
  mutate_if(is.character, as.numeric)
numerical_df <- as.data.frame(scale(numerical_df))





final_df <- cbind(base_df, category_df)
final_df <- cbind(final_df, numerical_df)

################################################################################
# Create features to keep in df based on EDA and domain knowledge
################################################################################
final_cols <- c("ACCIDENT_NO","FATAL_ACCIDENT",
                "ACCIDENTDATE","ACCIDENTTIME",
                "Road_User_Type_Desc","SEXF",
                "SEXM",
                "Accident_Type_DescCollision.with.a.fixed.object",
                "Accident_Type_DescStruck.animal",
                "Accident_Type_DescStruck.Pedestrian",
                "Accident_Type_DescVehicle.overturned..no.collision.",
                "Road_Surface_Type_DescUnpaved",
                "Surface_Cond_DescDry",
                "Surface_Cond_DescIcy",
                "Surface_Cond_DescMuddy",
                "Surface_Cond_DescSnowy",
                "Surface_Cond_DescWet",
                "Atmosph_Cond_DescClear",
                "Atmosph_Cond_DescFog",
                "Atmosph_Cond_DescRaining",
                "Atmosph_Cond_DescSmoke",
                "Atmosph_Cond_DescStrong.winds",
                "Light_Condition_DescDark.No.street.lights",
                "Light_Condition_DescDark.Street.lights.off",
                "ConditionsOvercast",
                "ConditionsRain",
                "ConditionsRain..Overcast",
                "Age_Group16.17",
                "Age_Group17.21",
                "Age_Group70.",
                "Day_Week_DescriptionSaturday",
                "Day_Week_DescriptionSunday",
                "NO_OF_CYLINDERS_4",
                "NO_OF_CYLINDERS_6",
                "NO_OF_CYLINDERS_8",
                "NO_OF_CYLINDERS_12",
                "NO_OF_VEHICLES",
                "NO_PERSONS",
                "SPEED_ZONE",
                "VEHICLE_YEAR_MANUF",
                "TOTAL_NO_OCCUPANTS",
                "LIGHT_CONDITION",
                "CloudCover",
                "WindSpeed",
                "Temperature",
                "DewPoint",
                "RelativeHumidity",
                "Precipitation")
                

final_df <- final_df[final_cols]
write_csv(final_df, here("data", "Car_Accident_Data_No_Na.csv"))

################################################################################
#final_df[numerical_cols] <- sapply(final_df[numerical_cols], as.numeric)

# 
# 
# 
# write_csv(final_df, here("data", "Car_Accident_Data_No_Na.csv"))
 
################################################################################
# Normalize 

################################################################################

# Use KNN to replace all missing values
# 
# df_no_na <- kNN(df, k = 3)
# # Save output to csv
# write_csv(final_df, here("data", "Car_Accident_Data_No_Na.csv"))

################################################################################
#Replace missing categorical with Amelia
# id var = data you dont want missing algo
#noms is all categorical data
# idvars = c("ACCIDENT_NO", "FATAL_ACCIDENT", "ACCIDENTDATE",
#            "ACCIDENTTIME")
# 
# noms = c("Day_Week_Description", "PERSON_ID",
#          "SEX", "Age_Group", "Inj_Level_Desc",
#          "SEATING_POSITION", "Road_User_Type_Desc",
#          "Accident_Type_Desc", "DCA_Description",
#          "Road_Geometry_Desc", "VEHICLE_BODY_STYLE",
#          "VEHICLE_MAKE", "VEHICLE_MODEL", "ROAD_NAME",
#          "ROAD_TYPE", "ROAD_NAME_INT", "ROAD_TYPE_INT",
#          "LGA_NAME", "Road_Surface_Type_Desc",
#          "Surface_Cond_Desc", "Atmosph_Cond_Desc",
#          "Light_Condition_Desc", "WeatherType",
#          "Conditions")

# a.out = amelia(x = df, m = 5,
#                idvars = , ts = NULL,
#                cs = NULL, priors = NULL, lags = NULL, empri = 0,
#                intercs = FALSE, leads = NULL, splinetime = NULL, logs = NULL,
#                sqrts = NULL, lgstc = NULL, ords = NULL, noms = noms,
#                bounds = NULL, max.resample = 1000, tolerance = 1e-04)
# 
# write.amelia(obj = a.out, file.stem = "outdata")
# 





################################################################################
# Replace missing categorical with mode
################################################################################
# non numeric index
# idx <- !sapply(df, is.numeric)
# # mode function
# Mode <- function(x) { 
#                       ux <- sort(unique(x))
#                       ux[which.max(tabulate(match(x, ux)))] 
# }
# # replace na in char columns
# df[idx] <- lapply(df[idx], function(x)
#             replace(x, is.na(x), Mode(x[!is.na(x)])))

################################################################################
# category_df %>%
#   gather() %>%
#   ggplot(aes(value)) +
#   facet_wrap(~ key, scales = "free") +
#   geom_density(color = "blue", fill = "red") +
#   coord_flip()

################################################################################
# Replace missing numerical data with mean ----
################################################################################
# replace na with mean
# df <- data.frame(sapply(df,function(x) ifelse(is.na(x), mean(x, na.rm = TRUE),x)))

# 
# 
# final_cols <- c("ACCIDENT_NO", "FATAL_ACCIDENT", "ACCIDENTDATE", "ACCIDENTTIME",
#                 "Road_User_Type_Desc", "SEXM", "Road_Surface_Type_DescPaved",
#                 "Road_Surface_Type_DescUnpaved",
#                 "Surface_Cond_DescIcy", "Surface_Cond_DescMuddy",
#                 "Surface_Cond_DescSnowy",
#                 "Surface_Cond_DescWet", "Atmosph_Cond_DescDust",
#                 "Atmosph_Cond_DescFog", "Atmosph_Cond_DescRaining",
#                 "Atmosph_Cond_DescSmoke",
#                 "Atmosph_Cond_DescSnowing","Atmosph_Cond_DescStrong.winds",
#                 "Light_Condition_DescDark.Street.lights.off",
#                 "Light_Condition_DescDark.Street.lights.on",
#                 "Light_Condition_DescDark.Street.lights.unknown",
#                 "Light_Condition_DescDay", "Light_Condition_DescDusk.Dawn",
#                 "ConditionsOvercast" ,
#                 "ConditionsPartially.cloudy", "ConditionsRain",
#                 "ConditionsRain..Overcast", "ConditionsRain..Partially.cloudy",
#                 "ConditionsUnknown", "Age_Group13.15", "Age_Group16.17",
#                 "Age_Group17.21", "Age_Group22.25", "Age_Group26.29",
#                 "Age_Group30.39", "Age_Group40.49", "Age_Group5.12",
#                 "Age_Group50.59", "Age_Group60.64",
#                 "Age_Group64.69", "Age_Group70.",
#                 "Day_Week_DescriptionMonday",
#                 "Day_Week_DescriptionSaturday", "Day_Week_DescriptionSunday",
#                 "Day_Week_DescriptionThursday" ,"Day_Week_DescriptionTuesday",
#                 "Day_Week_DescriptionWednesday", "NO_OF_CYLINDERS1",
#                 "NO_OF_CYLINDERS10", "NO_OF_CYLINDERS11",
#                 "NO_OF_CYLINDERS12",  "NO_OF_CYLINDERS14",
#                 "NO_OF_CYLINDERS19",  "NO_OF_CYLINDERS2",
#                 "NO_OF_CYLINDERS3",   "NO_OF_CYLINDERS4",
#                 "NO_OF_CYLINDERS40",   "NO_OF_CYLINDERS44",
#                 "NO_OF_CYLINDERS46", "NO_OF_CYLINDERS5",
#                 "NO_OF_CYLINDERS6",  "NO_OF_CYLINDERS60",
#                 "NO_OF_CYLINDERS63",   "NO_OF_CYLINDERS64",
#                 "NO_OF_CYLINDERS65", "NO_OF_CYLINDERS66",
#                 "NO_OF_CYLINDERS7",   "NO_OF_CYLINDERS8",
#                 "NO_OF_CYLINDERSUnknown",  "NO_OF_VEHICLES",
#                 "NO_PERSONS", "SPEED_ZONE",
#                 "VEHICLE_YEAR_MANUF",  "TOTAL_NO_OCCUPANTS",
#                 "LIGHT_CONDITION","CloudCover",
#                 "WindSpeed",  "Temperature",
#                 "DewPoint", "RelativeHumidity",
#                 "Precipitation")
