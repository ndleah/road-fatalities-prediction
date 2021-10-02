library(tidyverse)
library(here)
library(caret)
library(dplyr)
library(mltools)
library(VIM)
library(summarytools)
library(moments)
library(outliers)
library(DataExplorer)

df <- read_csv(here('data', 'car_accident.csv'))

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

all_features_cols <- c(cat_cols)
################################################################################
# Handle missing values-----
################################################################################
plot_missing(df)

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
glimpse(df)
for(name in names(log_df)){
  column_vector <- pull(log_df, name)
  column_vector <- as.numeric(column_vector)
  skew <- skewness(column_vector)
  if (skew < - 0.5){
    constant <- max(column_vector) + 1
    new_vals <- constant - column_vector
    new_vals <- log(new_vals)
    df[name] = as.numeric(new_vals)
    hist(new_vals)
  } else if (skew > 0.5){
    constant <- 1
    new_vals <- log((column_vector + constant))
    new_vals <- as.numeric(new_vals)
    df[name] = new_vals
  }
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
# Outlier
################################################################################
# box plot
boxplot(df$AGE, main="AGE") # Speed Zone
#multivariate box plot
boxplot(df$AGE~df$Age_Group)
# calculate z-score
mean(df$AGE)
#calculate z score
z.scores <- df$AGE %>% na.omit %>% scores(type = "z")
z.scores %>% summary()

# Finds the total number of outliers according to the z-score
length (which( abs(z.scores) >3 ))

################################################################################
# Standardize/ scale numeric values
################################################################################

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

