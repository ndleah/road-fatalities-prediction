library(tidyverse)
library(here)
library(caret)
library(dplyr)
library(mltools)
library(VIM)
library(summarytools)
library(DMwR)

################################################################################
# Read in data
df <- read.csv(here("data", "Car_Accident_Data_No_Na.csv"))

################################################################################
# Create train, test and cross validation df function
################################################################################
create_train_test_cross <- function(df){
  train_df <- df[df$ACCIDENTDATE < 	"2017-01-01",]
  test_df <- df[(df$ACCIDENTDATE > "2017-01-01" & df$ACCIDENTDATE < "2020-01-01"),]
  cross_valid_df <- df[(df$ACCIDENTDATE > "2020-01-01" ),]
  return (list(train_df, test_df, cross_valid_df))
}
################################################################################
################################################################################
# Create different balanced data sets
################################################################################
################################################################################
# Create train and test for no transformation techniques
################################################################################
no_change_list <- create_train_test_cross(df)
no_change_train <- no_change_list[[1]]
no_change_test <- no_change_list[[2]]
no_change_cross <- no_change_list[[3]]

drops <- c("Road_User_Type_Desc","ACCIDENTTIME", "ACCIDENT_NO", "ACCIDENTDATE")
no_change_train <- no_change_train[ , !(names(no_change_train) %in% drops)]
no_change_test <- no_change_test[ , !(names(no_change_test) %in% drops)]
no_change_cross <- no_change_cross[ , !(names(no_change_cross) %in% drops)]

write_csv(no_change_train, here("data", "no_change_train.csv"))
write_csv(no_change_test, here("data", "test_set_generic.csv"))
write_csv(no_change_cross, here("data", "cross_set_generic.csv"))



################################################################################
# Create under sampling
################################################################################

# create training set, testing set and cross validation
train_test_list <- create_train_test_cross(df)
under_sample_train <- train_test_list[[1]]

#filter df for only fatalities
fatalities <- under_sample_train[under_sample_train$FATAL_ACCIDENT == 1,]
#create a random sample of non Fatal so sample is balanced
non_fatal <- under_sample_train[under_sample_train$FATAL_ACCIDENT == 0,]
set.seed(1)
indx <- sample(nrow(non_fatal), 3919)
non_fatal <- non_fatal[indx, ]
#create balanced df
under_sample_train <- rbind(fatalities, non_fatal)
drops <- c("Road_User_Type_Desc","ACCIDENTTIME", "ACCIDENT_NO", "ACCIDENTDATE")
under_sample_train <- under_sample_train[ , !(names(under_sample_train) %in% drops)]
write_csv(under_sample_train, here("data", "under_sample_train.csv"))

################################################################################
# Perform SMOTE
################################################################################
# Make dummy variables factors
df <- df %>%
  mutate_if(is.integer, as.factor)

################################################################################
# create training set, testing set and cross validation
smote_train_test_list <- create_train_test_cross(df)
smote_train <- smote_train_test_list[[1]]

################################################################################
# format smote df
################################################################################
drops <- c("Road_User_Type_Desc","ACCIDENTTIME", "ACCIDENT_NO", "ACCIDENTDATE")
smote_train <- smote_train[ , !(names(smote_train) %in% drops)]

################################################################################
# Apply / Save SMOTE
################################################################################
smote_train$FATAL_ACCIDENT <- as.factor(smote_train$FATAL_ACCIDENT)
smote_train <- SMOTE(FATAL_ACCIDENT ~ ., smote_train, perc.over = 200)
write_csv(smote_train, here("data", "smote_train.csv"))
