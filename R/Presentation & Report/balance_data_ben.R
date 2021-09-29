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


# Create different balanced data sets
################################################################################
# Create under sampling
#filter df for only fatalities
fatalities <- df[df$FATAL_ACCIDENT == 1,]

#create a random sample of non Fatal so sample is balanced
non_fatal <- df[df$FATAL_ACCIDENT == 0,]
set.seed(1)
indx <- sample(nrow(non_fatal), 10312)
non_fatal <- non_fatal[indx, ]
#create balanced df
under_sampled_df <- rbind(fatalities, non_fatal)
# create training set, testing set and cross validation
train_test_list <- create_train_test_cross(under_sampled_df)
under_sample_train <- train_test_list[[1]]
under_sample_test <- train_test_list[[2]]
under_sample_cross <- train_test_list[[3]]

drops <- c("Road_User_Type_Desc","ACCIDENTTIME", "ACCIDENT_NO", "ACCIDENTDATE")
under_sample_train <- under_sample_train[ , !(names(under_sample_train) %in% drops)]
under_sample_test <- under_sample_test[ , !(names(under_sample_test) %in% drops)]
under_sample_cross <- under_sample_cross[ , !(names(under_sample_cross) %in% drops)]

write_csv(under_sample_train, here("data", "under_sample_train.csv"))
write_csv(under_sample_test, here("data", "under_sample_test.csv"))
write_csv(under_sample_cross, here("data", "under_sample_cross.csv"))
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
smote_test <- smote_train_test_list[[2]]
smote_cross <- smote_train_test_list[[3]]

################################################################################
# format smote df
################################################################################
drops <- c("Road_User_Type_Desc","ACCIDENTTIME", "ACCIDENT_NO", "ACCIDENTDATE")
smote_train <- smote_train[ , !(names(smote_train) %in% drops)]
smote_test <- smote_test[ , !(names(smote_test) %in% drops)]
smote_cross <- smote_cross[ , !(names(smote_cross) %in% drops)]

################################################################################
# Apply / Save SMOTE
################################################################################
smote_train$FATAL_ACCIDENT <- as.factor(smote_train$FATAL_ACCIDENT)
smote_test$FATAL_ACCIDENT <- as.factor(smote_test$FATAL_ACCIDENT)
smote_cross$FATAL_ACCIDENT <- as.factor(smote_cross$FATAL_ACCIDENT)

smote_train <- SMOTE(FATAL_ACCIDENT ~ ., smote_train, perc.over = 200)
smote_test <- SMOTE(FATAL_ACCIDENT ~ ., smote_test, perc.over = 200)
smote_cross <- SMOTE(FATAL_ACCIDENT ~ ., smote_cross, perc.over = 200)

write_csv(smote_train, here("data", "smote_train.csv"))
write_csv(smote_test, here("data", "smote_test.csv"))
write_csv(smote_cross, here("data", "smote_cross.csv"))