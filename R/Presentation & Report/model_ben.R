library(MASS)
library(tidyverse)
library(caret)
library(here)

################################################################################
# Read in data
################################################################################
smote_train <- read_csv(here("data", "smote_train.csv"))
smote_test <- read_csv(here("data", "smote_test.csv"))
under_sample_train <- read_csv(here("data", "under_sample_train.csv"))
under_sample_test <- read_csv(here("data", "under_sample_test.csv"))
################################################################################
# Model function starts here
################################################################################
create_model <- function(train_df, test_df, lift_threshold) {
# Fit/Train logistic regression model\
   print("Fitting model")
   model <- glm(FATAL_ACCIDENT ~., family = binomial, data = train_df)
   print("Performing forward Selection")
   model <- stepAIC(model, direction="forward")
   print("Forward Selection complete")
   print(summary(model))
# Test Model / Create Predictions
   print("Testing Model")
   pred <- predict(model, test_df, type = "response")
   pred <- as.data.frame(pred)
   pred <- mutate(pred, pred = ifelse(pred >= lift_threshold, 1,
                         ifelse(pred < lift_threshold, 0, NA)))
   print("Compiling confusion matrix")
# Create Confusion Matrix
   pred_y <- as.numeric(pred > 0)
   true_y <- as.numeric(test_df$FATAL_ACCIDENT)
   true_pos <- (true_y==1) & (pred_y==1)
   true_neg <- (true_y==0) & (pred_y==0)
   false_pos <- (true_y==0) & (pred_y==1)
   false_neg <- (true_y==1) & (pred_y==0)
   conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                        sum(false_neg), sum(true_neg)), 2, 2)
   colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
   rownames(conf_mat) <- c('Y = 1', 'Y = 0')
   print(conf_mat)
   print("###### Model finished here ######")
}

################################################################################
# Parse various data to model function here
################################################################################
create_model(smote_train, smote_test, 0.5)
create_model(under_sample_train, under_sample_test, 0.50)






#pred <- exp(pred)/(1+exp(pred))
# pred <- as.factor(pred)
# test <- as.factor(smote_test$FATAL_ACCIDENT)
# confusionMatrix(pred, as.factor(smote_test$FATAL_ACCIDENT))
# residuals <- residuals(model)



# Convert Dummys to factors
# dummy_cols <- c("FATAL_ACCIDENT",
#                 "SEXF",
#                 "SEXM",
#                 "Accident_Type_DescCollision.with.a.fixed.object",
#                 "Accident_Type_DescStruck.animal",
#                 "Accident_Type_DescStruck.Pedestrian",
#                 "Accident_Type_DescVehicle.overturned..no.collision.",
#                 "Road_Surface_Type_DescUnpaved",
#                 "Surface_Cond_DescDry",
#                 "Surface_Cond_DescIcy",
#                 "Surface_Cond_DescMuddy",
#                 "Surface_Cond_DescSnowy",
#                 "Surface_Cond_DescWet",
#                 "Atmosph_Cond_DescClear",
#                 "Atmosph_Cond_DescFog",
#                 "Atmosph_Cond_DescRaining",
#                 "Atmosph_Cond_DescSmoke",
#                 "Atmosph_Cond_DescStrong.winds",
#                 "Light_Condition_DescDark.No.street.lights",
#                 "Light_Condition_DescDark.Street.lights.off",
#                 "ConditionsOvercast",
#                 "ConditionsRain",
#                 "ConditionsRain..Overcast",
#                 "Age_Group16.17",
#                 "Age_Group17.21",
#                 "Age_Group70.",
#                 "Day_Week_DescriptionSaturday",
#                 "Day_Week_DescriptionSunday",
#                 "NO_OF_CYLINDERS_4",
#                 "NO_OF_CYLINDERS_6",
#                 "NO_OF_CYLINDERS_8",
#                 "NO_OF_CYLINDERS_12")
# 
# dummy_cols <- c('FATAL_ACCIDENT', 'NO_OF_CYLINDERS_8')
# smote_train[dummy_cols] <- lapply(smote_train[dummy_cols], factor)
# smote_test[dummy_cols] <- lapply(smote_test[dummy_cols], factor)
# smote_train <- smote_train[dummy_cols]
# smote_test <- smote_test[dummy_cols]