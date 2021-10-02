library(MASS)
library(tidyverse)
library(caret)
library(here)

################################################################################
# Read in data
################################################################################
no_change_train <-read_csv(here("data", "no_change_train.csv"))
test_generic <- read_csv(here("data", "test_set_generic.csv"))
smote_train <- read_csv(here("data", "smote_train.csv"))
under_sample_train <- read_csv(here("data", "under_sample_train.csv"))

################################################################################
# Model function starts here
################################################################################
create_model <- function(train_df, test_df, lift_threshold) {
# Fit/Train logistic regression model\
   print("Fitting model")
   #model <- glm(FATAL_ACCIDENT ~., family = binomial, data = train_df)
   print("Performing forward Selection")
   # Create base with target only
   fit_start <- glm(FATAL_ACCIDENT ~ 1, family = binomial, data = train_df)
   # Create fit all
   fit_all <- glm(FATAL_ACCIDENT ~., family = binomial, data = train_df)
   # Perform step forward
   model <- stepAIC(fit_start, direction="forward", scope = formula(fit_all))
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
   # colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
   # rownames(conf_mat) <- c('Y = 1', 'Y = 0')
   print(conf_mat)
   # calculate accuracy
   accuracy <- ((conf_mat[1,1] + conf_mat[2, 2]) / (sum(conf_mat[1,]) + sum(conf_mat[2,])))
   # calculate precision
   precision <- conf_mat[1, 1] / sum(conf_mat[,1])
   # calculate recall
   recall <- conf_mat[1, 1] / sum(conf_mat[1,])
   # calculate specificity
   specificity <- conf_mat[2, 2] / sum(conf_mat[2,])
   f1_score <- ((2*precision*recall)/(precision + recall))
   print(" ")

   print(paste0("Accuracy:", accuracy))
   print(paste0("Precision: ", precision))
   print(paste0("recall: ", recall))
   print(paste0("specificity: ", specificity))
   print(paste0("F1 Score: ", f1_score))
   
   # Create ROC curve
   idx <- order(-pred)
   recall <- cumsum(true_y[idx] == 1) / sum(true_y == 1)
   specificity <- (sum(true_y == 0) - cumsum(true_y[idx] == 0)) / sum(true_y == 0)
   roc_df <- data.frame(recall = recall, specificity = specificity)
   roc <- ggplot(roc_df, aes(x=specificity, y=recall)) +
      geom_line(color='blue') +
      scale_x_reverse(expand=c(0, 0)) +
      scale_y_continuous(expand=c(0, 0)) +
      geom_line(data=data.frame(x=(0:100) / 100), aes(x=x, y=1-x),
                linetype='dotted', color='red')
   
   print(roc)
   
   auc <- sum(roc_df$recall[-1] * diff(1 - roc_df$specificity))
   print(paste0("AUC: ", auc))

   print("###### Model finished here ######")
 }

################################################################################
# Parse various data to model function here
################################################################################
# https://www.sciencedirect.com/science/article/pii/S1556086415306043
#https://medium.com/black-feathers-labs/why-precision-recall-why-not-accuracy-83349aa4c829

#create_model(no_change_train, test_generic, 0.5)
#create_model(smote_train, test_generic, 0.5)
#create_model(under_sample_train, test_generic, 0.46)






