library(dplyr)
library(esquisse)
library(pROC)

df <- read.csv("~/Heart_disease_prediction_project/cardio_data_processed.csv")

tibble::view(df)

# translate blood pressure categories to numbers (0 to 4)
# and body mass index to numbers (0 to 5)

df <- df |> 
  dplyr::mutate(bp_cat = dplyr::case_when(
    bp_category == "Normal" ~ 0,
    bp_category == "Elevated" ~ 1,
    bp_category == "Hypertension Stage 1" ~ 2,
    bp_category == "Hypertension Stage 2" ~ 3,
    bp_category == "Hypertensive Crisis" ~ 4
  )) |> 
  dplyr::mutate(bmi_cat = dplyr::case_when(
    bmi <= 18.5 ~ 0,    # underweight
    bmi < 25 ~ 1,    # normal weight
    bmi < 30 ~ 2,    # overweight
    bmi < 35 ~ 3,    # class 1 obesity
    bmi < 40  ~ 4,    # class 2 obesity 
    bmi >= 40 ~ 5       # class 3 obesity
  )) |> 
  tibble::view()


# turning the categorical values into REAL categorical values
df_cat <- df |> 
  dplyr::mutate(gender = as.factor(gender),
                cholesterol = as.factor(cholesterol),
                gluc = as.factor(gluc),
                smoke = as.factor(smoke),
                alco = as.factor(alco),
                active = as.factor(active),
                cardio = as.factor(cardio),
                bp_cat = as.factor(bp_cat),
                bmi_cat = as.factor(bmi_cat)) |> 
  tibble::view()


# exclude columns that are unnecessary
df_cat |> 
  dplyr::select(-age, -bp_category_encoded) |> 
  tibble::view()


esquisse::esquisser(df_cat)


# fit an initial logistic regression model
cardio_log_reg_model <- glm(cardio ~ ., data = df_cat, family = "binomial")

# perform stepwise variable selection
stepwise_cardio_log_reg <- step(cardio_log_reg_model, direction = "both")

# print the selected model
summary(stepwise_cardio_log_reg)

# predicting responses using logistic regression model
predictions <- predict(stepwise_cardio_log_reg, newdata = df_cat, type = "response")

# choosing a threshold
threshold <- 0.4437
predicted_classes <- ifelse(predictions > threshold, 1, 0)

# assuming 'actual_class' is the vector of actual classes
confusion_matrix <- table(Actual = df_cat$cardio, Predicted = predicted_classes)
print(confusion_matrix)

# calculate performance metrics
sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

cat("Confusion Matrix:\n")
print(confusion_matrix)

cat("\nPerformance Metrics:\n")
cat("Sensitivity (True Positive Rate): ", sensitivity, "\n")
cat("Specificity (True Negative Rate): ", specificity, "\n")
cat("Precision (Positive Predictive Value): ", precision, "\n")
cat("Accuracy: ", accuracy, "\n")


# finding the best threshold and accuracy for this model
# start from 0.1 and increment by 0.05 until 0.95
# initialize variables to store the maximum accuracy and corresponding threshold
max_accuracy <- 0
best_threshold <- 0

for (threshold in seq(0.1, 0.95, by = 0.05)) {
  
  # predicting responses using logistic regression model
  predictions <- predict(stepwise_cardio_log_reg, newdata = df_cat, type = "response")
  
  # choosing a threshold
  predicted_classes <- ifelse(predictions > threshold, 1, 0)
  
  # Assuming 'actual_class' is the vector of actual classes
  confusion_matrix <- table(Actual = df_cat$cardio, Predicted = predicted_classes)
  
  # performance metrics
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  # Check if the current accuracy is higher than the maximum accuracy
  if (accuracy > max_accuracy) {
    max_accuracy <- accuracy
    best_threshold <- threshold
  }
  
  cat("Threshold: ", threshold, "\n")
  cat("Accuracy: ", accuracy, "\n\n")
}

cat("Best Threshold: ", best_threshold, "\n")
cat("Max Accuracy: ", max_accuracy, "\n")


    #####    ROC curve     #####
# using pROC to obtain ROC curve (Receiver Operating Characteristic)
tpr_vector <- numeric()    # true positive rate
fpr_vector <- numeric()    # false positive rate

for (threshold in seq(0.1, 0.95, by = 0.05)) {
  
  # predicting responses using logistic regression model
  predictions <- predict(stepwise_cardio_log_reg, newdata = df_cat, type = "response")
  
  # choosing a threshold
  predicted_classes <- ifelse(predictions > threshold, 1, 0)
  
  confusion_matrix <- table(Actual = df_cat$cardio, Predicted = predicted_classes)
  
  # performance metrics
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  

  tpr_vector <- c(tpr_vector, sensitivity)
  fpr_vector <- c(fpr_vector, 1 - specificity)
}

print(sensitivity)
print(specificity)
print(fpr_vector)
print(tpr_vector)

# create ROC object
roc_curve <- pROC::roc(df_cat$cardio, predictions)

#plot(roc_curve, main = "ROC curve", col = "blue", lwd = 2)
plot(roc_curve, main = "ROC curve", col = "blue", lwd = 2, legacy.axes = TRUE)

# reference line
lines(x = c(0, 1), y = c(0, 1), col = "red", lty = 2, lwd = 2)

points(fpr_vector, tpr_vector, col = "green", pch = 19)

legend("bottomright", legend = c("ROC curve", "Reference Line", "Model"), col = c("blue", "red", "green"), lwd = 2)


# identify the optimal point (from the ROC curve)
optimal_point <- coords(roc_curve, "best", ret = "threshold")

cat("Optimal Point:\n")
print(optimal_point)


### different models and variables:


#  linear regression model
# 'cardio' needs to be turned into a numeric variable
df_cat$cardio <- as.numeric(df_cat$cardio) - 1


cardio_lin_reg_model <- lm(cardio ~ ., data = df_cat)

# stepwise variable selection using the AIC criterion
stepwise_cardio_lin_reg <- step(cardio_lin_reg_model, direction = "both", trace = 1, k = 2)

# verify the final model after variable selection
summary(stepwise_cardio_lin_reg)



##### ROC CURVE CHOLESTEROL #####

# logistics regression with CHOLESTEROL as dependent variable
# fit an initial logistic regression model
chol_log_reg_model <- glm(cholesterol ~ ., data = df_cat, family = "binomial")

# perform stepwise variable selection
stepwise_chol_log_reg <- step(chol_log_reg_model, direction = "both")

# print the selected model
summary(stepwise_chol_log_reg)

predictions <- predict(stepwise_chol_log_reg, newdata = df_cat, type = "response")

# calculate ROC curve
roc_curve <- roc(df_cat$cholesterol, predictions)

# identify the optimal point
optimal_point <- coords(roc_curve, "best", ret = "threshold")

# plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# add a diagonal reference line
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# add labels
legend("bottomright", legend = c("ROC Curve", "Reference Line"), col = c("blue", "red"), lwd = 2)

# add optimal point to the plot
points(optimal_point, col = "green", pch = 19)
text(optimal_point, labels = "Optimal Point", pos = 3, col = "green")
cat("Optimal Point:\n")
print(optimal_point)



### linear regresion
# 'cholesterol' needs to be turned into a numeric variable
df_cat$cholesterol <- as.numeric(df_cat$cholesterol) - 1

chol_lin_reg_model <- lm(cholesterol ~ ., data = df_cat)

#  stepwise variable selection using the AIC criterion
stepwise_chol_lin_reg <- step(chol_lin_reg_model, direction = "both", trace = 1, k = 2)

# verify the final model after variable selection
summary(stepwise_chol_lin_reg)
