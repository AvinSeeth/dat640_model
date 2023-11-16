urine <- read.csv(file = "C:/DAT-640/TicdataTraining.csv",stringsAsFactors = FALSE)
train_data <- na.omit(urine)


# Assuming 'target' is your binary target variable, and 'predictors' are your predictor variables
model <- glm(CARAVAN ~ ., data = train_data, family = binomial)
print(model)


test_data <- read.csv(file = "C:/DAT-640/ticdataTesting.csv",stringsAsFactors = FALSE)
test_predictions <- predict(model, newdata = test_data, type = "response")

# Set a custom threshold (e.g., 0.5) to classify observations
custom_threshold <- 0.5
predicted_labels <- ifelse(test_predictions > custom_threshold, 1, 0)

# Create training and testing datasets
target_data <- read.csv(file = "C:/DAT-640/ticdataTarget.csv",stringsAsFactors = FALSE)
actual_labels <- target_data$Target


# Check the lengths of actual and predicted vectors
length_actual <- length(actual_labels)
length_predicted <- length(predicted_labels)

if (length_actual != length_predicted) {
  stop("Error: Vectors have different lengths.")
} else {
  # Create the confusion matrix
  confusion_matrix <- table(Actual = actual_labels, Predicted = predicted_labels)
  print(confusion_matrix)
  
  # Calculate accuracy, precision, recall, and F1-score
  TP <- sum(actual_labels == 1 & predicted_labels == 1)
  TN <- sum(actual_labels == 0 & predicted_labels == 0)
  FP <- sum(actual_labels == 0 & predicted_labels == 1)
  FN <- sum(actual_labels == 1 & predicted_labels == 0)
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  # Calculate accuracy with a check for division by zero
  accuracy <- ifelse((TP + TN + FP + FN) == 0, 0, (TP + TN) / (TP + TN + FP + FN))
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  cat("Accuracy:", accuracy, "\n")
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("F1-Score:", f1_score, "\n")
}

install.packages("caret")

library(caret)

# Assuming 'y_true' contains true labels and 'y_pred' contains predicted labels
confusion_matrix <- confusionMatrix(predicted_labels, actual_labels)


library(ROCR)

# Assuming 'y_true' contains true labels and 'y_pred' contains predicted probabilities
prediction_obj <- prediction(predicted_labels, actual_labels)
performance_obj <- performance(prediction_obj, "tpr", "fpr")

# Plot ROC curve
plot(performance_obj)


var_importance <- importance(rf_model)
var_importance


print(confusion_matrix)

