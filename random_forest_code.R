install.packages("randomForest")
install.packages("boot")
#install.packages("caret")

library(randomForest)
library(boot)
# Load the caTools package
library(caTools)

#missing_values <- colnames(your_data)[apply(your_data, 2, function(x) any(is.na(x)))]
#your_data$column_with_missing_values <- na.impute(your_data$column_with_missing_values, option = "mean")

urine <- read.csv(file = "C:/DAT-640/TicdataTraining.csv",stringsAsFactors = FALSE)
train_data <- na.omit(urine)

# Set a random seed for reproducibility
#set.seed(123)

# Split the dataset (70% for training, 30% for testing)
#split <- sample.split(urine_na_omit$CARVAN, SplitRatio = 0.7)

# Create training and testing datasets
#train_data <- urine_na_omit[split, ]
#test_data <- urine_na_omit[!split, ]

#train_data
#test_data


# Assuming "r" is the target variable and other variables are predictors
# Modify the formula and parameters as needed
# Assuming 'target' is your target variable and 'predictors' are your predictor variables
#rf_model <- randomForest(r ~ ., data = train_data)  -- Regression
#rf_model <- randomForest(r ~ .,                     -- Regression
#                                                data = urine,  
#                                                importance = TRUE, 
#                                                proximity = TRUE) 
## The below is RF for type classification
rf_model <- randomForest(factor(CARAVAN) ~., data=train_data, nodesize=25, ntree=200)
print(rf_model)
# This is for regression
#rf_model <- randomForest(r ~ ., data = train_data, ntree = 100)

# Use the model to make predictions on the testing dataset
test_data <- read.csv(file = "C:/DAT-640/ticdataTesting.csv",stringsAsFactors = FALSE)
predicted_labels <- predict(rf_model, newdata = test_data)

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