# Load necessary libraries
library(tidyverse)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)

# Load the dataset
df <- read.csv("/Users/aimanmagi/Desktop/A1 python/diabetes2.csv")  # Replace with your dataset path

# Data preprocessing
# Check for missing values
print(colSums(is.na(df)))

# Optionally handle missing values
# df <- na.omit(df)  # Remove rows with missing values

# Define features and target
X <- df %>% select(-Outcome)
y <- df$Outcome

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex,]
X_test <- X[-trainIndex,]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Standardize the feature variables
preProcValues <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(preProcValues, X_train)
X_test_scaled <- predict(preProcValues, X_test)

# Logistic Regression
log_reg <- glm(Outcome ~ ., data = cbind(X_train_scaled, Outcome = y_train), family = binomial)

# Predictions
y_pred_log_reg_prob <- predict(log_reg, newdata = X_test_scaled, type = "response")
y_pred_log_reg <- ifelse(y_pred_log_reg_prob > 0.5, 1, 0)

# Confusion matrix and classification report
confusionMatrix(factor(y_pred_log_reg), factor(y_test), positive = "1")

# ROC curve and AUC
roc_log_reg <- roc(y_test, y_pred_log_reg_prob)
plot.roc(roc_log_reg, main = "ROC Curve for Logistic Regression")
auc(roc_log_reg)

# Decision Tree Classifier
tree_clf <- rpart(Outcome ~ ., data = cbind(X_train, Outcome = y_train), method = "class", control = rpart.control(cp = 0.01))

# Predictions
y_pred_tree_prob <- predict(tree_clf, newdata = X_test, type = "prob")[,2]
y_pred_tree <- ifelse(y_pred_tree_prob > 0.5, 1, 0)

# Confusion matrix and classification report
confusionMatrix(factor(y_pred_tree), factor(y_test), positive = "1")

# ROC curve and AUC
roc_tree <- roc(y_test, y_pred_tree_prob)
plot.roc(roc_tree, main = "ROC Curve for Decision Tree")
auc(roc_tree)

# Compare the models
cat('Logistic Regression AUC: ', auc(roc_log_reg), '\n')
cat('Decision Tree AUC: ', auc(roc_tree), '\n')

