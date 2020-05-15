## Data Creation

# Install Packages
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(tidyr)) install.packages("tidyr")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(caret)) install.packages("caret")
if(!require(randomForest)) install.packages("randomForest")
if(!require(matrixStats)) install.packages("matrixStats")
if(!require(data.table)) install.packages("data.table")

# Load libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(randomForest)
library(matrixStats)
library(data.table)

# Download the wine quality datasets
wine_quality_red <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep = ";") %>%
  mutate(type = "Red")
wine_quality_white <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep = ";") %>%
  mutate(type = "White")

# Combine wine quality datasets
wine_quality <- bind_rows(wine_quality_red, wine_quality_white)
rm(wine_quality_red, wine_quality_white)

# Create test dataset (30% of dataset)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = wine_quality$quality, times = 1, p = 0.3, list = FALSE)
test_set <- wine_quality[test_index,]

# Create train dataset (70% of dataset)
train_set <- wine_quality[-test_index,]
rm(test_index)

# Create validation and test datasets (equally split)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = test_set$quality, times = 1, p = 0.5, list = FALSE)
validation <- test_set[test_index,]
test_set <- test_set[-test_index,]
rm(test_index)

# Generate sample records of dataset
head(wine_quality)

# Generate summary of dataset
summary(wine_quality)

## Data Visualization

### Distribution

# Create density plot of fixed acidity by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(fixed.acidity, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Fixed Acidity") +
  ylab("Density") +
  ggtitle("Fixed Acidity Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of volatile acidity by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(volatile.acidity, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Volatile Acidity") +
  ylab("Density") +
  ggtitle("Volatile Acidity Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of citric acid by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(citric.acid, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Citric Acid") +
  ylab("Density") +
  ggtitle("Citric Acid Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of residual sugar by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(residual.sugar, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Residual Sugar") +
  ylab("Density") +
  ggtitle("Residual Sugar Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of chlorides by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(chlorides, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Chlorides") +
  ylab("Density") +
  ggtitle("Chlorides Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of free sulfur dioxide by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(free.sulfur.dioxide, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Free Sulfur Dioxide") +
  ylab("Density") +
  ggtitle("Free Sulfur Dioxide Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of total sulfur dioxide by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(total.sulfur.dioxide, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Total Sulfur Dioxide") +
  ylab("Density") +
  ggtitle("Total Sulfur Dioxide Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of density by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(citric.acid, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Density") +
  ylab("Density") +
  ggtitle("Density Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of pH by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(pH, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("pH") +
  ylab("Density") +
  ggtitle("pH Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of sulphates by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(sulphates, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Sulphates") +
  ylab("Density") +
  ggtitle("Sulphates Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create density plot of alcohol by wine type
wine_quality %>% 
  mutate(Type = type) %>%
  ggplot(aes(alcohol, fill = Type)) +
  geom_density(alpha = 0.2) +
  xlab("Alcohol") +
  ylab("Density") +
  ggtitle("Alcohol Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

# Create boxplot of fixed acidity by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, fixed.acidity, fill = Type)) +
  geom_boxplot() +
  ylab("Fixed Acidity") +
  ggtitle("Quality of Wine by Fixed Acidity") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of fixed acidity to quality by wine type
cor_fixed <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(fixed.acidity, quality))
cor_results <- data.frame(Variable = "Fixed Acidity", Type = cor_fixed$Type, Correlation = cor_fixed$Correlation)
cor_fixed %>% knitr::kable()

# Create boxplot of volatile acidity by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, volatile.acidity, fill = Type)) +
  geom_boxplot() +
  ylab("Volatile Acidity") +
  ggtitle("Quality of Wine by Volatile Acidity") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of volatile acidity to quality by wine type
cor_volat <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(volatile.acidity, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Volatile Acidity", Type = cor_volat$Type, Correlation = cor_volat$Correlation))
cor_volat %>% knitr::kable()

# Create boxplot of citric acid by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, citric.acid, fill = Type)) +
  geom_boxplot() +
  ylab("Citric Acid") +
  ggtitle("Quality of Wine by Citric Acid Level") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of citric acid to quality by wine type
cor_citric <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(citric.acid, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Citric Acid", Type = cor_citric$Type, Correlation = cor_citric$Correlation))
cor_citric %>% knitr::kable()

# Create boxplot of residual sugar by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, residual.sugar, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Residual Sugar (log10)") +
  ggtitle("Quality of Wine by Residual Sugar Level") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of residual sugar to quality by wine type
cor_resid <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(residual.sugar, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Residual Sugar", Type = cor_resid$Type, Correlation = cor_resid$Correlation))
cor_resid %>% knitr::kable()

# Create boxplot of chlorides by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, chlorides, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Chlorides (log10)") +
  ggtitle("Quality of Wine by Chlorides") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of chlorides to quality by wine type
cor_chlor <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(chlorides, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Chlorides", Type = cor_chlor$Type, Correlation = cor_chlor$Correlation))
cor_chlor %>% knitr::kable()

# Create boxplot of free sulfur dioxide by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, free.sulfur.dioxide, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Free Sulfur Dioxide (log10)") +
  ggtitle("Quality of Wine by Free Sulfur Dioxide") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of free sulfur dioxide to quality by wine type
cor_fsulf <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(free.sulfur.dioxide, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Free Sulfur Dioxide", Type = cor_fsulf$Type, Correlation = cor_fsulf$Correlation))
cor_fsulf %>% knitr::kable()

# Create boxplot of total sulfur dioxide by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, total.sulfur.dioxide, fill = Type)) +
  geom_boxplot() +
  ylab("Total Sulfur Dioxide") +
  ggtitle("Quality of Wine by Total Sulfur Dioxide") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of total sulfur dioxide to quality by wine type
cor_tsulf <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(total.sulfur.dioxide, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Total Sulfur Dioxide", Type = cor_tsulf$Type, Correlation = cor_tsulf$Correlation))
cor_tsulf %>% knitr::kable()

# Create boxplot of density by wine type
wine_quality %>%
  filter(density < 1.01) %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, density, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Density (log10)") +
  ggtitle("Quality of Wine by Density") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of density to quality by wine type
cor_densi <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(density, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Density", Type = cor_densi$Type, Correlation = cor_densi$Correlation))
cor_densi %>% knitr::kable()

# Create boxplot of pH by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, pH, fill = Type)) +
  geom_boxplot() +
  ylab("pH") +
  ggtitle("Quality of Wine by pH") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of pH to quality by wine type
cor_ph <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(pH, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "pH", Type = cor_ph$Type, Correlation = cor_ph$Correlation))
cor_ph %>% knitr::kable()

# Create boxplot of sulphates by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, sulphates, fill = Type)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Sulphates (log10)") +
  ggtitle("Quality of Wine by Sulphates") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of sulphates to quality by wine type
cor_sulph <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(sulphates, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Sulphates", Type = cor_sulph$Type, Correlation = cor_sulph$Correlation))
cor_sulph %>% knitr::kable()

# Create boxplot of alcohol by wine type
wine_quality %>%
  mutate(Type = factor(type), Quality = factor(quality)) %>%
  ggplot(aes(Quality, alcohol, fill = Type)) +
  geom_boxplot() +
  ylab("ABV") +
  ggtitle("Quality of Wine by ABV") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation of alcohol to quality by wine type
cor_abv <- wine_quality %>%
  group_by(Type = type) %>%
  summarize(Correlation = cor(alcohol, quality))
cor_results <- bind_rows(cor_results, data_frame(Variable = "Alcohol", Type = cor_abv$Type, Correlation = cor_abv$Correlation))
cor_abv %>% knitr::kable()

# Create summary of red and white wine correlation results
cor_summary <- cor_results %>%
  filter(Type=="Red") %>%
  mutate(Red = Correlation) %>%
  select(Variable, Red)
cor_white <- cor_results %>%
  filter(Type=="White") %>%
  mutate(White = Correlation) %>%
  select(Variable, White)
cor_summary <- data.frame(inner_join(cor_summary, cor_white))

# Calculate total correlation for variables
cor_fixed2 <- wine_quality %>%
  summarize(Correlation = cor(fixed.acidity, quality))
cor_results2 <- data.frame(Variable = "Fixed Acidity", Total = cor_fixed2$Correlation)

cor_volat2 <- wine_quality %>%
  summarize(Correlation = cor(volatile.acidity, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Volatile Acidity", Total = cor_volat2$Correlation))

cor_citric2 <- wine_quality %>%
  summarize(Correlation = cor(citric.acid, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Citric Acid", Total = cor_citric2$Correlation))

cor_resid2 <- wine_quality %>%
  summarize(Correlation = cor(residual.sugar, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Residual Sugar", Total = cor_resid2$Correlation))

cor_chlor2 <- wine_quality %>%
  summarize(Correlation = cor(chlorides, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Chlorides", Total = cor_chlor2$Correlation))

cor_fsulf2 <- wine_quality %>%
  summarize(Correlation = cor(free.sulfur.dioxide, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Free Sulfur Dioxide", Total = cor_fsulf2$Correlation))

cor_tsulf2 <- wine_quality %>%
  summarize(Correlation = cor(total.sulfur.dioxide, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Total Sulfur Dioxide", Total = cor_tsulf2$Correlation))

cor_densi2 <- wine_quality %>%
  summarize(Correlation = cor(density, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Density", Total = cor_densi2$Correlation))

cor_ph2 <- wine_quality %>%
  summarize(Correlation = cor(pH, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "pH", Total = cor_ph2$Correlation))

cor_sulph2 <- wine_quality %>%
  summarize(Correlation = cor(sulphates, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Sulphates", Total = cor_sulph2$Correlation))

cor_abv2 <- wine_quality %>%
  summarize(Correlation = cor(alcohol, quality))
cor_results2 <- bind_rows(cor_results2, data_frame(Variable = "Alcohol", Total = cor_abv2$Correlation))

# Add total correlation to summary of red and white wine correlation results
cor_summary <- data.frame(inner_join(cor_summary, cor_results2))
cor_summary %>% 
  arrange(desc(abs(Total))) %>%
  knitr::kable()

# Create bar chart by quality
wine_quality %>%
  group_by(quality, type) %>%
  summarize(count = n()) %>%
  ggplot(aes(quality, count, fill = type)) +
  geom_bar(stat="identity", show.legend = TRUE, position = "stack") +
  xlab("Quality") +
  ylab("Number of Ratings") +
  ggtitle("Volume of Quality Scores") +
  theme(plot.title = element_text(hjust = 0.5))

### Logistic Regression

# Create predictor train, test, and validation datasets
train_x <- train_set %>%
  select(alcohol, density, volatile.acidity, chlorides)
test_x <- test_set %>%
  select(alcohol, density, volatile.acidity, chlorides)
validation_x <- validation %>%
  select(alcohol, density, volatile.acidity, chlorides)

# Create outcome train, test, and validation datasets
train_y <- train_set %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)
test_y <- test_set %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)
validation_y <- validation %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)

# Fit logistic regression model on the training dataset
train_glm <- train(train_x, train_y$Quality, method = "glm")

# Make predictions on the test dataset
glm_preds <- predict(train_glm, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(glm_preds == test_y$Quality)
sensitivity <- sensitivity(glm_preds, test_y$Quality)
specificity <- specificity(glm_preds, test_y$Quality)
precision <- precision(glm_preds, test_y$Quality)
results <- data_frame(Model = "Logistic Regression", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision)
results %>% knitr::kable()

### LDA

# Fit LDA model on the training dataset
train_lda <- train(train_x, train_y$Quality, method = "lda")

# Make predictions on the test dataset
lda_preds <- predict(train_lda, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(lda_preds == test_y$Quality)
sensitivity <- sensitivity(lda_preds, test_y$Quality)
specificity <- specificity(lda_preds, test_y$Quality)
precision <- precision(lda_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "LDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

### QDA

# Fit QDA model on the training dataset
train_qda <- train(train_x, train_y$Quality, method = "qda")

# Make predictions on the test dataset
qda_preds <- predict(train_qda, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(qda_preds == test_y$Quality)
sensitivity <- sensitivity(qda_preds, test_y$Quality)
specificity <- specificity(qda_preds, test_y$Quality)
precision <- precision(qda_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "QDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

### Loess

# Fit loess model on the training dataset
train_gam <- train(train_x, train_y$Quality, method = "gamLoess")

# Make predictions on the test dataset
gam_preds <- predict(train_gam, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(gam_preds == test_y$Quality)
sensitivity <- sensitivity(gam_preds, test_y$Quality)
specificity <- specificity(gam_preds, test_y$Quality)
precision <- precision(gam_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "Loess", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

### KNN

# Generate sequence of tuning options
set.seed(1, sample.kind = "Rounding")
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y$Quality, method = "knn", tuneGrid = tuning)
tuning <- train_knn$bestTune

# Fit KNN model on the training dataset
train_knn <- train(train_x, train_y$Quality, method = "knn", tuneGrid = tuning)

# Make predictions on the test dataset
knn_preds <- predict(train_knn, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(knn_preds == test_y$Quality)
sensitivity <- sensitivity(knn_preds, test_y$Quality)
specificity <- specificity(knn_preds, test_y$Quality)
precision <- precision(knn_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "KNN", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

### Random Forest

# Generate sequence of tuning options
set.seed(1, sample.kind = "Rounding")
tuning <- data.frame(mtry = seq(3, 9, 2))
train_rf <- train(train_x, train_y$Quality, method = "rf", nodesize = 1, tuneGrid = tuning)
tuning <- train_rf$bestTune

# Fit random forest model on the training dataset
train_rf <- train(train_x, train_y$Quality, method = "rf", nodesize = 1, tuneGrid = tuning)

# Make predictions on the test dataset
rf_preds <- predict(train_rf, test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(rf_preds == test_y$Quality)
sensitivity <- sensitivity(rf_preds, test_y$Quality)
specificity <- specificity(rf_preds, test_y$Quality)
precision <- precision(rf_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "Random Forest", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

# Generate variable importance ranking
imp <- varImp(train_rf)
imp

### Ensemble

# Fit ensemble model on the predictions
ensemble <- cbind(glm = glm_preds == "H", lda = lda_preds == "H", qda = qda_preds == "H", loess = gam_preds == "H", rf = rf_preds == "H", knn = knn_preds == "H")

# Make predictions on the ensemble
ensemble_preds <- factor(ifelse(rowMeans(ensemble) > 0.5, "H", "L"))

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(ensemble_preds == test_y$Quality)
sensitivity <- sensitivity(ensemble_preds, test_y$Quality)
specificity <- specificity(ensemble_preds, test_y$Quality)
precision <- precision(ensemble_preds, test_y$Quality)
results <- bind_rows(results,data_frame(Model = "Ensemble", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
results %>% knitr::kable()

# Display Results of Models
results %>% knitr::kable()

## White Wine Only Models

# Create predictor train, test, and validation datasets
white_train_x <- train_set %>%
  filter(type == "White") %>%
  select(alcohol, density, volatile.acidity, chlorides)

white_test_x <- test_set %>%
  filter(type == "White") %>%
  select(alcohol, density, volatile.acidity, chlorides)

white_validation_x <- validation %>%
  filter(type == "White") %>%
  select(alcohol, density, volatile.acidity, chlorides)

# Create outcome train, test, and validation datasets
white_train_y <- train_set %>%
  filter(type == "White") %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)

white_test_y <- test_set %>%
  filter(type == "White") %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)

white_validation_y <- validation %>%
  filter(type == "White") %>%
  mutate(Quality = factor(ifelse(quality > 5, "H", "L"))) %>%
  select(Quality)

# Fit logistic regression model on the training dataset
white_train_glm <- train(white_train_x, white_train_y$Quality, method = "glm")

# Make predictions on the test dataset
white_glm_preds <- predict(white_train_glm, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_glm_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_glm_preds, white_test_y$Quality)
specificity <- specificity(white_glm_preds, white_test_y$Quality)
precision <- precision(white_glm_preds, white_test_y$Quality)
white_results <- data_frame(Model = "Logistic Regression", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision)

# Fit LDA model on the training dataset
white_train_lda <- train(white_train_x, white_train_y$Quality, method = "lda")

# Make predictions on the test dataset
white_lda_preds <- predict(white_train_lda, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_lda_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_lda_preds, white_test_y$Quality)
specificity <- specificity(white_lda_preds, white_test_y$Quality)
precision <- precision(white_lda_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "LDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Fit QDA model on the training dataset
white_train_qda <- train(white_train_x, white_train_y$Quality, method = "qda")

# Make predictions on the test dataset
white_qda_preds <- predict(white_train_qda, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_qda_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_qda_preds, white_test_y$Quality)
specificity <- specificity(white_qda_preds, white_test_y$Quality)
precision <- precision(white_qda_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "QDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Fit loess model on the training dataset
white_train_gam <- train(white_train_x, white_train_y$Quality, method = "gamLoess")

# Make predictions on the test dataset
white_gam_preds <- predict(white_train_gam, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_gam_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_gam_preds, white_test_y$Quality)
specificity <- specificity(white_gam_preds, white_test_y$Quality)
precision <- precision(white_gam_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "Loess", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Generate sequence of KNN tuning options
set.seed(1, sample.kind = "Rounding")
white_tuning <- data.frame(k = seq(3, 21, 2))
white_train_knn <- train(white_train_x, white_train_y$Quality, method = "knn", tuneGrid = white_tuning)
white_tuning <- white_train_knn$bestTune

# Fit KNN model on the training dataset
white_train_knn <- train(white_train_x, white_train_y$Quality, method = "knn", tuneGrid = white_tuning)

# Make predictions on the test dataset
white_knn_preds <- predict(white_train_knn, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_knn_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_knn_preds, white_test_y$Quality)
specificity <- specificity(white_knn_preds, white_test_y$Quality)
precision <- precision(white_knn_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "KNN", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Generate sequence of MTRY tuning options
set.seed(1, sample.kind = "Rounding")
white_tuning <- data.frame(mtry = seq(3, 9, 2))
white_train_rf <- train(white_train_x, white_train_y$Quality, method = "rf", nodesize = 1, tuneGrid = white_tuning)
white_tuning <- white_train_rf$bestTune

# Fit random forest model on the training dataset
white_train_rf <- train(white_train_x, white_train_y$Quality, method = "rf", nodesize = 1, tuneGrid = tuning)

# Make predictions on the test dataset
white_rf_preds <- predict(white_train_rf, white_test_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_rf_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_rf_preds, white_test_y$Quality)
specificity <- specificity(white_rf_preds, white_test_y$Quality)
precision <- precision(white_rf_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "Random Forest", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Fit ensemble model on the predictions
white_ensemble <- cbind(glm = white_glm_preds == "H", lda = white_lda_preds == "H", qda = white_qda_preds == "H", loess = white_gam_preds == "H", rf = white_rf_preds == "H", knn = white_knn_preds == "H")

# Make predictions on the ensemble
white_ensemble_preds <- factor(ifelse(rowMeans(white_ensemble) > 0.5, "H", "L"))

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(white_ensemble_preds == white_test_y$Quality)
sensitivity <- sensitivity(white_ensemble_preds, white_test_y$Quality)
specificity <- specificity(white_ensemble_preds, white_test_y$Quality)
precision <- precision(white_ensemble_preds, white_test_y$Quality)
white_results <- bind_rows(white_results,data_frame(Model = "Ensemble", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
white_results %>% knitr::kable()

## Run Final White Wine Only Model on Validation Set

# Make logistic regression predictions on the validation dataset
final_glm_preds <- predict(white_train_glm, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_glm_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_glm_preds, white_validation_y$Quality)
specificity <- specificity(final_glm_preds, white_validation_y$Quality)
precision <- precision(final_glm_preds, white_validation_y$Quality)
final_results <- data_frame(Model = "Logistic Regression", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision)

# Make LDA model predictions on the validation dataset
final_lda_preds <- predict(white_train_lda, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_lda_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_lda_preds, white_validation_y$Quality)
specificity <- specificity(final_lda_preds, white_validation_y$Quality)
precision <- precision(final_lda_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "LDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Make QDA model predictions on the validation dataset
final_qda_preds <- predict(white_train_qda, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_qda_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_qda_preds, white_validation_y$Quality)
specificity <- specificity(final_qda_preds, white_validation_y$Quality)
precision <- precision(final_qda_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "QDA", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Make loess model predictions on the validation dataset
final_gam_preds <- predict(white_train_gam, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_gam_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_gam_preds, white_validation_y$Quality)
specificity <- specificity(final_gam_preds, white_validation_y$Quality)
precision <- precision(final_gam_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "Loess", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Make KNN model predictions on the validation dataset
final_knn_preds <- predict(white_train_knn, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_knn_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_knn_preds, white_validation_y$Quality)
specificity <- specificity(final_knn_preds, white_validation_y$Quality)
precision <- precision(final_knn_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "KNN", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Make random forest predictions on the validation dataset
final_rf_preds <- predict(white_train_rf, white_validation_x)

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_rf_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_rf_preds, white_validation_y$Quality)
specificity <- specificity(final_rf_preds, white_validation_y$Quality)
precision <- precision(final_rf_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "Random Forest", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))

# Fit ensemble model on the predicted datasets
final_ensemble <- cbind(glm = final_glm_preds == "H", lda = final_lda_preds == "H", qda = final_qda_preds == "H", loess = final_gam_preds == "H", rf = final_rf_preds == "H", knn = final_knn_preds == "H")

# Make predictions on the dataset
final_ensemble_preds <- factor(ifelse(rowMeans(final_ensemble) > 0.5, "H", "L"))

# Calculate accuracy, sensitivity, specificity, and precision
accuracy <- mean(final_ensemble_preds == white_validation_y$Quality)
sensitivity <- sensitivity(final_ensemble_preds, white_validation_y$Quality)
specificity <- specificity(final_ensemble_preds, white_validation_y$Quality)
precision <- precision(final_ensemble_preds, white_validation_y$Quality)
final_results <- bind_rows(final_results,data_frame(Model = "Ensemble", Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity, Precision = precision))
final_results %>% knitr::kable()