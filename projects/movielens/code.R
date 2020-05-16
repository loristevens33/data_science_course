## Data Creation

# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# Load libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(lubridate)

# Download the MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>%
  mutate(movieId = as.numeric(levels(movieId))[movieId],
  title = as.character(title),
  genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Create validation dataset (10% of MovieLens dataset)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Validate userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove unnecessary data
rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Convert timestamp to YYYY-MM-DD HH:MM:SS format
edx$timestamp <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$timestamp <- as.POSIXct(validation$timestamp, origin="1970-01-01")

# Create year column in datasets
edx$year <- format(edx$timestamp,"%Y")
validation$year <- format(validation$timestamp,"%Y")

# Create test dataset (20% of the edx dataset)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
test_set <- edx[test_index,]

# Create train dataset (80% of the edx dataset)
train_set <- edx[-test_index,]

# Validate userId and movieId in test set are also in train set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Generate sample records of edx dataset
head(edx) %>%
  knitr::kable()

# Create summary of edx dataset
summary(edx)

# Calculate total number of unique users and movies in edx dataset
edx %>%
  summarize(unique_users = n_distinct(userId), unique_movies = n_distinct(movieId)) %>%
   knitr::kable()

# Create bar chart by rating in edx dataset
edx %>%
  group_by(rating) %>%
  summarize(rpercent = n() / nrow(edx) * 100) %>%
  ggplot(aes(rating, rpercent)) +
  geom_col(color = "black",
           fill = "gray50") +
  xlab("Ratings") +
  ylab("% of Ratings") +
  geom_vline(aes(xintercept = 3.5),
    size = 0.5, color = "blue",
    linetype = "dashed") +
  geom_text(aes(x = 2, y = 15, label = "Average = 3.5")) +
  ggtitle("Distribution of Ratings") +
  theme(plot.title = element_text(hjust = 0.5))

# Create boxplot of movie ratings by year
edx %>%
  group_by(movieId) %>%
  filter(year != "2009") %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Year") +
  ylab("Ratings (log10)") +
  ggtitle("Volume of Ratings by Year")+
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram of volume of ratings per movie
edx %>%
  dplyr::count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "gray50") +
  scale_x_log10() +
  geom_vline(aes(xintercept = 843),
  size = 0.5, color = "blue",
  linetype = "dashed") +
  geom_text(aes(x = 8000, y = 500, label = "Average = 843")) +
  xlab("Ratings") +
  ylab("Movies") +
  ggtitle("Ratings per Movie")+
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram of volume of ratings per user
edx %>%
  dplyr::count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "gray50") +
  scale_x_log10() +
  geom_vline(aes(xintercept = 129),
  size = 0.5, color = "blue",
  linetype = "dashed") +
  geom_text(aes(x = 500, y = 5000, label = "Average = 129")) +
  xlab("Ratings") +
  ylab("Users") +
  ggtitle("Ratings per User")+
  theme(plot.title = element_text(hjust = 0.5))

# Create histogram of mean ratings by user
edx %>%
  group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(mean_rating)) +
  geom_histogram(color = "gray50") +
  xlab("Mean Rating") +
  ylab("Users") +
  ggtitle("Mean Ratings by User")+
  theme(plot.title = element_text(hjust = 0.5))

# Create RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))}

### Baseline Model

# Calculate average of train dataset
mu_hat <- mean(train_set$rating)
print(mu_hat)

# Calculate RMSE of baseline model
naive_rmse <- RMSE(test_set$rating, mu_hat)
print(naive_rmse)

# Baseline model RMSE results table
rmse_results <- data_frame(Method = "Baseline Model", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

### Movie Effect Model

# Calculate average of train dataset
mu <- mean(train_set$rating)
print(mu)

# Calculate bias of each movie in train dataset
movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

# Predict ratings with movie effect model
predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  .$b_i

# Calculate RMSE of movie effect model
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
print(model_1_rmse)

# Add movie effect model RMSE results to table
rmse_results <- bind_rows(rmse_results, data_frame(Method = "Movie Effect Model", RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

### User Effect Model

# Calculate bias of each user in train dataset
user_avgs <- train_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# Predict ratings with user effect model
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

# Calculate RMSE of user effect model
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
print(model_2_rmse)

# Add user effect model RMSE results to table
rmse_results <- bind_rows(rmse_results, data_frame(Method = "User Effect Model", RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

### Regularization Model

# Create sequence of potential lambdas to evaluate
lambdas <- seq(0, 10, 0.25)

# Use cross validation to identify ideal lambda as a tuning parameter
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- test_set %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))})

# Plot lambas for evaluation
qplot(lambdas, rmses) +
  geom_vline(xintercept=4.75, col = "blue", linetype = "dashed") +
  geom_text(data = data.frame(x=5.5, y=0.8654), mapping = aes(x, y, label="4.75"), color="blue") +
  xlab("Lambda") +
  ylab("RMSE") +
  ggtitle("RMSE by Lambda") +
  theme(plot.title = element_text(hjust = 0.5))

# Save optimal lambda for use in model
lambda <- lambdas[which.min(rmses)]

# Calculate regularized estimate of b_i
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# Calculate regularized estimate of b_u
b_u <- train_set %>%
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Predict ratings with regularization model
predicted_ratings <- test_set %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE of regularization model
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
print(model_3_rmse)

# Add regularization model RMSE results to table
rmse_results <- bind_rows(rmse_results, data_frame(Method = "Regularization Model", RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

# Calculate regularized estimate of b_i
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

# Calculate regularized estimate of b_u
b_u <- edx %>%
  left_join(b_i, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

# Predict ratings with regularization model
predicted_ratings <- validation %>%
  left_join(b_i, by = 'movieId') %>%
  left_join(b_u, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

# Calculate RMSE of regularization model
final_rmse <- RMSE(predicted_ratings, validation$rating)
print(final_rmse)

# Add final calculation of RMSE results to table
rmse_results <- bind_rows(rmse_results, data_frame(Method = "RMSE w/Validation", RMSE = final_rmse))
rmse_results %>% knitr::kable()

# Create bar chart by model
rmse_results %>%
  mutate(Method = reorder(Method, RMSE)) %>%
  ggplot(aes(Method, RMSE)) +
  geom_bar(stat="identity", show.legend = FALSE) +
  geom_col(color = "black",
           fill = "gray50") +
  coord_flip() +
  xlab("Model") +
  ylab("RMSE") +
  ggtitle("RMSE by Model") +
  theme(plot.title = element_text(hjust = 0.15))