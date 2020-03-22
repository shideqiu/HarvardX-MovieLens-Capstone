################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes
# Check all necessary libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(lubridate)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

# Load the data

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# add year as a column in the edx & validation datasets
edx <- edx %>% mutate(year = as.numeric(str_sub(title, -5, -2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title, -5, -2)))
edx_genres <- edx %>% separate_rows(genres, sep = '\\|') 
valid_genres <- validation %>% separate_rows(genres, sep = "\\|")
# check the data

head(edx) 

# edx Summary Statistics

summary(edx)

# Number of unique users, movies, and genres in the edx dataset
edx %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId), n_genres = n_distinct(genres))

# Total movie ratings per genre
edx_genres %>%
group_by(genres) %>%
summarize(count = n()) %>%
arrange(desc(count)) %>%
head(10)

# Top 10 movies ranked by number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Ratings distribution
edx %>% group_by(rating) %>%
  ggplot(aes(rating)) +
  geom_histogram(bins = 30, fill = 'steelblue', col = 'black') +
  xlab('Rate') +
  ylab('Count') +
  ggtitle('Rating Distribution') +
  theme(plot.title = element_text(hjust = 0.5))

# Some movies are rated more often than others
# 
edx %>% count(movieId) %>% ggplot(aes(n)) +
  geom_histogram(bins = 30, fill = 'steelblue', col = 'black') +
  scale_x_log10() +
  xlab('Number of ratings') +
  ylab('Number of movies') +
  ggtitle('Number of ratings per movie') +
  theme(plot.title = element_text(hjust = 0.5))

# Genres popularity per year
edx_genres %>% 
  group_by(year, genres) %>% 
  summarize(number = n()) %>% 
  filter(year > 1930) %>% 
  ggplot(aes(year, number)) +
  geom_line(aes(color = genres)) +
  ylab('Number of movies') +
  ggtitle('Genres popularity per year') +
  theme(plot.title = element_text(hjust = 0.5))


# Rating vs release year
edx %>% group_by(year) %>% summarize(rating = mean(rating)) %>% ggplot(aes(year, rating)) +
  geom_point() +
  geom_smooth() +
  ylab('Rate') +
  ggtitle('Rate vs Release Year') +
  theme(plot.title = element_text(hjust = 0.5))

----------------------------------------------------------

# Model Preparation
# Initiate RMSE results to compare various models
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Simplest possible model (Average movie rating model) #
mu <- mean(edx$rating)
mu

naive_rmse <- RMSE(validation$rating, mu)
naive_rmse

# Movie Effect Model #
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% ggplot(aes(b_i)) +
  geom_histogram(bins = 30, fill = 'steelblue', col = 'black') + 
  ggtitle('Movie Effect') +
  theme(plot.title = element_text(hjust = 0.5))

predicted_rating <- mu + validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  pull(b_i)
model_rmse <- RMSE(validation$rating, predicted_rating)
model_rmse

# Movie and User Effect Model #
user_avgs <- edx %>% 
  left_join(movie_avgs, by = 'movieId') %>%
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu - b_i))
user_avgs %>% ggplot(aes(b_u)) +
  geom_histogram(bins = 30, fill = 'steelblue', col = 'black') + 
  ggtitle('User Effect') +
  theme(plot.title = element_text(hjust = 0.5))

predicted_rating <- validation %>%
  left_join(movie_avgs, by = 'movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

user_rmse <- RMSE(validation$rating, predicted_rating)  
user_rmse

# Regularized movie and user effect model #
lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx %>% 
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n() + l))
  
  predicted_rating <- validation %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(validation$rating, predicted_rating))
})

# Plot rmses vs lambdas to select the optimal lambda
data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  ggtitle('rmses vs lambdas') +
  theme(plot.title = element_text(hjust = 0.5))

lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i, b_u with lambda
movie_avgs_reg <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

user_avgs_reg <- edx %>% 
  left_join(movie_avgs_reg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))

predicted_rating <- validation %>%
  left_join(movie_avgs_reg, by = 'movieId') %>%
  left_join(user_avgs_reg, by = 'userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)


rmse <- RMSE(validation$rating, predicted_rating)
rmse

# Regularized movies, users, years and genres #

lambdas <- seq(0, 20, 0.5)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  
  b_i <- edx_genres %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n() + l))
  
  b_u <- edx_genres %>%
    left_join(b_i, by = 'movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n() + l))
  
  b_y <- edx_genres %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    group_by(year) %>%
    summarize(b_y = sum(rating - mu - b_i - mu)/(n() + l))
  
  b_g <- edx_genres %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_y, by = 'year') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_y - mu)/(n() + l))
  
  predicted_rating <- valid_genres %>%
    left_join(b_i, by = 'movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_y, by = 'year') %>%
    left_join(b_g, by = 'genres') %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
})

# Plot rmses vs lambdas to select the optimal lambda
data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  ggtitle('rmses vs lambdas') +
  theme(plot.title = element_text(hjust = 0.5))

lambda <- lambdas[which.min(rmses)]
lambda

# Compute regularized estimates of b_i, b_u, b_y, and b_g with lambda
movie_avgs_reg <- edx_genres %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

user_avgs_reg <- edx_genres %>% 
  left_join(movie_avgs_reg, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n() + lambda))

year_avgs_reg <- edx_genres %>%
  left_join(movie_avgs_reg, by = 'movieId') %>%
  left_join(user_avgs_reg, by = 'userId') %>%
  group_by(year) %>%
  summarize(b_y = sum(rating - mu - b_i - b_u)/(n() + lambda))

genre_avgs_reg <- edx_genres %>%
  left_join(movie_avgs_reg, by = 'movieId') %>%
  left_join(user_avgs_reg, by = 'userId') %>%
  left_join(year_avgs_reg, by = 'year') %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u - b_y)/(n() + lambda))

predicted_rating <- validation %>%
  left_join(movie_avgs_reg, by = 'movieId') %>%
  left_join(user_avgs_reg, by = 'userId') %>%
  left_join(year_avgs_reg, by = 'year') %>%
  left_join(genre_avgs_reg, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  pull(pred)

# Plot rmses vs lambdas to select the optimal lambda
data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  ggtitle('rmses vs lambdas') +
  theme(plot.title = element_text(hjust = 0.5))

rmse <- RMSE(validation$rating, predicted_rating)
rmse

