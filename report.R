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
edx_genres <- edx %>% separate_rows(genres, sep = "\\|") 

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
  geom_smooth()
