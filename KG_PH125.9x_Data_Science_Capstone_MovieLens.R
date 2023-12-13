## ----setup, include=FALSE-------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, 
                      message = FALSE, fig.align = "center",
                      out.width = "80%")


## ----Create edx and final_holdout_test sets provided by edx---------------------------------------------------
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(float)) install.packages("float", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(dplyr)
library(lubridate)
library(ggplot2)
library(kableExtra)
library(float)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


## ----Top 6 rows of edx----------------------------------------------------------------------------------------
# A quick glimpse of top 6 rows of edx dataset prior to cleaning, data manipulation
head(edx) %>%
  kable(caption = "Top 6 rows of edx raw dataset", align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE, row.names = FALSE) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----structure of edx dataset, echo=TRUE----------------------------------------------------------------------
str(edx)


## ----check for NA values in the edx dataset-------------------------------------------------------------------
#check for NA in the edx dataset
na <- anyNA(edx)


## ----extract the year of the movie release from the title column----------------------------------------------
release_year <- as.numeric(str_sub(edx$title, start = -5, end = -2))
edx <- edx %>% mutate(release_year = as.integer(release_year))


## ----Convert timestamp column into human readable format------------------------------------------------------
edx <- edx %>% mutate(review_year = year(as_date(as_datetime(timestamp))))


## ----Top 6 rows of edx after pre-processing-------------------------------------------------------------------
# A quick glimpse of top 6 rows of edx dataset after data cleaning and data manipulation
head(edx) %>%
  kable(caption = "Top 6 rows of edx dataset after pre-processing", align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE, row.names = FALSE) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----apply pre-processing to final holdout set as well--------------------------------------------------------

release_year <- as.numeric(str_sub(final_holdout_test$title, start = -5, end = -2))
final_holdout_test <- final_holdout_test %>% mutate(release_year = as.integer(release_year))
final_holdout_test <- final_holdout_test %>% mutate(review_year = year(as_date(as_datetime(timestamp))))



## ----glimpse of final hold out set after pre-processing-------------------------------------------------------
# Glimpse of final hold out set after pre-processing
head(final_holdout_test) %>%
  kable(caption = "Top 6 rows of final hold out set dataset after pre-processing", align = 'ccclll', format = "latex", linesep = "", booktabs = TRUE, row.names = FALSE) %>%
  row_spec(0, bold=TRUE) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options = c("scale_down", "HOLD_position", "striped"))


## ----ratings by movie-----------------------------------------------------------------------------------------
edx %>% 
  count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram(color="black", fill="blue", bins = 20, binwidth=0.2) +
  labs(title = "Number of ratings per movie",
       x = "Movies", 
       y="Number of Ratings") +
  scale_x_log10()


## ----average rating by movie----------------------------------------------------------------------------------
edx %>% 
  group_by(movieId) %>%
  summarize(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=20, color="brown") +
  labs(title = "Average ratings by Movie",
       x = "Average rating", 
       y = "Number of movies")


## ----ratings by user------------------------------------------------------------------------------------------
edx %>% 
  count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram(color="brown", fill="purple", bins = 20, binwidth=0.2) +
  labs(title = "Number of ratings per user",
       x = "Users",
       y = "Number of Ratings") +
  scale_x_log10()


## ----average rating by users----------------------------------------------------------------------------------
edx %>% 
  group_by(userId) %>%
  summarize(avg_rating = sum(rating)/n()) %>%
  ggplot(aes(avg_rating)) +
  geom_histogram(bins=20, color="red") +
  labs(title = "Average rating by users",
       x = "Average rating", 
       y = "Number of users")


## ----categorize genres since most movies belong to more than one genre----------------------------------------
# Categorize genres since most movies belong to more than one genre
edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>% summarize(ratings_count = n()) %>%
  ggplot(aes(x = reorder(genres, ratings_count), y = ratings_count, fill = genres)) +
  geom_bar(stat = "identity", color = "gray") +
  labs(title = "Number of ratings by genre", 
       x = "Genres",
       y = "Number of Ratings") +
  coord_flip() 


## ----Average ratings by genre---------------------------------------------------------------------------------
# Categorize genres since most movies belong to more than one genre
edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>% summarize(avg_rating = mean(rating)) %>%
  ggplot(aes(x = reorder(genres, avg_rating), y = avg_rating, fill = genres)) +
  geom_bar(stat = "identity", color = "green") +
  labs(title = "Average ratings by genre", 
       x = "Average rating",
       y = "Genres") +
  coord_flip() 


## ----ratings by the year a movie was rated--------------------------------------------------------------------
# Ratings by the year a movie was rated
edx %>% 
  group_by(review_year) %>%
  summarize(rating = n()) %>%
  ggplot(aes(review_year, rating)) +
  geom_line(color="red") +
  labs(title = "Number of ratings by review year",
       x = "Review Year",
       y = "Number of Ratings") +
  scale_y_continuous(labels = scales::comma) 


## ----ratings by the year a movie was released-----------------------------------------------------------------
# Ratings by the year a movie was released
edx %>%
  group_by(release_year) %>%
  summarize(rating = n()) %>%
  ggplot(aes(release_year, rating)) +
  geom_line(color="purple") +
  labs(title = "Number of ratings by release year",
       x = "Release Year",
       y = "Number of Ratings") +
  scale_y_continuous(labels = scales::comma)


## ----data frame of dataset and split--------------------------------------------------------------------------

# Table to display the dataset and split percentage
Dataset <- c("MovieLens", "edx", "final_hold_out", "train_set", "test_set")
Split <- c("N/A", "90% of MovieLens", "10% of MovieLens", "90% of edx", "10% of edx")
dataset_split_df <- data.frame(Dataset, Split)

 dataset_split_df %>% knitr::kable()



## ----split edx into train and test sets-----------------------------------------------------------------------
# Split edx into train and test sets
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)

edx_train_set <- edx[-test_index,]
edx_temp <- edx[test_index,]

edx_test_set <- edx_temp %>%
  semi_join(edx_train_set, by = "movieId") %>%
  semi_join(edx_train_set, by = "userId")

removed <- anti_join(edx_temp, edx_test_set)
edx_train_set <- rbind(edx_train_set, removed)
rm(edx_temp, test_index, removed)


## ----define RMSE----------------------------------------------------------------------------------------------
RMSE <- function(predicted_ratings, observed_ratings) {
  sqrt(mean((predicted_ratings - observed_ratings)^2))
}


## ----Naive RMSE-----------------------------------------------------------------------------------------------
edx_train_set_mu <- mean(edx_train_set$rating)
Naive_RMSE <- RMSE(edx_train_set_mu, edx_test_set$rating)
Naive_RMSE


## ----movie bias-----------------------------------------------------------------------------------------------
# Movie bias effects on RMSE
movie_bias <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(bm = mean(rating - edx_train_set_mu))

prediction_bm <- edx_train_set_mu + edx_test_set %>%
  left_join(movie_bias, by = "movieId") %>% .$bm
Movie_RMSE <- RMSE(prediction_bm, edx_test_set$rating)
Movie_RMSE


## ----user bias------------------------------------------------------------------------------------------------
# User bias effects on RMSE
user_bias <- edx_train_set %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - edx_train_set_mu))

prediction_bu <- edx_train_set_mu + edx_test_set %>%
  left_join(user_bias, by = "userId") %>% .$bu
User_RMSE <- RMSE(prediction_bu, edx_test_set$rating)
User_RMSE


## ----genre bias-----------------------------------------------------------------------------------------------
# Movie bias effects on RMSE
genre_bias <- edx_train_set %>%
  group_by(genres) %>%
  summarize(bg = mean(rating - edx_train_set_mu))

prediction_bg <- edx_train_set_mu + edx_test_set %>%
  left_join(genre_bias, by = "genres") %>% .$bg
Genre_RMSE <- RMSE(prediction_bm, edx_test_set$rating)
Genre_RMSE


## ----movie+user bias------------------------------------------------------------------------------------------
# Adding user bias effects on movie RMSE
movie_user_bias <- edx_train_set %>%
  left_join(movie_bias, by = "movieId") %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - edx_train_set_mu - bm))

prediction_bu <- edx_test_set %>% 
  left_join(movie_bias, by = "movieId") %>%
  left_join(movie_user_bias, by = "userId") %>%
  mutate(prediction = edx_train_set_mu + bm + bu) %>% .$prediction
MU_RMSE <- RMSE(prediction_bu, edx_test_set$rating)
MU_RMSE


## ----+ genre effects to movie and user biases-----------------------------------------------------------------
# Adding genre bias effects on movie and user RMSE
movie_user_genre_bias <- edx_train_set %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(movie_user_bias, by = "userId") %>%
  group_by(genres) %>%
  summarize(bg = mean(rating - edx_train_set_mu - bm -bu))

prediction_bg <- edx_test_set %>% 
  left_join(movie_bias, by = "movieId") %>%
  left_join(movie_user_bias, by = "userId") %>%
  left_join(movie_user_genre_bias, by = "genres") %>%
  mutate(prediction = edx_train_set_mu + bm + bu + bg) %>% .$prediction
MUG_RMSE <- RMSE(prediction_bg, edx_test_set$rating)
MUG_RMSE


## ----regularization on the movie_user_genre model-------------------------------------------------------------
# Regularization method on the movie_user_genre model which meets the target requirements.
lambdas <- seq(0,10,0.1)
rmses <- sapply(lambdas, function(lambda) {
  edx_train_set_mu <- mean(edx_train_set$rating)
  b_m <- edx_train_set %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - edx_train_set_mu)/ (n() + lambda))
  
  b_u <- edx_train_set %>%
    left_join(b_m, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - edx_train_set_mu - b_m)/(n() + lambda))
  
  b_g <- edx_train_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - edx_train_set_mu - b_m - b_u)/(n() + lambda))
  
  predicted_ratings <- edx_test_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(prediction = edx_train_set_mu + b_m + b_u + b_g) %>% .$prediction
  return(RMSE(predicted_ratings, edx_test_set$rating))
})


## -------------------------------------------------------------------------------------------------------------
# Identify the lambda yields minimum RMSE value
lambda_mug <- lambdas[which.min(rmses)]


## ----plot of lambdas vs rmses---------------------------------------------------------------------------------

# Plot illustrating lambdas vs rmses generated from the regularized model from above.
lambdas_rmses_df <- data.frame(lambdas, rmses)
lambdas_rmses_df %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point(color="green") +
  geom_hline(yintercept=min(rmses), color="blue") +
  labs(title = "Lambdas vs RMSEs for Regularized Movie+User+Genre Model",
       x = "Lambdas",
       y = "RMSEs") 


## ----RMSE for the regularized movie user genre model----------------------------------------------------------
# Find the RMSE value based on the lambda that yielded min RMSE

b_m <- edx_train_set %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - edx_train_set_mu)/ (n() + lambda_mug))

b_u <- edx_train_set %>%
    left_join(b_m, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - edx_train_set_mu - b_m)/ (n() + lambda_mug))
  
b_g <- edx_train_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - edx_train_set_mu - b_m - b_u)/ (n() + lambda_mug))
  
predicted_ratings_mug <- edx_test_set %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(prediction = edx_train_set_mu + b_m + b_u + b_g) %>% 
    pull(prediction)
MUG_REG_RMSE <- RMSE(predicted_ratings_mug, edx_test_set$rating)
MUG_REG_RMSE



## ----rmse for final hold out set based on the lambda from the regularized model-------------------------------

# Evaluate RMSE on the final hold out set
lambda_final <- lambda_mug

b_m <- edx %>%
  group_by(movieId) %>%
  summarize(b_m = sum(rating - edx_train_set_mu)/ (n() + lambda_final))

b_u <- edx %>%
    left_join(b_m, by = "movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - edx_train_set_mu - b_m)/ (n() + lambda_final))
  
b_g <- edx %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - edx_train_set_mu - b_m - b_u)/ (n() + lambda_final))
  
predicted_ratings_final <- final_holdout_test %>%
    left_join(b_m, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(final_prediction = mean(edx$rating) + b_m + b_u + b_g) %>% .$final_prediction
    
FINAL_RMSE <- RMSE(predicted_ratings_final, final_holdout_test$rating)
FINAL_RMSE



## ----rmse results table, echo=FALSE---------------------------------------------------------------------------
# Table of Models and corresponding RMSE

Models <- c("Naive", "Movie Effects", "User Effects", "Genre Effects", "Movie+User Effects", "Movie+User+Genre Effects", "Regularized Movie+User+Genre", "Final Holdout Set")

RMSEs <- c(Naive_RMSE, Movie_RMSE, User_RMSE, Genre_RMSE, MU_RMSE, MUG_RMSE, MUG_REG_RMSE, FINAL_RMSE)

model_rmse_df <- data.frame(Models, RMSEs)

model_rmse_df %>% knitr::kable()

