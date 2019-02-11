RMSE(4, edx$rating)

ConstantModel <- function(s) {
  model <- list()
  
  model$pred <- 4
  
  model$predict <- function() {
    model$pred
  }
}



#----------------------

library(lubridate)

edx %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  labs(x = 'timestamp', y = 'rating')


partition_timestamp <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)

set.seed(1)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(partition_timestamp)),
             color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(partition_timestamp),
                label = as_datetime(partition_timestamp),
                y = 2.5),
            color = "red", vjust = -1, angle = 90) +
  labs(x = 'timestamp', y = 'rating')

partition_names = c(paste('before', as_datetime(partition_timestamp)),
                    paste('after', as_datetime(partition_timestamp)))

edx %>%
  mutate(partition = factor(ifelse(timestamp < partition_timestamp,
                                   partition_names[1], partition_names[2]),
                            levels = partition_names)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)


#-------------------

PartitionedtModel <- function(s, base_model_generator) {
  partitioned_model <- list()

  # Spliting the dataset in 2,
  # one set for data before the timestamp to do the partition,
  s1 <- s %>% filter(timestamp < partition_timestamp)
  # and the other one for the data on or after the timestampt to do the partition
  s2 <- s %>% filter(timestamp >= partition_timestamp)

  # Generation a model for each dataset
  partitioned_model$model1 <- base_model_generator(s1)
  partitioned_model$model2 <- base_model_generator(s2)

  partitioned_model$predict <- function(t) {
    # 
    pred1 <- partitioned_model$model1$predict(t)
    pred2 <- partitioned_model$model2$predict(t)

    t %>%
      mutate(pred1 = pred1, pred2 = pred2) %>%
      mutate(pred = ifelse(timestamp < partition_timestamp,
                           ifelse(!is.na(pred1), pred1, pred2),
                           ifelse(!is.na(pred2), pred2, pred1))) %>%
      .$pred
  }

  partitioned_model
}

round_ratings <- function(s, ratings) {
  ifelse(s$timestamp < partition_timestamp, round(ratings), round(ratings * 2)/2)
}


#-------------------
SimpleAvgModel <- function(s) {
  model <- list()

  model$mu <- mean(s$rating)

  model$predict <- function(t) {
    model$mu
  }

  model
}

m <- SimpleAvgModel(edx)
RMSE(m$predict(edx), edx$rating)

#-------------------

PseudoLinearBiasBasedModel <- function(s) {
  model <- list()

  model$mu <- mean(s$rating)

  model$movie_info <- s %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))

  model$user_info <- s %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))

  model$predict <- function(t) {
    t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = model$mu +
                    ifelse(!is.na(movie_bias), movie_bias, 0) +
                    ifelse(!is.na(user_bias), user_bias, 0)) %>%
      .$pred
  }

  model
}

m <- PseudoLinearBiasBasedModel(edx)
pred <- m$predict(edx)
RMSE(pred, edx$rating)
# 0.8567039

#-------------------

PartitionedtModel <- function(s, base_model_generator) {
  partitioned_model <- list()

  s1 <- s %>% filter(timestamp < partition_timestamp)
  s2 <- s %>% filter(timestamp >= partition_timestamp)

  partitioned_model$model1 <- base_model_generator(s1)
  partitioned_model$model2 <- base_model_generator(s2)

  partitioned_model$predict <- function(t) {
    pred1 <- partitioned_model$model1$predict(t)
    pred2 <- partitioned_model$model2$predict(t)

    t %>%
      mutate(pred1 = pred1, pred2 = pred2) %>%
      mutate(pred = ifelse(timestamp < partition_timestamp,
                           ifelse(!is.na(pred1), pred1, pred2),
                           ifelse(!is.na(pred2), pred2, pred1))) %>%
      .$pred
  }

  partitioned_model
}

round_ratings <- function(s, ratings) {
  ifelse(s$timestamp < partition_timestamp, round(ratings), round(ratings * 2)/2)
}

m <- PartitionedtModel(edx, SimpleAvgModel)
pred <- m$predict(edx)
RMSE(m$predict(edx), edx$rating)

pm <- PartitionedtModel(edx, PseudoLinearBiasBasedModel)
pred <- pm$predict(edx)
RMSE(pred, edx$rating)
# 0.8524909
mean(edx$rating == round_ratings(edx, pred))
# 0.361467


#-------------------
#-------------------
#-------------------
#-------------------

edx %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point()



library(lubridate)

partition_timestamp <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)

set.seed(1)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(partition_timestamp)),
             color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(partition_timestamp),
                label = as_datetime(partition_timestamp),
                y = 2.5),
            color = "red", vjust = -1, angle = 90)


partition_names = c(paste('before', as_datetime(partition_timestamp)),
                    paste('after', as_datetime(partition_timestamp)))

edx %>%
  mutate(partition = factor(ifelse(timestamp < partition_timestamp,
                                   partition_names[1], partition_names[2]),
                            levels = partition_names)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)





#----------------------


library(lubridate)

half_stars_begining <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)
data_partitions = c(paste('before', as_datetime(half_stars_begining)),
                    paste('after', as_datetime(half_stars_begining)))

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.01, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(half_stars_begining)), color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(half_stars_begining), y = 0, label = as_datetime(half_stars_begining)), size=4, angle=90, vjust=-0.4, hjust=0) +
  labs(x = 'Timestamp', y = 'Rating')

edx %>%
  mutate(partition = factor(ifelse(timestamp < half_stars_begining,
                                   data_partitions[1], data_partitions[2]),
                            levels = data_partitions)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)



library(lubridate)

half_stars_begining <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(half_stars_begining)), color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(half_stars_begining), label = as_datetime(half_stars_begining), y = 2.5), color = "red", vjust = -1, angle = 90) +
  labs(x = 'timestamp', y = 'rating')




#-----------------------
#-----------------------


library(tidyverse)



SimpleBiasLinearModel <- function(s) {
  model <- list()

  model$mu <- mean(s$rating)

  model$movie_info <- s %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))

  model$user_info <- s %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))

  model$predict <- function(t) {
    t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = model$mu + movie_bias + user_bias) %>%
      .$pred
  }

  model
}


edx %>%
  ggplot() +
  geom_point(aes(x = timestamp, y = rating))

m <- SimpleBiasLinearModel(edx)
pred <- m$predict(edx)
RMSE(pred, edx$rating)

pred <- m$predict(validation)
RMSE(pred, validation$rating)

edx %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)

edx %>%
  filter(timestamp < half_stars_begining) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)



edx %>%
  mutate(partition = factor(ifelse(timestamp < half_stars_begining,
                                   data_partitions[1], data_partitions[2]),
                            levels = data_partitions)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)



edx %>%
  ggplot() +
  geom_point(aes(x = timestamp, y = rating))






library(lubridate)

half_stars_begining <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)
data_partitions = c(paste('before', as_datetime(half_stars_begining)),
                    paste('after', as_datetime(half_stars_begining)))

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.01, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(half_stars_begining)), color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(half_stars_begining), y = 0, label = as_datetime(half_stars_begining)), size=4, angle=90, vjust=-0.4, hjust=0) +
  labs(x = 'Timestamp', y = 'Rating')

edx %>%
  mutate(partition = factor(ifelse(timestamp < half_stars_begining,
                                   data_partitions[1], data_partitions[2]),
                            levels = data_partitions)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)


#--------------------------------
#--------------------------------

GenresInfo <- function(s) {
  genres_info = list()
  
  # Getting the available genres
  genres_info$genres <- unique(unlist(strsplit(s$genres, '|', fixed = TRUE)))
  genres_info$genres <-
    genres_info$genres[!is.na(genres_info$genres) &
                         genres_info$genres != '(no genres listed)']
  
  # Generating the column names to use in order to identify the presence of a genre
  genres_info$genre_cols <- str_replace_all(tolower(genres_info$genres), '-', '_')
  genres_info$genre_cols <- c(genres_info$genre_cols, 'without_genre')
  
  genres_info$add_one_hot_genres <- function(t) {
    # Adding a column for each genre with a boolean value to indicate 
    # the presence of the respective genre.
    for (i in 1:length(genres_info$genres)) {
      t[[genres_info$genre_cols[i]]] <- str_detect(t$genres, genres_info$genres[i])
      t[[genres_info$genre_cols[i]]][which(is.na(t[[genres_info$genre_cols[i]]]))] <- FALSE
    }
    
    # Adding an extra column 'without_genre' to indicate that
    # no genres were found,
    t[[genres_info$genre_cols[i+1]]] <-
      is.na(t$genres) |
      t$genres == '' |
      t$genres == '(no genres listed)'
    
    t
  }
  
  # Colums used to store the weights of the genres in a movie.
  genres_info$genre_user_weight_cols <-
    paste(genres_info$genre_cols, 'user_weight', sep = '_')
  # Colums used to store the weights of the genres in an user.
  genres_info$genre_movie_weight_cols <-
    paste(genres_info$genre_cols, 'movie_weight', sep = '_')
  
  # Adding one-hot columns to the dataset for each one of the genres
  s <- genres_info$add_one_hot_genres(s)
  
  # Grouping the dataset by user to get the weights per genre.
  # The weight is intended to reflect the user's proportion of rated movies
  # with a particular genre.
  genres_info$user_genre_weights <- s %>%
    group_by(userId) %>%
    summarise_at(genres_info$genre_cols, funs(user_weight = mean(.)))
  
  # For each row in the dataset replacing the value of the genre column
  # for the user's weight if TRUE, or 0 (zero) if FALSE.
  # The idea is that each row contains the apportation of the customer's 
  # preference per genre.
  s <- s %>%
    left_join(genres_info$user_genre_weights, by = 'userId')
  for (i in 1:length(genres_info$genre_cols)) {
    s[[genres_info$genre_cols[i]]] <- 
      ifelse(s[[genres_info$genre_cols[i]]],
             s[[genres_info$genre_user_weight_cols[i]]],
             0)
  }
  
  # Grouping the dataset by movie and summing the weights of all
  # the users that movie.
  # The intention is having the sums of the customers preferences per genre
  # for each one of the movies.
  genres_info$movie_genre_weights <- s %>%
    group_by(movieId) %>%
    summarise_at(genres_info$genre_cols, funs(movie_weight = sum(.)))
  
  # For each one of the movies, the sums of the customers preferences per genre
  # are normalized to sum 1, these would be the genre weights per movie
  # and they intent to represent the proportion that a movie has of a 
  # particular genre.
  sum_movie_genre_weights <-
    rowSums(genres_info$movie_genre_weights[genres_info$genre_movie_weight_cols])
  for (i in 1:length(genres_info$genre_cols)) {
    genres_info$movie_genre_weights[[genres_info$genre_movie_weight_cols[i]]] <-
      genres_info$movie_genre_weights[[genres_info$genre_movie_weight_cols[i]]] /
      sum_movie_genre_weights
  }
  
  genres_info
}


genres_info <- GenresInfo(edx)
training_set <- edx %>%
  genres_info$add_one_hot_genres() %>%
  select(-c('title', 'genres'))




GenreWeights <- function(s, genre_cols) {
  genre_weights <- list()
  
  # Colums used to store the weights of the genres in a movie.
  genre_weights$genre_user_weight_cols <- paste(genre_cols, 'user_weight', sep = '_')
  # Colums used to store the weights of the genres in an user.
  genre_weights$genre_movie_weight_cols <- paste(genre_cols, 'movie_weight', sep = '_')
  
  # Grouping the dataset by user to get the weights per genre.
  # The weight is intended to reflect the user's proportion of rated movies
  # with a particular a genre.
  genre_weights$user_genre_weights <- s %>%
    group_by(userId) %>%
    summarise_at(genre_cols, funs(user_weight = mean(.)))
  
  # For each row in the dataset replacing the value of the genre column
  # for the user's weight if TRUE, or 0 (zero) if FALSE.
  # The idea is that each row contains the apportation of the customer's 
  # preference per genre.
  s <- s %>%
    left_join(genre_weights$user_genre_weights, by = 'userId')
  for (i in 1:length(genre_cols)) {
    s[[genre_cols[i]]] <- 
      ifelse(s[[genre_cols[i]]], s[[genre_weights$genre_user_weight_cols[i]]], 0)
  }
  
  # Grouping the dataset by movie and summing the weights of all
  # the users that movie.
  # The intention is having the sums of the customers preferences per genre
  # for each one of the movies.
  genre_weights$movie_genre_weights <- s %>%
    group_by(movieId) %>%
    summarise_at(genre_cols, funs(movie_weight = sum(.)))
  
  # For each one of the movies, the sums of the customers preferences per genre
  # are normalized to sum 1, these would be the genre weights per movie
  # and they intent to represent the proportion that a movie has of a 
  # particular genre.
  sum_movie_genre_weights <-
    rowSums(genre_weights$movie_genre_weights[genre_weights$genre_movie_weight_cols])
  for (i in 1:length(genre_cols)) {
    genre_weights$movie_genre_weights[[genre_weights$genre_movie_weight_cols[i]]] <-
      genre_weights$movie_genre_weights[[genre_weights$genre_movie_weight_cols[i]]] /
      sum_movie_genre_weights
  }
  
  genre_weights
}


MovieUserBiasModel <- function(s) {
  model <- list()
  
  model$mu <- mean(s$rating)
  
  model$movie_info <- s %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))
  
  model$user_info <- s %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))
  
  model$predict <- function(t) {
    t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = model$mu + movie_bias + user_bias) %>%
      .$pred
  }
  
  model
}



RatingNaiveBayes <- function(s) {
  model <- list()
  
  model$ratings <- sort(unique(s$rating))
  
  model$rating_movie_cols <- paste('rating_movie', model$ratings, sep = '_')
  model$rating_user_cols <- paste('rating_user', model$ratings, sep = '_')
  
  model$movie_info <- s %>%
    group_by(movieId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_movie_', fill = 0) %>%
    left_join(s %>% group_by(movieId) %>% summarise(num_ratings = n()),
              by = 'movieId') %>%
    group_by(movieId) %>%
    summarise_at(model$rating_movie_cols, funs(sum(.) / num_ratings))
  
  model$user_info <- s %>%
    group_by(userId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_user_', fill = 0) %>%
    left_join(s %>% group_by(userId) %>% summarise(num_ratings = n()),
              by = 'userId') %>%
    group_by(userId) %>%
    summarise_at(model$rating_user_cols, funs(sum(.) / num_ratings))
  
  model$predict <- function(t) {
    pred_dataset <- t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId')
    
    max_prod <- NULL
    selected_rating <- NULL
    for (i in 1:length(model$ratings)) {
      prod <- pred_dataset[[model$rating_movie_cols[i]]] *
        pred_dataset[[model$rating_user_cols[i]]]
      
      if (i <= 1) {
        selected_rating <- rep(model$ratings[i], nrow(t))
        max_prod <- prod
      } else {
        selected_rating <- ifelse(prod >= max_prod, model$ratings[i], selected_rating)
        max_prod <- ifelse(prod >= max_prod, prod, max_prod)
      }
    }
    
    selected_rating
  }
  
  model
}


RF_Rec <- function(s) {
  model <- list()
  
  model$ratings <- sort(unique(s$rating))
  
  model$rating_movie_cols <- paste('rating_movie', model$ratings, sep = '_')
  model$rating_user_cols <- paste('rating_user', model$ratings, sep = '_')
  
  model$movie_info <- s %>%
    group_by(movieId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_movie_', fill = 0) %>%
    group_by(movieId) %>%
    summarise_at(model$rating_movie_cols, funs(sum(.))) %>%
    left_join(s %>% group_by(movieId) %>% summarise(movie_avg = mean(rating)),
              by = 'movieId')
  
  model$user_info <- s %>%
    group_by(userId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_user_', fill = 0) %>%
    group_by(userId) %>%
    summarise_at(model$rating_user_cols, funs(sum(.))) %>%
    left_join(s %>% group_by(userId) %>% summarise(user_avg = mean(rating)),
              by = 'userId')
  
  
  model$predict <- function(t) {
    pred_dataset <- t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId')
    
    max_prod <- NULL
    selected_rating <- NULL
    for (i in 1:length(model$ratings)) {
      prod <- (pred_dataset[[model$rating_movie_cols[i]]] + 1 +
                 ifelse(round(pred_dataset$movie_avg) == model$ratings[i], 1, 0)) *
        (pred_dataset[[model$rating_user_cols[i]]] + 1 +
           ifelse(round(pred_dataset$user_avg) == model$ratings[i], 1, 0))
      
      if (i <= 1) {
        selected_rating <- rep(model$ratings[i], nrow(t))
        max_prod <- prod
      } else {
        selected_rating <- ifelse(prod > max_prod, model$ratings[i], selected_rating)
        max_prod <- ifelse(prod > max_prod, prod, max_prod)
      }
    }
    
    selected_rating
  }
  
  model
}









half_stars_begining <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)

SplitModel <- function(s, model_generator) {
  split_model <- list()
  
  s1 <- s %>% filter(timestamp < half_stars_begining)
  s2 <- s %>% filter(timestamp >= half_stars_begining)
  
  split_model$model1 <- model_generator(s1)
  split_model$model2 <- model_generator(s2)
  
  split_model$predict <- function(t) {
    #t1 <- t %>% filter(timestamp < half_stars_begining)
    #t2 <- t %>% filter(timestamp >= half_stars_begining)
    
    #pred1 <- split_model$model1$predict(t1)
    #pred2 <- split_model$model2$predict(t2)
    
    #t %>%
    #  select(userId, movieId, timestamp) %>%
    #  left_join(t1 %>% select(userId, movieId, timestamp) %>% mutate(pred1 = pred1), 
    #            by = c('userId', 'movieId', 'timestamp')) %>%
    #  left_join(t2 %>% select(userId, movieId, timestamp) %>% mutate(pred2 = pred2), 
    #            by = c('userId', 'movieId', 'timestamp')) %>%
    #  mutate(pred = ifelse(!is.na(pred1), pred1, pred2)) %>%
    #  .$pred
    
    pred1 <- split_model$model1$predict(t)
    pred2 <- split_model$model2$predict(t)
    
    t %>%
      mutate(pred1 = pred1, pred2 = pred2) %>%
      mutate(pred = ifelse(timestamp < half_stars_begining,
                           ifelse(!is.na(pred1), pred1, pred2),
                           ifelse(!is.na(pred2), pred2, pred1))) %>%
      .$pred
  }
  
  split_model
}

round_ratings <- function(s, ratings) {
  ifelse(s$timestamp < half_stars_begining,
         ifelse(!is.na(ratings), round(ratings), 4),
         ifelse(!is.na(ratings), round(ratings * 2)/2, 4))
}


split_bias_model <- SplitModel(edx, MovieUserBiasModel)
pred_split_bias_model <- split_bias_model$predict(edx)

RMSE(pred_split_bias_model, edx$rating)
# 0.8524909
mean(round_ratings(edx, pred_split_bias_model) == edx$rating)
# 0.361467


split_rnb_model <- SplitModel(edx, RatingNaiveBayes)
pred_split_rnb_model <- split_rnb_model$predict(edx)
RMSE(pred_split_rnb_model, edx$rating)
# 0.9916377
mean(pred_split_rnb_model == edx$rating)
mean(round_ratings(edx, pred_split_rnb_model) == edx$rating)
# 0.3887722


#---
training_set_1 <- edx %>%
  filter(timestamp < half_stars_begining)

tmp_m <- RF_Rec(training_set_1)
tmp_pred <- tmp_m$predict(training_set_1)
RMSE(tmp_pred, training_set_1$rating)
mean(tmp_pred == training_set_1$rating)

#----

training_set <- edx %>%
  add_one_hot_genres() %>%
  select(-c('title', 'genres'))




#----

tmp_pred <- split_bias_model$predict(validation)
RMSE(tmp_pred, tmp_rubric$rating)
mean(round_ratings(validation, tmp_pred) == tmp_rubric$rating)
# 0.3581094

tmp_pred <- split_rnb_model$predict(validation)
RMSE(tmp_pred, tmp_rubric$rating)
mean(round_ratings(validation, tmp_pred) == tmp_rubric$rating)
# 0.3756354

mean(4 == tmp_rubric$rating)
# 0.2874203


#-------------------------
edx1 <- edx %>% filter(timestamp < half_stars_startpoint)
edx2 <- edx %>% filter(timestamp >= half_stars_startpoint)


#-------------------------

library(recosystem)

train_data <- data_memory(user_index = edx1$userId, item_index = edx1$movieId, 
                          rating = edx1$rating, index1 = T)

recommender <- Reco()
recommender$train(train_data, opts = c(dim = 30, costp_l2 = 0.1, costq_l2 = 0.1, 
                                       lrate = 0.1, niter = 100, nthread = 6,
                                       verbose = F))

tmp_pred <- recommender$predict(train_data, out_memory())

RMSE(tmp_pred, edx1$rating)
mean(round(tmp_pred) == edx1$rating)

#-----------------------

# Getting the available genres
genres <- unique(unlist(strsplit(edx$genres, '|', fixed = TRUE)))
genres <- genres[!is.na(genres) & genres != '(no genres listed)']

# Generating the column names to use in order to identify the presence of a genre
genre_cols <- str_replace_all(tolower(genres), '-', '_')

set_one_hot_genres <- function(t) {
  # Adding a column for each genre with a boolean value to indicate 
  # the presence of the respective genre.
  for (i in 1:length(genres)) {
    t[[genre_cols[i]]] <- str_detect(t$genres, genres[i])
    t[[genre_cols[i]]][which(is.na(t[[genre_cols[i]]]))] <- FALSE
  }
  
  # Removing previous column containing genres as a text-list
  t %>% select(-genres)
}

ext_edx <- set_one_hot_genres(edx)


# Colums used to store the weights of the genres in a movie.
genre_user_weight_cols <- paste(genre_cols, 'user_weight', sep = '_')
# Colums used to store the weights of the genres in an user.
genre_movie_weight_cols <- paste(genre_cols, 'movie_weight', sep = '_')

# Grouping the dataset by user to get the weights per genre.
# The weight is intended to reflect the user's proportion of rated movies
# with a particular genre.
user_genre_weights <- ext_edx %>%
  group_by(userId) %>%
  summarise_at(genre_cols, funs(user_weight = mean(.)))


# For each row in the dataset replacing the value of the genre column
# for the user's weight if TRUE, or 0 (zero) if FALSE.
# The idea is that each row contains the apportation of the customer's 
# preference per genre.
tmp_dataset <- ext_edx %>%
  left_join(user_genre_weights, by = 'userId')
for (i in 1:length(genre_cols)) {
  tmp_dataset[[genre_cols[i]]] <- 
    ifelse(tmp_dataset[[genre_cols[i]]],
           tmp_dataset[[genre_user_weight_cols[i]]],
           0)
}

# Grouping the dataset by movie and summing the weights of all
# the users that rated the movie.
# The intention is having the sums of the customers preferences per genre
# for each one of the movies.
movie_genre_weights <- tmp_dataset %>%
  group_by(movieId) %>%
  summarise_at(genre_cols, funs(movie_weight = sum(.)))

# For each one of the movies, the sums of the customers preferences per genre
# are normalized to sum 1 (if any), these would be the genre weights per movie
# and they intent to represent the proportion that a movie has of a 
# particular genre.
sum_movie_genre_weights <- rowSums(movie_genre_weights[genre_movie_weight_cols])
for (i in 1:length(genre_cols)) {
  movie_genre_weights[[genre_movie_weight_cols[i]]] <-
    ifelse(sum_movie_genre_weights != 0,
           movie_genre_weights[[genre_movie_weight_cols[i]]] / sum_movie_genre_weights,
           0)
}

rm(tmp_dataset, i, sum_movie_genre_weights)


mu <- mean(ext_edx$rating)

movie_info <- ext_edx %>%
  group_by(movieId) %>%
  summarise(movie_bias = mean(rating - mu)) %>%
  left_join(movie_genre_weights, by = 'movieId')

user_info <- ext_edx %>%
  left_join(movie_info, by = 'movieId') %>%
  group_by(userId) %>%
  summarise(user_bias = mean(rating - movie_bias - mu))


tmp_user_info <- ext_edx %>%
  left_join(movie_genre_weights, by = 'movieId')
for (i in 1:length(genre_cols)) {
  tmp_user_info[[genre_cols[i]]] <-
    ifelse(tmp_user_info[[genre_cols[i]]],
           tmp_user_info[[genre_movie_weight_cols[i]]],
           0)
}
tmp_sum_user_genre_weights <- rowSums(tmp_user_info[genre_cols])
for (i in 1:length(genre_cols)) {
  tmp_user_info[[genre_cols[i]]] <-
    ifelse(tmp_sum_user_genre_weights != 0,
           tmp_user_info[[genre_cols[i]]] / tmp_sum_user_genre_weights,
           0)
}

user_info <- tmp_user_info %>%
  left_join(movie_info, by = 'movieId') %>%
  left_join(user_info, by = 'userId') %>%
  group_by(userId) %>%
  summarise_at(genre_cols,
               funs(user_bias = ifelse(sum(. != 0), 
                                       sum((rating - mu - movie_bias - user_bias) * .) / sum(. != 0),
                                       NA))) %>%
  left_join(user_info, by = 'userId')

genre_user_bias_cols <- paste(genre_cols, 'user_bias', sep = '_')


predict <- function(t) {
  t <- t %>%
    left_join(movie_info, by = 'movieId') %>%
    left_join(user_info, by = 'userId') %>%
    mutate(pred = mu +
             ifelse(!is.na(movie_bias), movie_bias, 0) +
             ifelse(!is.na(user_bias), user_bias, 0))
  
  for (i in 1:length(genre_cols)) {
    t$pred <- t$pred + 
      ifelse(!is.na(t[[genre_user_bias_cols[i]]]) & t[[genre_movie_weight_cols[i]]] != 0,
             t[[genre_user_bias_cols[i]]], 0)
  }
  
  t$pred
}


rm(i, tmp_user_info, tmp_sum_user_genre_weights)


pred <- predict(edx)
RMSE(edx$rating, pred)
mean(edx$rating == pred2stars(edx$timestamp, pred))

#-------

LinearLikeGenreBiasBasedModel <- function(s) {
  model <- list()
  
  # Colums used to store the weights of the genres in a movie.
  model$genre_user_weight_cols <- paste(genre_cols, 'user_weight', sep = '_')
  # Colums used to store the weights of the genres in an user.
  model$genre_movie_weight_cols <- paste(genre_cols, 'movie_weight', sep = '_')
  # Colums used to store the bias per genre of an user.
  model$genre_user_bias_cols <- paste(genre_cols, 'user_bias', sep = '_')
  
  # Setting the genres as one-hot columns
  s <- set_one_hot_genres(s)
  
  # Grouping the dataset by user to get the weights per genre.
  # The weight is intended to reflect the user's proportion of rated movies
  # with a particular genre.
  model$user_genre_weights <- s %>%
    group_by(userId) %>%
    summarise_at(genre_cols, funs(user_weight = mean(.)))
  
  # For each row in the dataset replacing the value of the genre column
  # for the user's weight if TRUE, or 0 (zero) if FALSE.
  # The idea is that each row contains the apportation of the customer's 
  # preference per genre.
  tmp_dataset <- s %>%
    left_join(model$user_genre_weights, by = 'userId')
  for (i in 1:length(genre_cols)) {
    tmp_dataset[[genre_cols[i]]] <- 
      ifelse(tmp_dataset[[genre_cols[i]]],
             tmp_dataset[[model$genre_user_weight_cols[i]]],
             0)
  }
  
  # Grouping the dataset by movie and summing the weights of all
  # the users that rated the movie.
  # The intention is having the sums of the customers preferences per genre
  # for each one of the movies.
  model$movie_genre_weights <- tmp_dataset %>%
    group_by(movieId) %>%
    summarise_at(genre_cols, funs(movie_weight = sum(.)))
  
  # For each one of the movies, the sums of the customers preferences per genre
  # are normalized to sum 1 (if any), these would be the genre weights per movie
  # and they intent to represent the proportion that a movie has of a 
  # particular genre.
  sum_movie_genre_weights <-
    rowSums(model$movie_genre_weights[model$genre_movie_weight_cols])
  for (i in 1:length(genre_cols)) {
    model$movie_genre_weights[[model$genre_movie_weight_cols[i]]] <-
      ifelse(sum_movie_genre_weights != 0,
             model$movie_genre_weights[[model$genre_movie_weight_cols[i]]] /
               sum_movie_genre_weights,
             0)
  }
  
  # Average of the total of rating
  model$mu <- mean(s$rating)
  
  # Getting the bias per movie
  model$movie_info <- s %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu)) %>%
    left_join(model$movie_genre_weights, by = 'movieId')
  
  # Getting the bias per user
  model$user_info <- s %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))
  
  
  tmp_user_info <- s %>%
    left_join(model$movie_genre_weights, by = 'movieId')
  for (i in 1:length(genre_cols)) {
    tmp_user_info[[genre_cols[i]]] <-
      ifelse(tmp_user_info[[genre_cols[i]]],
             tmp_user_info[[model$genre_movie_weight_cols[i]]],
             0)
  }
  sum_genre_weights <- rowSums(tmp_user_info[genre_cols])
  for (i in 1:length(genre_cols)) {
    tmp_user_info[[genre_cols[i]]] <-
      ifelse(sum_genre_weights != 0,
             tmp_user_info[[genre_cols[i]]] / sum_genre_weights,
             0)
  }
  
  model$user_info <- tmp_user_info %>%
    left_join(model$movie_info, by = 'movieId') %>%
    left_join(model$user_info, by = 'userId') %>%
    group_by(userId) %>%
    summarise_at(
      genre_cols,
      funs(user_bias =
             ifelse(
               sum(. != 0), 
               sum((rating - model$mu - movie_bias - user_bias) * .) / sum(. != 0),
               NA))) %>%
    left_join(model$user_info, by = 'userId')
  
  model$predict <- function(t) {
    t <- t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = model$mu +
               ifelse(!is.na(movie_bias), movie_bias, 0) +
               ifelse(!is.na(user_bias), user_bias, 0))
    
    for (i in 1:length(genre_cols)) {
      t$pred <- t$pred + 
        ifelse(!is.na(t[[model$genre_user_bias_cols[i]]]) &
                 t[[model$genre_movie_weight_cols[i]]] != 0,
               t[[model$genre_user_bias_cols[i]]],
               0)
    }
    
    t$pred
  }
  
  model
}

get_performance_metrics(LinearLikeGenreBiasBasedModel)

#---
model <- LinearLikeGenreBiasBasedModel(edx)
pred <- model$predict(edx)
RMSE(edx$rating, pred)
mean(edx$rating == pred2stars(edx$timestamp, pred))


