edx %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)

library(lubridate)

set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  labs(x = 'timestamp', y = 'rating')


half_stars_startpoint <- min(filter(edx, (rating * 2) %% 2 == 1)$timestamp)

as_datetime(half_stars_startpoint)


set.seed(0)
edx[createDataPartition(y = edx$rating, times = 1, p = 0.001, list = FALSE),] %>%
  ggplot(aes(x = as_datetime(timestamp), y = rating)) +
  geom_point() +
  geom_vline(aes(xintercept = as_datetime(half_stars_startpoint)),
             color = "red", linetype = "dashed") +
  geom_text(aes(x = as_datetime(half_stars_startpoint),
                label = as_datetime(half_stars_startpoint),
                y = 2.5),
            color = "red", vjust = -1, angle = 90) +
  labs(x = 'timestamp', y = 'rating')


partition_names = c(paste('before', as_datetime(half_stars_startpoint)),
                    paste('on or after', as_datetime(half_stars_startpoint)))

edx %>%
  mutate(partition = factor(ifelse(timestamp < half_stars_startpoint,
                                   partition_names[1], partition_names[2]),
                            levels = partition_names)) %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25) +
  facet_grid(~ partition)

rm(partition_names)



#' This object-constructor function is used to generate a model that returns 
#' a as prediction the most common rating in the dataset used to fit it.
#' @param dataset The dataset used to fit the model
#' @return The model
RModeModel <- function(dataset) {
  model <- list()
  
  model$ratings <- unique(dataset$rating)
  model$mode <- model$ratings[which.max(tabulate(match(dataset$rating, model$ratings)))]
  
  #' The prediction function
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction for the given dataset
  model$predict <- function(s) {
    model$mode
  }
  
  model
}


model <- RModeModel(edx)

training_pred <- model$predict(edx)
validation_pred <- model$predict(validation)

sprintf("Train-RMSE: %f, Train-Acc: %f, Val-RMSE: %f, Val-Acc: %f",
        RMSE(training_pred, edx$rating),
        mean(training_pred == edx$rating),
        RMSE(validation_pred, validation$rating),
        mean(validation_pred == validation$rating))

rm(model, training_pred, validation_pred)



#' This object-constructor function is used to generate a metamodel 
#' that contains two models,
#' one fitted for data before the startpoint when half stars were allowed in the
#' ratings, and the other one fitted for data on or after that startpoint.
#' The predictions are performed by choosing the appropriate model according to the 
#' data's timestamp.
#' 
#' @param dataset The dataset used to fit both models,
#'    it should contain a column called 'timestamp'
#' @param base_model_generator The function used to generate the base models,
#'    it should receive a dataset to fit the model and have a prediction function
#' @return The created metamodel
PartitionedModel <- function(dataset, base_model_generator) {
  partitioned_model <- list()
  
  # Spliting the dataset in 2,
  # one set for data before the startpoint when half stars were allowed
  dataset1 <- dataset %>% filter(timestamp < half_stars_startpoint)
  # the other one for the data on or after the startpoint when half stars were allowed
  dataset2 <- dataset %>% filter(timestamp >= half_stars_startpoint)
  
  # Generating a model for each dataset
  partitioned_model$model1 <- base_model_generator(dataset1)
  partitioned_model$model2 <- base_model_generator(dataset2)
  
  #' Performs a prediction with the combined fitted models,
  #' it tries to do the prediction with the respective model based on the timestamp.
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction for each row of the dataset
  partitioned_model$predict <- function(s) {
    # Performing the predictions on the whole dataset for each one of the models
    pred1 <- partitioned_model$model1$predict(s)
    pred2 <- partitioned_model$model2$predict(s)
    
    # Selecting the prediction to use according to the data's timestamp.
    s %>%
      mutate(pred = ifelse(timestamp < half_stars_startpoint, pred1, pred2)) %>%
      .$pred
  }
  
  partitioned_model
}


#' Converts a prediction (which is a floating point number) to a one used to 
#' represent ratings given by stars,
#' i.e. {1, 2, 3, 4, 5} if the timestamp is before the half start startpoint 
#' or {1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5} if the timestamp is on or after.
pred2stars <- function(timestamp, pred) {
  # Rounds the prediction either to be full-stars or having a half-star
  # according to the timestamp
  rounded_pred <- ifelse(timestamp < half_stars_startpoint,
                         round(pred),
                         round(pred * 2)/2)
  
  # Making sure the rating is not smaller that 1 or bigger than 5  
  rounded_pred <- ifelse(rounded_pred >= 1, rounded_pred, 1)
  rounded_pred <- ifelse(rounded_pred <= 5, rounded_pred, 5)
}


#' This function is used to report the performance of a model in terms of
#' RMSE and Accuracy for the training and validation sets.
#' It evaluates the performance in two modes:
#' 1) using the whole training set to fit the model and
#' 2) partitioning the training set before and on-or-after 
#' the startpoint when half stars were allowed.
#' @param  method_name The name of the method to evaluate
#' @param training_set The dataset used to fit the models
#' @param validation_set The dataset used as validation set
#' @param model_generator The constructor function to generate the model
#' @returns A dataset reporting the performance results 
get_performance_metrics <- function(method_name, training_set, validation_set,
                                    model_generator) {

  result <- data.frame('METHOD' = character(), 'SET_TYPE' = character(),
                       'TRAIN_TIME' = integer(),
                       'PRED_TRAIN_TIME' = integer(), 'PRED_VAL_TIME' = integer(),
                       'TRAIN_RMSE' = double(), 'TRAIN_ACC' = double(),
                       'VAL_RMSE' = double(), 'VAL_ACC' = double(),
                       stringsAsFactors = FALSE)

  counter <- 0

  for (is_partitioned in c(FALSE, TRUE)) {
    counter <- counter + 1
    result[counter,] <- list(method_name, NA, NA, NA, NA, NA, NA)

    train_start <- Sys.time() # recording start o the training

    # Chosing the set type: partitioned or whole
    if (is_partitioned) {
      result[counter, 'SET_MODEL'] <- 'partitioned'
      model <- PartitionedModel(training_set, model_generator)
    } else {
      result[counter, 'SET_MODEL'] <- 'whole'
      model <- model_generator(training_set)
    }

    train_end <- Sys.time() # recording the end of the training

    # Recording the time spent on training
    result[counter, 'TRAIN_TIME'] <- train_end - train_start

    for (is_training in c(TRUE, FALSE)) {
      # Chosing the dataset to evaluate
      if (is_training) {
        ds <- training_set
      } else {
        ds <- validation_set
      }

      pred_start <- Sys.time() # recording the start of the prediction

      # Getting the prediction for the chosen dataset
      pred <- model$predict(ds)

      pred_end <- Sys.time() # recording the end of the prediction

      # Recording the time spent on the prediction
      result[counter, ifelse(is_training, 'PRED_TRAIN_TIME', 'PRED_VAL_TIME')] <-
        pred_end - pred_start

      # Calculating the RMSE
      result[counter, ifelse(is_training, 'TRAIN_RMSE', 'VAL_RMSE')] <-
        RMSE(pred, ds$rating)
      # Calculating the accuracy
      result[counter, ifelse(is_training, 'TRAIN_ACC', 'VAL_ACC')] <-
        mean(pred2stars(ds$timestamp, pred) == ds$rating)
    }
  }

  result
}

results_RModeModel <-
    get_performance_metrics('Ratings Mode', edx, validation, RModeModel)


#' This object-constructor function is used to generate a model
#' that always returns as prediction the average of the rating in the
#' given dataset used to fit the model.
#' @param dataset The dataset used to fit the model
#' @return The model
RAvgModel <- function(dataset) {
  model <- list()
  
  # The average of ratings
  model$mu <- mean(dataset$rating)
  
  #' The prediction function
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction
  model$predict <- function(s) {
    model$mu
  }
  
  model
}

if (!exists('results_RAvgModel')) {
  results_RAvgModel <-
    get_performance_metrics(edx, validation, RAvgModel)
}
results_RAvgModel


#' This object-constructor function is used to generate a model
#' of the form:
#'   r_u,m = mu + b_m + b_u + E_u,m
#' where:
#'  - 'r_u,m' is the rating given by an user 'u' to a movie 'm'
#'  - 'mu' is the average of all the observed ratings
#'  - 'b_m' is the movie effect (movie bias) of a movie 'm'
#'  - 'b_u' is the user effect (user bias) of an user 'u'
#'  - 'E_u,m' is the error in the prediction.
#'
#' @param dataset The dataset used to fit the model
#' @return The model
MovieUserEffectModel <- function(dataset) {
  model <- list()

  # The average of all the ratings in the dataset
  model$mu <- mean(dataset$rating)

  # Getting the movie bias per movie
  model$movie_info <- dataset %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))

  # Getting the user bias per user
  model$user_info <- dataset %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))

  #' The prediction function, it retrieves as prediction:
  #'   mu + b_m + b_u
  #' where:
  #'  - 'mu' is the average of all the observed ratings during training
  #'  - 'b_m' is the movie effect (movie bias) observed during training for a movie 'm'
  #'  -  b_u' is the user effect (user bias) observed during training for an user 'u'
  #'
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction
  model$predict <- function(s) {
    s %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = model$mu +
               ifelse(!is.na(movie_bias), movie_bias, 0) +
               ifelse(!is.na(user_bias), user_bias, 0)) %>%
      .$pred
  }
  
  model
}

if (!exists('results_MovieUserEffectModel')) {
  results_MovieUserEffectModel <-
    get_performance_metrics(edx, validation, MovieUserEffectModel)
}
results_MovieUserEffectModel


#' This object-constructor function is used to generate a model
#' to estimate the probability that an user gives a rating 'r',
#' and the probability that a movie is given a rating 'r',
#' for each one of the existing ratings.
#' Then using those probabilities to estimate the prediction of the
#' rating that an user gives to a movie, by getting the rating which maximizes
#' the product of those probabilities.
#'
#' @param dataset The dataset used to fit the model
#' @return The model
RFNaiveBayesModel <- function(dataset) {
  model <- list()

  # Getting the set of all the existing ratings
  model$ratings <- sort(unique(dataset$rating))

  # Names of the columns to be used to store the probabilities that
  # an user gives a specific rating, there would be as many columns as
  # existing ratings
  model$rating_movie_cols <- paste('rating_movie', model$ratings, sep = '_')

  # Names of the columns to be used to store the probabilities that
  # a movie is given a specific rating, there would be as many columns as
  # existing ratings
  model$rating_user_cols <- paste('rating_user', model$ratings, sep = '_')

  # Information of the movies, including the probability for each movie
  # to be given each one of the existing ratings
  model$movie_info <- dataset %>%
    group_by(movieId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_movie_', fill = 0) %>%
    left_join(dataset %>% group_by(movieId) %>% summarise(num_ratings = n()),
              by = 'movieId') %>%
    group_by(movieId) %>%
    summarise_at(model$rating_movie_cols, funs(sum(.) / num_ratings))

  # Information of the user, including the probability for each user
  # to give each one of the existing ratings
  model$user_info <- dataset %>%
    group_by(userId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_user_', fill = 0) %>%
    left_join(dataset %>% group_by(userId) %>% summarise(num_ratings = n()),
              by = 'userId') %>%
    group_by(userId) %>%
    summarise_at(model$rating_user_cols, funs(sum(.) / num_ratings))

  #' The prediction function, it retrieves as prediction the rating 'r'
  #' that gives the maximun of the products: 
  #'    p(r|u) * p(r|m)
  #' where:
  #'  - 'p(r|u)' is the probability that the user 'u' gives a rating 'r'
  #'  - 'p(r|m)' is the probability that the movie 'm' is rated as 'r'
  #'
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction
  model$predict <- function(s) {
    # Adding p(r|u) and p(r|m) for each rating in each row of the set
    pred_dataset <- s %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId')

    # For missing estimates probabilities the same probability for
    # each rating is assumed
    pred_dataset[is.na(pred_dataset)] <- 1.0 / length(model$ratings)

    # Calculating the maximum of the products p(r|u) * p(r|m)
    # in each row of the dataset
    max_prod <- NULL
    selected_rating <- NULL
    for (i in 1:length(model$ratings)) {
      prod <-
        pred_dataset[[model$rating_movie_cols[i]]] *
        pred_dataset[[model$rating_user_cols[i]]]
      
      if (i <= 1) {
        selected_rating <- rep(model$ratings[i], nrow(s))
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

if (!exists('results_NaiveBayesModel')) {
  results_NaiveBayesModel <-
    get_performance_metrics(edx, validation, NaiveBayesModel)
}
results_NaiveBayesModel



#' This object-constructor function is used to generate a model
#' based in RF-Rec schema.
#'
#' @param dataset The dataset used to fit the model
#' @return The model
RFRecModel <- function(dataset) {
  model <- list()

  # Average of all the observed ratings in the dataset
  model$mu <- mean(dataset$rating)

  # Getting the set of all the existing ratings
  model$ratings <- sort(unique(dataset$rating))

  # Names of the columns to be used to store the frequencies of the ratings
  # that an user gives, there would be as many columns as existing ratings
  model$rating_movie_cols <- paste('rating_movie', model$ratings, sep = '_')
  # Names of the columns to be used to store the frequencies of the ratings
  # that a movie is given, there would be as many columns as existing ratings
  model$rating_user_cols <- paste('rating_user', model$ratings, sep = '_')

  # Information of the movies, including the frequency of the received ratings
  # for each rating
  model$movie_info <- dataset %>%
    group_by(movieId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_movie_', fill = 0) %>%
    group_by(movieId) %>%
    summarise_at(model$rating_movie_cols, funs(sum(.))) %>%
    left_join(dataset %>% group_by(movieId) %>% summarise(movie_avg = mean(rating)),
              by = 'movieId')

  # Information of the user, including the frequency of the given ratings
  # for each rating
  model$user_info <- dataset %>%
    group_by(userId, rating) %>%
    summarise(freq = n()) %>%
    spread(rating, freq, sep = '_user_', fill = 0) %>%
    group_by(userId) %>%
    summarise_at(model$rating_user_cols, funs(sum(.))) %>%
    left_join(dataset %>% group_by(userId) %>% summarise(user_avg = mean(rating)),
              by = 'userId')

  #' The prediction function, it retrieves as prediction the rating 'r'
  #' that gives the maximun of the products: 
  #'    (freq_user(u, r) + 1 + 1_user(u,r)) * (freq_movie(m, r) + 1 + 1_movie(m,r))
  #' where:
  #'  - 'freq_user(u, r)' is the frequency of the rating 'r' given for the user 'u'
  #'  - 'freq_movie(m, r)' is the frequency of the rating 'r' given to the movie 'm'
  #'  - '1_user(u,r)' is 1 if 'r' corresponds to the given average 
  #'     (rounded to the closest existing rating) of the user 'u', or 0 otherwise
  #'  - '1_movie(m,r)' is 1 if 'r' corresponds to the given average
  #'     (rounded to the closest existing rating) of the movie 'm', or 0 otherwise
  #'
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction
  model$predict <- function(s) {
    pred_dataset <- s %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId')

    # In case of missing user or movie averages, using the global average
    pred_dataset$movie_avg[is.na(pred_dataset$movie_avg)] <- model$mu
    pred_dataset$user_avg[is.na(pred_dataset$user_avg)] <- model$mu
    # In case of missing frequencies using 0
    pred_dataset[is.na(pred_dataset)] <- 0

    # Getting the maximum 'r' which maximizes the product
    # (freq_user(u, r) + 1 + 1_user(u,r)) * (freq_movie(m, r) + 1 + 1_movie(m,r))
    # per row in the dataset
    max_prod <- NULL
    selected_rating <- NULL
    for (i in 1:length(model$ratings)) {
      # Calculating the product
      # (freq_user(u, r) + 1 + 1_user(u,r)) * (freq_movie(m, r) + 1 + 1_movie(m,r))
      prod <- (pred_dataset[[model$rating_movie_cols[i]]] + 1 +
               ifelse(pred2stars(s$timestamp, pred_dataset$movie_avg) == model$ratings[i],
                      1, 0)) *   
              (pred_dataset[[model$rating_user_cols[i]]] + 1 +
               ifelse(pred2stars(s$timestamp, pred_dataset$user_avg) == model$ratings[i],
                      1, 0))

      if (i <= 1) {
        selected_rating <- rep(model$ratings[i], nrow(s))
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

if (!exists('results_RFRecModel')) {
  results_RFRecModel <-
    get_performance_metrics(edx, validation, RFRecModel)
}
results_RFRecModel


library(recosystem)

#' This object-constructor function is used to generate a model
#' of the form:
#'   r_u,m ~ mu + b_m + b_u + sum(p_u,k * q_m_k)
#' where:
#'  - 'r_u,m' is the rating given by an user 'u' to a movie 'm'
#'  - 'mu' is the average of all the observed ratings
#'  - 'b_m' is the movie effect (movie bias) of a movie 'm'
#'  - 'b_u' is the user effect (user bias) of an user 'u'
#'  - 'E_u,m' is the error in the prediction.
#'
#' @param dataset The dataset used to fit the model
#' @return The model
MatrixFactorizationModel <- function(dataset) {
  model <- list()

  # The average of all the ratings in the dataset
  model$mu <- mean(dataset$rating)

  # Getting the movie bias for each movie
  model$movie_info <- dataset %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))

  # Getting the user bias for each user
  model$user_info <- dataset %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))

  # Gettint the training set containing the residuals
  training_set <- dataset %>%
    left_join(model$movie_info, by = 'movieId') %>%
    left_join(model$user_info, by = 'userId') %>%
    mutate(residual = rating - (model$mu + movie_bias + user_bias))

  # Training set to perform the matroix factorization of the residuals
  train_data <- data_memory(user_index = training_set$userId,
                            item_index = training_set$movieId, 
                            rating = training_set$residual,
                            index1 = T)
  # Training a recomender, this is the one that does the matrix factorization
  # in this case for 30 factors
  model$recommender <- Reco()
  model$recommender$train(train_data,
                          opts = c(dim = 30, costp_l2 = 0.1, costq_l2 = 0.1, 
                                   lrate = 0.1, niter = 100, nthread = 6,
                                   verbose = F))

  model$predict <- function(s) {
    pred_data <- data_memory(user_index = s$userId, item_index = t$movieId, 
                             index1 = T)

    pred_residuals <- model$recommender$predict(pred_data, out_memory())

    s %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      mutate(pred = pred_residuals + model$mu +
                    ifelse(!is.na(movie_bias), movie_bias, 0) +
                    ifelse(!is.na(user_bias), user_bias, 0)) %>%
      .$pred
  }

  model
}

if (!exists('results_MatrixFactorizationModel')) {
  set.seed(0)
  results_MatrixFactorizationModel <-
    get_performance_metrics(MatrixFactorizationModel)
}
results_MatrixFactorizationModel

#---------------------

# Getting the available genres
genres <- unique(unlist(strsplit(edx$genres, '|', fixed = TRUE)))
genres <- genres[!is.na(genres) & genres != '(no genres listed)']
# Generating the column names to use in order to identify the presence of a genre
genre_cols <- str_replace_all(tolower(genres), '-', '_')

# Colums used to store the weights of the genres in a movie.
genre_user_weight_cols <- paste(genre_cols, 'user_weight', sep = '_')
# Colums used to store the weights of the genres in an user.
genre_movie_weight_cols <- paste(genre_cols, 'movie_weight', sep = '_')
# Colums used to store the bias per genre of an user.
genre_user_bias_cols <- paste(genre_cols, 'user_bias', sep = '_')


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


ext_edx <- set_one_hot_genres(edx) %>% select(-title)

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
tmp_movie_genre_weights <- ext_edx %>%
  left_join(user_genre_weights, by = 'userId')
for (i in 1:length(genre_cols)) {
  tmp_movie_genre_weights[[genre_cols[i]]] <- 
    ifelse(tmp_movie_genre_weights[[genre_cols[i]]],
           tmp_movie_genre_weights[[genre_user_weight_cols[i]]],
           0)
}

# Grouping the dataset by movie and summing the weights of all
# the users that rated the movie.
# The intention is having the sums of the customers preferences per genre
# for each one of the movies.
movie_genre_weights <- tmp_movie_genre_weights %>%
  group_by(movieId) %>%
  summarise_at(genre_cols, funs(movie_weight = sum(.)))

rm(i, tmp_movie_genre_weights)

# For each one of the movies, the sums of the customers preferences per genre
# are normalized to sum 1 (if any), these would be the genre weights per movie
# and they intent to represent the proportion that a movie has of a 
# particular genre.
tmp_sum_movie_genre_weights <- rowSums(movie_genre_weights[genre_movie_weight_cols])
for (i in 1:length(genre_cols)) {
  movie_genre_weights[[genre_movie_weight_cols[i]]] <-
    ifelse(tmp_sum_movie_genre_weights != 0,
           movie_genre_weights[[genre_movie_weight_cols[i]]] /
             tmp_sum_movie_genre_weights,
           0)
}

rm(i, tmp_sum_movie_genre_weights)



LinearLikeGenreBiasBasedModel <- function(s) {
  model <- list()

  # Average of the total of rating
  model$mu <- mean(s$rating)

  # Getting the bias per movie
  model$movie_info <- s %>%
    group_by(movieId) %>%
    summarise(movie_bias = mean(rating - model$mu))

  # Getting the bias per user
  model$user_info <- s %>%
    left_join(model$movie_info, by = 'movieId') %>%
    group_by(userId) %>%
    summarise(user_bias = mean(rating - movie_bias - model$mu))

  tmp_user_info <- s %>%
    left_join(movie_genre_weights, by = 'movieId')
  tmp_sum_genre_weights <- rep(0, nrow(tmp_user_info))
  for (i in 1:length(genre_cols)) {
    tmp_sum_genre_weights <- tmp_sum_genre_weights +
      ifelse(tmp_user_info[[genre_cols[i]]],
             tmp_user_info[[genre_movie_weight_cols[i]]],
             0)
  }
  for (i in 1:length(genre_cols)) {
    tmp_user_info[[genre_cols[i]]] <-
      ifelse(tmp_user_info[[genre_cols[i]]],
             tmp_user_info[[genre_movie_weight_cols[i]]] / tmp_sum_genre_weights,
             0)
  }

  model$user_info <- tmp_user_info %>%
    left_join(model$movie_info, by = 'movieId') %>%
    left_join(model$user_info, by = 'userId') %>%
    mutate(residual = rating - (model$mu + movie_bias + user_bias)) %>%
    group_by(userId) %>%
    summarise_at(
      genre_cols,
      funs(user_bias = ifelse(sum(. != 0) > 0,
                              sum(residual * .) / sum(. != 0),
                              NA))) %>%
    left_join(model$user_info, by = 'userId')

  model$predict <- function(t) {
    t <- t %>%
      left_join(model$movie_info, by = 'movieId') %>%
      left_join(model$user_info, by = 'userId') %>%
      left_join(movie_genre_weights, by = 'movieId') %>%
      mutate(pred = model$mu +
               ifelse(!is.na(movie_bias), movie_bias, 0) +
               ifelse(!is.na(user_bias), user_bias, 0))

    for (i in 1:length(genre_cols)) {
      t$pred <- t$pred + 
        ifelse(!is.na(t[[genre_user_bias_cols[i]]]) &
                t[[genre_movie_weight_cols[i]]] != 0,
               t[[genre_user_bias_cols[i]]],
               0)
    }

    t$pred
  }

  model
}

get_performance_metrics(ext_edx, validation, LinearLikeGenreBiasBasedModel)

#---
model <- LinearLikeGenreBiasBasedModel(ext_edx)
pred <- model$predict(edx)
RMSE(edx$rating, pred)
mean(edx$rating == pred2stars(edx$timestamp, pred))
pred <- model$predict(validation)
RMSE(validation$rating, pred)
mean(validation$rating == pred2stars(validation$timestamp, pred))

