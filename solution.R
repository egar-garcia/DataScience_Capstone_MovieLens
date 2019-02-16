if(!require(recosystem))
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

#' This object-constructor function is used to generate a model
#' of the form:
#'   r_u,m ~ mu + b_m + b_u + sum {k = 1..K} (p_u,k * q_m_k)
#' where:
#'  - 'r_u,m' is the rating given by an user 'u' to a movie 'm'
#'  - 'mu' is the average of all the observed ratings
#'  - 'b_m' is the movie effect (movie bias) of a movie 'm'
#'  - 'b_u' is the user effect (user bias) of an user 'u'
#'  - 'p_u,k' is the amount of the factor 'k' that the user 'u' has
#'  - 'q_m,k' is the amount of the factor 'k' that the movie 'm' has
#'  - 'K' is the number of (latent) factors
#'
#' @param dataset The dataset used to fit the model
#' @return The model
ResidualsMatrixFactorizationModel <- function(dataset) {
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

  #' The prediction function, it retrieves as prediction:
  #'   mu + b_m + b_u + sum {k = 1..K} (p_u,k * q_m_k)
  #' where:
  #'  - 'mu' is the average of all the observed ratings during training
  #'  - 'b_m' is the movie effect (movie bias) observed during training for a movie 'm'
  #'  -  b_u' is the user effect (user bias) observed during training for an user 'u'
  #'  - 'p_u,k' is the quantity of the factor 'k' that the user 'u' has,
  #'            calculated by matrix factorization
  #'  - 'q_m,k' is the quantity of the factor 'k' that the movie 'm' has,
  #'            calculated by matrix factorization
  #'  - 'K' is the number of (latent) factors, in this case 30
  #'
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction
  model$predict <- function(s) {
    # Dataset used to do the prediction, based on the movie and user
    pred_data <- data_memory(user_index = s$userId, item_index = s$movieId, index1 = T)
    
    # Predicting the residuals
    pred_residuals <- model$recommender$predict(pred_data, out_memory())
    
    # Prediction based in movie and user effects, plus the prediction of the residual
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

set.seed(0)
# Fitting the model with the 'edx' set
model <- ResidualsMatrixFactorizationModel(edx)

# Performing a prediction agains the 'validation' set
validation_pred <- model$predict(validation)

# Calculating the RMSE agains the ground truth of the 'validation' set
rmse <- RMSE(validation_pred, validation$rating)

# Displaying the RMSE
rmse