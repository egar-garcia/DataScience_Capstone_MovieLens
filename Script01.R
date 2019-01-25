library(lubridate)

edx %>%
  ggplot() +
  geom_histogram(aes(x = rating), binwidth = 0.25)

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


#' This object-constructor function is used to generate a metamodel 
#' that contains two models,
#' one model fitted for data before the startpoint when half stars were allowed
#' for the ratings, and the other one fitted for data on or after that.
#' The predictions are performed by choosing the appropriate model according to the 
#' data's timestamp.
#' 
#' @param dataset The dataset used to fit both models,
#'    it should contain a column called 'timestamp'
#' @param base_model_generator The function used to generate the base models,
#'    it should receive a dataset to fit the model and have a prediction function
#' @return The created metamodel
PartitionedtModel <- function(dataset, base_model_generator) {
  partitioned_model <- list()

  # Spliting the dataset in 2,
  # one set for data before the timestamp to do the partition,
  dataset1 <- dataset %>% filter(timestamp < half_stars_startpoint)
  # and the other one for the data on or after the timestampt to do the partition
  dataset2 <- dataset %>% filter(timestamp >= half_stars_startpoint)

  # Generation a model for each dataset
  partitioned_model$model1 <- base_model_generator(dataset1)
  partitioned_model$model2 <- base_model_generator(dataset2)

  #' Performs a prediction with the combined fitted models
  #' @param s The dataset used to perform the prediction of
  #' @return A vector containing the prediction for each row of the dataset
  partitioned_model$predict <- function(s) {
    # Performing the predictions on the whole dataset for each one of the models
    pred1 <- partitioned_model$model1$predict(s)
    pred2 <- partitioned_model$model2$predict(s)

    # Selecting the prediction to use according to the data's timestamp,
    # if a prediction is missing the prediction for the other model is used
    s %>%
      mutate(pred1 = pred1, pred2 = pred2) %>%
      mutate(pred = ifelse(timestamp < half_stars_startpoint,
                           ifelse(!is.na(pred1), pred1, pred2),
                           ifelse(!is.na(pred2), pred2, pred1))) %>%
      .$pred
  }
  
  partitioned_model
}


# Converts a prediction whic is a floating point number to a one used to 
# represent ratings given by stars,
# i.e. {1, 2, 3, 4, 5} if the timestamp is before the half start startpoint 
# or {1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5} if the timestamp is on or after
pred2stars <- function(timestamp, pred) {
  # Rounds the prediction either to be full stars or having a half star
  # according to the timestamp
  rounded_pred <- ifelse(timestamp < half_stars_startpoint,
                         round(pred),
                         round(pred * 2)/2)

  # Making sure the rating is in the range of 1 to 5  
  min(max(rounded_pred, 1), 5)
}


#' This object-constructor function is used to generate a model
#' that always return as prediction the average of the rating in the
#' given dataset used to fit the model
#' @param dataset The dataset used to fit the model
#' @return The model
SimpleAvgModel <- function(dataset) {
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

pred <- SimpleAvgModel(edx)$predict(edx)
RMSE(edx$rating, pred)
# 1.060331
mean(edx$rating == pred2stars(edx$timestamp, pred))

edx1 <- edx %>% filter(timestamp < half_stars_startpoint)
RMSE(edx1$rating, SimpleAvgModel(edx1)$predict(edx1))
# 1.086534

edx2 <- edx %>% filter(timestamp >= half_stars_startpoint)
RMSE(edx2$rating, SimpleAvgModel(edx2)$predict(edx2))
# 1.02891


RMSE(edx$rating, PartitionedtModel(edx, SimpleAvgModel)$predict(edx))
# 1.059362

round_ratings <- function(s, ratings) {
  ifelse(s$timestamp < partition_timestamp, round(ratings), round(ratings * 2)/2)
}




