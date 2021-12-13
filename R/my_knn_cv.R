#' My K-Nearnest Neighbors Cross Validation
#'
#' Uses k-fold cross validation to perform k-nearest neighbors.
#'
#' @details IMPORTANT NOTE: K-Nearest Neighbors requires no NA values in the
#'   data. Therefore, this function removes NA values from the passed in
#'   data.frames. If the length of the vector output$class is less than the
#'   number of rows in the input 'train,' this may be the reason.
#'
#' @param train Data.frame object where columns are the variables used to
#'   predict classification
#' @param cl Data.frame object where ncols == 1. This column is the true
#'   classifications of the nodes
#' @param k_nn The number of nearest neighbors to look at for nearest neighbors
#' @param k_cv The number of folds to use in cross validation
#'
#' @return A List containing:
#'   * \code{class} - vector with all classes
#'   * \code{cv_err} - numeric with the average CV misclassification error
#'
#' @examples
#' my_knn_cv(train = my_penguins[,c("bill_length_mm",
#'                                  "bill_depth_mm",
#'                                  "flipper_length_mm",
#'                                  "body_mass_g")],
#'           cl = my_penguins[,c("species")],
#'           k_nn = 1,
#'           k_cv = 5)
#'
#' @import dplyr
#' @import class
#' @importFrom rlang .data
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  # test to make sure input variables meet conditions
  if (!is.numeric(k_nn)) {
    stop("Input variable 'k_nn' must be of type numeric.")
  }
  if (!is.numeric(k_cv)) {
    stop("Input variable 'k_cv' must be of type numeric.")
  }
  if (!is.data.frame(train)) {
    stop("Input variable 'train' must be of type data.frame.")
  }
  if (!is.data.frame(cl)) {
    stop("Input variable 'cl' must of of type data.frame.")
  }
  if (length(cl) != 1) {
    stop("Input variable 'cl' must have length of 1.")
  }

  # add index column to training data
  train <- train %>% mutate(index = c(1:nrow(train)))


  # remove na values from training and classification datasets

  ## find index where na values are

  train_na <- which(is.na(train), arr.ind=TRUE)
  cl_na <- which(is.na(cl), arr.ind=TRUE)

  ## combine indices for the rows with na vals to remove
  rows_to_remove <- c(unique(train_na[,1]), unique(cl_na[,1]))

  ## check if warning message is needed
  if (length(rows_to_remove) > 0) {
    warning("NA values were removed from the input 'train.' The length of the
             vector output$class will be the length of 'train' without the
             NA values.")
  }

  ## remove the na values from both the training and classification dataset
  train <- train[-c(rows_to_remove),]
  cl <- cl[-c(rows_to_remove),]

  # randomly assign 1:k folds to each penguin
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  train <- train %>% mutate(fold = fold)
  cl <- cl %>% mutate(fold = fold)

  misclass_rate <- numeric()
  all_preds <- c()
  pred_index <- numeric()

  # iterate from i = 1:k folds
  for (i in c(1:k_cv)) {
    # all data that isn't in the current fold is used as training
    # data in that fold is used as testing
    data_train <- train[train$fold != i,] %>% select(-fold)

    data_test <- train[train$fold == i,] %>% select(-fold)

    # grabs the true classifications for the training set

    train_cl <- cl[cl$fold != i,]$species

    # run knn on training data
    pred <- class::knn(train = (data_train %>% select(-.data$index)),
                       test = (data_test %>% select(-.data$index)), k = k_nn,
                       cl = train_cl)

    # do as.character to unfactor the classes
    all_preds <- c(all_preds, as.character(pred))
    pred_index <- c(pred_index, data_test$index)

    # grab classifications for the testing sets
    test_cl <- cl[cl$fold == i,]$species

    # compare predicted classifications to the testing classifications
    misclass_rate <- c(misclass_rate, mean(pred != test_cl))
  }

  class_df <- data.frame(preds = all_preds, index = pred_index)

  class_df <- class_df[order(class_df$index),]


  class <- class_df$preds
  cv_err <- mean(misclass_rate)
  return(list(class = class, cv_err = cv_err))
}
