#' Data Partition
#'
#' @param data A \code{data.frame} that contains the study’s features and the outcome variable.
#' @param outcome A character string with the name of the outcome variable from the data.
#' @param proportion A numeric value for the proportion of data to be put into model training. Default is set to 0.7.
#' @param seed A numeric value for set.seed. It is set to be 2022 by default.
#'
#' @return This function returns a list with training and testing data sets using a stratified selection by the outcome variable as performed by the createDataPartition function from the caret package.
#' @export
#'
#' @examples
#' dp <- DataPartition(data = cp025q01.wgt, outcome = "outcome")


DataPartition <- function(data = NULL,
                          outcome = NULL,
                          proportion = .7,
                          seed = 2022) {
  # Data partition
  set.seed(seed)
  inS <- caret::createDataPartition(data[, outcome], p = proportion, list = FALSE)
  training <- data[inS, ]
  testing <- data[-inS, ]
  dsl <- list(training, testing)
  names(dsl) <- c("training_set", "testing_set")
  return(dsl)
}
