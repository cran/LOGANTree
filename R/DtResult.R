#' Decision Tree Result in Text View and Plot
#'
#' @param ModelObject A fitted model object from TreeModels() or TreeModelsAllSteps() functions.
#'
#' @return This function returns the structure of the decision tree final model as a text view, and a plot of the rpart model object as displayed by the rpart.plot package.
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = "dt",checkprogress = TRUE)
#'
#' DtResult(ensemblist$ModelObject)
#' }
DtResult <- function(ModelObject) {
  pl <- rpart.plot::rpart.plot(ModelObject$rpart$finalModel)
  text <- ModelObject$rpart$finalModel
  # ls <- list(pl,text)
  return(text)
}
