#' ROC Curves Plot
#'
#' @param ModelObject An object obtained from TreeModels() or TreeModelsAllSteps() functions.
#' @param testdata A testing dataset.
#' @param reflevel A character string with the quoted reference level of outcome.
#'
#' @return This function returns a plot with ROC curves for the selected tree-based models (i.e., decision tree, random forest, or gradient boosting).
#' @import ggplot2
#' @import graphics
#' @importFrom dplyr %>%
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "gbm","rf"),checkprogress = TRUE)
#'
#' RocPlot(ModelObject = ensemblist$ModelObject, testdata = testing, reflevel = "incorrect")
#' }
RocPlot <- function(ModelObject, testdata, reflevel) {
  cv_dt_pred_class <- predict(ModelObject$rpart, testdata, type = "prob")[reflevel]
  cv_rf_pred_class <- predict(ModelObject$ranger, testdata, type = "prob")[reflevel]
  cv_gbm_pred_class <- predict(ModelObject$gbm, testdata, type = "prob")[reflevel]

  perf1 <- ROCR::prediction(cv_dt_pred_class, testdata$outcome) %>%
    ROCR::performance(measure = "tpr", x.measure = "fpr")
  perf2 <- ROCR::prediction(cv_rf_pred_class, testdata$outcome) %>%
    ROCR::performance(measure = "tpr", x.measure = "fpr")
  perf3 <- ROCR::prediction(cv_gbm_pred_class, testdata$outcome) %>%
    ROCR::performance(measure = "tpr", x.measure = "fpr")

  plot(perf1, col = "black", main = "ROC Curves for Tree-based Models")
  plot(perf2, add = TRUE, col = "blue")
  plot(perf3, add = TRUE, col = "red")
  legend(0.7, 0.3,
    legend = c("Decision Tree", "Random Forest", "Gradient Boosting"),
    col = c("black", "blue", "red"), lty = 1, cex = 0.5
  )
}
