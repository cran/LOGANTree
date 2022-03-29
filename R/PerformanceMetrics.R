#' Report table with the performance metrics for tree-based learning methods
#'
#' @param testdata A test dataset that contains the study’s features and the outcome variable.
#' @param DT A fitted decision tree model object
#' @param RF A fitted random forest model object
#' @param GBM A fitted gradient boosting model object
#' @param outcome  A factor variable with the outcome levels.
#' @param reflevel A character string with the quoted reference level of outcome.
#' @return This function returns a \code{data.frame} with a table that compares five performance metrics from different tree-based machine learning methods. The metrics are: Accuracy, Kappa, Sensitivity, Specificity, and Precision. The results are derived from the confusionMatrix function from the caret package.
#' @importFrom caret confusionMatrix
#' @importFrom stats predict
#' @importFrom stats relevel
#'
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "rf","gbm"),checkprogress = TRUE)
#'
#' PerformanceMetrics(testdata = testing, RF = ensemblist$ModelObject$ranger,
#' outcome = "outcome", reflevel = "correct")
#'
#' PerformanceMetrics(testdata = testing, RF = ensemblist$ModelObject$ranger,
#' GBM = ensemblist$ModelObject$gbm,
#' outcome = "outcome", reflevel = "correct")
#'
#' PerformanceMetrics(testdata = testing, DT = ensemblist$ModelObject$rpart,
#' RF = ensemblist$ModelObject$ranger, GBM = ensemblist$ModelObject$gbm,
#' outcome = "outcome", reflevel = "correct")
#' }
PerformanceMetrics <- function(testdata, DT = NULL, RF = NULL, GBM = NULL, outcome, reflevel) {
  if (!is.null(DT)) {
    pred_class_dt <- predict(DT, testdata)
    cm.dt <- caret::confusionMatrix(
      data = relevel(pred_class_dt, ref = reflevel),
      reference = relevel(testdata[, outcome], ref = reflevel)
    )
    cmDT <- as.data.frame(c(cm.dt$overall[c("Accuracy", "Kappa")], cm.dt$byClass[c("Sensitivity", "Specificity", "Precision")]))
    colnames(cmDT) <- "DT"
    cmDT
  }

  if (!is.null(RF)) {
    pred_class_rf <- predict(RF, testdata)
    cm.rf <- caret::confusionMatrix(
      data = relevel(pred_class_rf, ref = reflevel),
      reference = relevel(testdata[, outcome], ref = reflevel)
    )
    cmRF <- as.data.frame(c(cm.rf$overall[c("Accuracy", "Kappa")], cm.rf$byClass[c("Sensitivity", "Specificity", "Precision")]))
    colnames(cmRF) <- "RF"
    cmRF
  }

  if (!is.null(GBM)) {
    pred_class_gbm <- predict(GBM, testdata)
    cm.gbm <- caret::confusionMatrix(
      data = relevel(pred_class_gbm, ref = reflevel),
      reference = relevel(testdata[, outcome], ref = reflevel)
    )
    cmGBM <- as.data.frame(c(cm.gbm$overall[c("Accuracy", "Kappa")], cm.gbm$byClass[c("Sensitivity", "Specificity", "Precision")]))
    colnames(cmGBM) <- "GBM"
    cmGBM
  }

  if (is.null(DT) & is.null(RF)) {
    return(cmGBM)
  }

  if (is.null(GBM) & is.null(RF)) {
    return(cmDT)
  }

  if (is.null(DT) & is.null(GBM)) {
    return(cmRF)
  }
  if (is.null(DT)) {
    dtnull <- cbind(cmRF, cmGBM)
    return(dtnull)
  }
  if (is.null(RF)) {
    rfnull <- cbind(cmDT, cmGBM)
    return(rfnull)
  }
  if (is.null(GBM)) {
    gbmnull <- cbind(cmDT, cmRF)
    return(gbmnull)
  } else {
    alljoin <- cbind(cmDT, cmRF, cmGBM)
    return(alljoin)
  }
}
