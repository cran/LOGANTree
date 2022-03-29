#' Table comparing the feature importance for tree-based learning methods.
#'
#' @param DT A fitted decision tree model object
#' @param RF A fitted random forest model object
#' @param GBM A fitted gradient boosting model object
#'
#' @return This function returns a data frame that compares the feature importance from different tree-based machine learning methods. These measures are computed via the caret package.
#' @importFrom dplyr full_join
#' @export
#'
#' @examples
#' \donttest{
#' library(gbm)
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "rf","gbm"),checkprogress = TRUE)
#'
#' VariableImportanceTable(DT = ensemblist$ModelObject$rpart,
#' RF = ensemblist$ModelObject$ranger,GBM = ensemblist$ModelObject$gbm)
#'
#' VariableImportanceTable(DT = ensemblist$ModelObject$rpart,
#' RF = ensemblist$ModelObject$ranger)
#'
#' VariableImportanceTable(DT = ensemblist$ModelObject$rpart)
#' }
VariableImportanceTable <- function(DT = NULL, RF = NULL, GBM = NULL) {
  if (!is.null(DT)) {
    # Decision tree
    dt <- tibble::rownames_to_column(caret::varImp(DT, scale = FALSE)$importance, "Feature")
    dt <- dt[order(dt$Overall, decreasing = TRUE), ]
    colnames(dt)[2] <- "VariableImportance.DT"
    dt$VariableImportance.DT <- round(dt$VariableImportance.DT, 4)
    dt$RankDT <- seq_len(nrow(dt))
  }


  if (!is.null(RF)) {
    # Random forest
    rf <- tibble::rownames_to_column(caret::varImp(RF, scale = FALSE)$importance, "Feature")
    colnames(rf)[2] <- "VariableImportance.RF"
    rf$VariableImportance.RF <- round(rf$VariableImportance.RF, 4)
    rf$RankRF <- rank(-rf$VariableImportance.RF)
  }


  if (!is.null(GBM)) {
    # Gradient boosting machine
    gbm <- tibble::rownames_to_column(caret::varImp(GBM, scale = FALSE)$importance, "Feature")
    colnames(gbm)[2] <- "VariableImportance.GBM"
    gbm$VariableImportance.GBM <- round(gbm$VariableImportance.GBM, 4)
    gbm$RankGBM <- rank(-gbm$VariableImportance.GBM, ties.method = "first")
  }

  # When DT and RF are missing
  if (is.null(DT) & is.null(RF)) {
    return(gbm)
  }

  # When GBM and RF are missing
  if (is.null(GBM) & is.null(RF)) {
    # Showing variable importance only
    return(dt)
  }

  # When DT and GBM are missing
  if (is.null(DT) & is.null(GBM)) {
    # Showing variable importance only
    return(rf)
  }

  # When DT is missing
  if (is.null(DT)) {
    rfgbm <- dplyr::full_join(rf, gbm, by = "Feature")
    return(rfgbm)
  }
  # When RF is missing
  if (is.null(RF)) {
    rfgbm <- dplyr::full_join(dt, gbm, by = "Feature")
    return(dtgbm)
  }

  # When GBM is missing
  if (is.null(GBM)) {
    dtrf <- dplyr::full_join(dt, rf, by = "Feature")
    return(dtrf)
  } else {
    all.vi <- dplyr::full_join(dplyr::full_join(dt, rf, by = "Feature"), gbm, by = "Feature")
    return(all.vi)
  }
}
