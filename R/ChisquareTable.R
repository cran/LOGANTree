#' Chi-square Statistics Table
#'
#' @param trainingdata A data set used for training
#' @param nfeatureNames A vector of feature names that will be used for computing chi-square statistics
#' @param outcome A character string with the name of the binary outcome variable.
#' @param level A numerical value indicating the number of categories that the outcome contains
#' @param ModelObject A model object containing tree-based models
#'
#' @return This function returns a table with five columns. The chi-square statistics were computed as described by He & von Davier (2015).
#' @return Feature: Features names
#' @return CvAverageChisq: Average chisquare statistics computed from 10-fold cross validation samples
#' @return Rank.CvAverageChisq: Ordem of the feature importance from the CvAverageChisq measures#'
#' @return OverallChisq: chisquare scores computed from the whole training sample
#' @return Rank.OverallChisq: Ordem of the feature importance from the OverallChisq measures
#' @importFrom dplyr left_join
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "gbm"),checkprogress = TRUE)
#'
#' ChiSquareTable(trainingdata=training,
#' nfeatureNames=colnames(training[,7:13]),
#' outcome = "perf",level = 2, ModelObject = ensemblist$ModelObject)
#' }
#' @references
#' He, Q., & von Davier, M. (2015). Identifying feature sequences from process data in problem-solving items with N-grams. In Quantitative Psychology Research: The 79th Annual Meeting of the Psychometric Society (pp. 173–190). Madison, Wisconsin: Springer International Publishing.

ChiSquareTable <- function(trainingdata = NULL, nfeatureNames = NULL, outcome = NULL, level = NULL, ModelObject = NULL) {
  if (!is.null(ModelObject$rpart)) {
    # Decision tree
    alist.dt <- list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    ind.dt <- ModelObject$rpart$control$index
    for (i in 1:10) {
      alist.dt[[i]] <- ComputeChisquared(trainingdata[ind.dt[[i]], c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2)
    }


    multiJoin.dt <- Reduce(
      function(x, y, ...) dplyr::left_join(x, y, by = "Feature", ...),
      alist.dt
    )

    colnames(multiJoin.dt)[2:ncol(multiJoin.dt)] <- names(table(ModelObject$rpart$pred$Resample))


    for (i in 2:11) {
      multiJoin.dt[[i]] <- as.numeric(unlist(multiJoin.dt[i]))
    }
    multiJoin.dt$CvAverageChisq <- rowMeans(multiJoin.dt[2:ncol(multiJoin.dt)])

    multiJoin.dt <- dplyr::left_join(multiJoin.dt, ComputeChisquared(trainingdata[, c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2), by = "Feature")
    colnames(multiJoin.dt)[13] <- "OverallChisq"
    multiJoin.dt$OverallChisq <- as.numeric(multiJoin.dt$OverallChisq)
    multiJoin.dt$Rank.CvAverageChisq <- rank(-multiJoin.dt$CvAverageChisq)
    multiJoin.dt$Rank.OverallChisq <- rank(-multiJoin.dt$OverallChisq)
    multiJoin.dt <- multiJoin.dt[order(multiJoin.dt$Overall, decreasing = TRUE), ]

    return(multiJoin.dt[, c(1, 12, 14, 13, 15)])
  }


  if (!is.null(ModelObject$ranger)) {
    # Random forest
    alist.rf <- list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    ind.rf <- ModelObject$ranger$control$index
    for (i in 1:10) {
      alist.rf[[i]] <- ComputeChisquared(trainingdata[ind.rf[[i]], c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2)
    }


    multiJoin.rf <- Reduce(
      function(x, y, ...) dplyr::left_join(x, y, by = "Feature", ...),
      alist.rf
    )

    colnames(multiJoin.rf)[2:ncol(multiJoin.rf)] <- names(table(ModelObject$ranger$pred$Resample))


    for (i in 2:11) {
      multiJoin.rf[[i]] <- as.numeric(unlist(multiJoin.rf[i]))
    }
    multiJoin.rf$CvAverageChisq <- rowMeans(multiJoin.rf[2:ncol(multiJoin.rf)])

    multiJoin.rf <- dplyr::left_join(multiJoin.rf, ComputeChisquared(trainingdata[, c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2), by = "Feature")
    colnames(multiJoin.rf)[13] <- "OverallChisq"
    multiJoin.rf$OverallChisq <- as.numeric(multiJoin.rf$OverallChisq)

    multiJoin.rf$Rank.CvAverageChisq <- rank(-multiJoin.rf$CvAverageChisq)
    multiJoin.rf$Rank.OverallChisq <- rank(-multiJoin.rf$OverallChisq)

    multiJoin.rf <- multiJoin.rf[order(multiJoin.rf$Overall, decreasing = TRUE), ]
    return(multiJoin.rf[, c(1, 12, 14, 13, 15)])
  }


  if (!is.null(ModelObject$gbm)) {
    # Gradient boosting machine
    alist.gbm <- list(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    ind.gbm <- ModelObject$gbm$control$index
    for (i in 1:10) {
      alist.gbm[[i]] <- ComputeChisquared(trainingdata[ind.gbm[[i]], c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2)
    }


    multiJoin.gbm <- Reduce(
      function(x, y, ...) dplyr::left_join(x, y, by = "Feature", ...),
      alist.gbm
    )

    colnames(multiJoin.gbm)[2:ncol(multiJoin.gbm)] <- names(table(ModelObject$gbm$pred$Resample))


    for (i in 2:11) {
      multiJoin.gbm[[i]] <- as.numeric(unlist(multiJoin.gbm[i]))
    }
    multiJoin.gbm$CvAverageChisq <- rowMeans(multiJoin.gbm[2:ncol(multiJoin.gbm)])

    multiJoin.gbm <- left_join(multiJoin.gbm, ComputeChisquared(trainingdata[, c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2), by = "Feature")
    colnames(multiJoin.gbm)[13] <- "OverallChisq"
    multiJoin.gbm$OverallChisq <- as.numeric(multiJoin.gbm$OverallChisq)
    multiJoin.gbm$Rank.CvAverageChisq <- rank(-multiJoin.gbm$CvAverageChisq)
    multiJoin.gbm$Rank.OverallChisq <- rank(-multiJoin.gbm$OverallChisq)

    multiJoin.gbm <- multiJoin.gbm[order(multiJoin.gbm$Overall, decreasing = TRUE), ]
    return(multiJoin.gbm[, c(1, 12, 14, 13, 15)])
  }
}
