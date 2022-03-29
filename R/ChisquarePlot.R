#' Plot for Chi-square Statistics
#'
#' @param trainingdata A data set used for training
#' @param nfeatureNames A vector of feature names that will be used for computing chi-square statistics
#' @param outcome A character string with the name of the binary outcome variable.
#' @param level A numerical value indicating the number of categories that the outcome contains
#' @param ModelObject A model object containing tree-based models
#'
#' @return This function returns a barplot of scaled chi-square statistics for the study’s features. These measures were computed as described by He & von Davier (2015).
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "gbm"),checkprogress = TRUE)
#'
#' ChiSquarePlot(trainingdata = training,
#' nfeatureNames = colnames(training[,7:13]),
#' outcome = "perf", level = 2, ModelObject = ensemblist$ModelObject)
#' }
#' @references
#' He, Q., & von Davier, M. (2015). Identifying feature sequences from process data in problem-solving items with N-grams. In Quantitative Psychology Research: The 79th Annual Meeting of the Psychometric Society (pp. 173–190). Madison, Wisconsin: Springer International Publishing.
ChiSquarePlot <- function(trainingdata = NULL,
                          nfeatureNames = NULL,
                          outcome = NULL,
                          level = NULL,
                          ModelObject = NULL) {
  if (!is.null(ModelObject$rpart)) {
    # Decision tree
    alist.dt <- list(rep(NA, 10))
    ind_dt <- ModelObject$rpart$control$index
    for (i in 1:10) {
      alist.dt[[i]] <- ComputeChisquared(data = trainingdata[ind_dt[[i]], c(colnames(trainingdata)[colnames(trainingdata) %in% nfeatureNames], outcome)], outcome, level = 2)
    }


    multiJoin_dt <- Reduce(
      function(x, y, ...) dplyr::left_join(x, y, by = "Feature", ...),
      alist.dt
    )

    colnames(multiJoin_dt)[2:ncol(multiJoin_dt)] <- names(table(ModelObject$rpart$pred$Resample))


    for (i in 2:11) {
      multiJoin_dt[[i]] <- as.numeric(unlist(multiJoin_dt[i]))
    }
    multiJoin_dt$CvAverageChisq <- rowMeans(multiJoin_dt[2:ncol(multiJoin_dt)])

    dtchi1 <- multiJoin_dt[, c(1, 12)]
    dtchi <- dtchi1
    dtchi <- dtchi[order(dtchi$CvAverageChisq, decreasing = TRUE), ]

    # scale the results
    dtnan <- dtchi[is.na(dtchi$CvAverageChisq),"Feature"]
    dtchi <- dtchi[!dtchi$Feature%in%dtnan,]
    dtchi$ScaledCvAverageChisq <- (dtchi$CvAverageChisq - min(dtchi$CvAverageChisq)) / (max(dtchi$CvAverageChisq) - min(dtchi$CvAverageChisq)) * 100
    dtchi$Feature <- factor(dtchi$Feature, levels = dtchi[order(dtchi$ScaledCvAverageChisq, decreasing = T), 1])
    if (length(dtchi$Feature) > 25) {
      dtchi <- dtchi[1:25, ]
    }
    dp <- ggplot2::ggplot(dtchi, aes(x = ScaledCvAverageChisq, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_classic() +
      labs(y = paste("Features n = ", length(dtchi1$Feature), sep = ""))
    return(dp)
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
    rfchi1 <- multiJoin.rf[, c(1, 12)]
    rfchi <- rfchi1
    rfchi <- rfchi[order(rfchi$CvAverageChisq, decreasing = TRUE), ]
    # scale the reults
    rfnan <- rfchi[is.na(rfchi$CvAverageChisq),"Feature"]
    rfchi <- rfchi[!rfchi$Feature%in%rfnan,]
    rfchi$ScaledCvAverageChisq <- (rfchi$CvAverageChisq - min(rfchi$CvAverageChisq)) / (max(rfchi$CvAverageChisq) - min(rfchi$CvAverageChisq)) * 100
    rfchi$Feature <- factor(rfchi$Feature, levels = rfchi[order(rfchi$ScaledCvAverageChisq, decreasing = T), 1])
    if (length(rfchi$Feature) > 25) {
      rfchi <- rfchi[1:25, ]
    }
    rp <- ggplot2::ggplot(rfchi, aes(x = ScaledCvAverageChisq, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_classic() +
      labs(y = paste("Features n = ", length(rfchi1$Feature), sep = ""))
    return(rp)
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

    gbmchi1 <- multiJoin.gbm[, c(1, 12)]
    gbmchi <- gbmchi1
    gbmchi <- gbmchi[order(gbmchi$CvAverageChisq, decreasing = TRUE), ]

    # scale the results
    gbmnan <- gbmchi[is.na(gbmchi$CvAverageChisq),"Feature"]
    gbmchi <- gbmchi[!gbmchi$Feature%in%gbmnan,]
    gbmchi$ScaledCvAverageChisq <- (gbmchi$CvAverageChisq - min(gbmchi$CvAverageChisq)) / (max(gbmchi$CvAverageChisq) - min(gbmchi$CvAverageChisq)) * 100

    gbmchi$Feature <- factor(gbmchi$Feature, levels = gbmchi[order(gbmchi$ScaledCvAverageChisq, decreasing = T), 1])
    if (length(gbmchi$Feature) > 25) {
      gbmchi <- gbmchi[1:25, ]
    }
    gp <- ggplot2::ggplot(gbmchi, aes(x = ScaledCvAverageChisq, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_classic() +
      labs(y = paste("Features n = ", length(dtchi$Feature), sep = ""))
    return(gp)
  }
}
