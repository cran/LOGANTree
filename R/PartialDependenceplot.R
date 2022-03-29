#' Partial Dependence Plot
#'
#' @param data A \code{data.frame} that contains the study’s features and the outcome.
#' @param FeatureNames A vector with the names of features to plot.
#' @param FittedModelObject A fitted model object.
#' @param j A numerical value that indicates the size of the equally spaced values for the feature of interest.
#'
#' @return This function returns a plot where X axis presents the values for each feature and Y axis illustrates the predicted proportion of correct answer to the item.
#' @import ggplot2
#' @import stats
#' @importFrom dplyr left_join
#' @export
#'
#' @examples
#' \donttest{
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt","rf"),checkprogress = TRUE)
#'
#' PartialDependencePlot(data = training,
#' FeatureNames = colnames(training[-c(4,14)]),
#' FittedModelObject = ensemblist$ModelObject$rpart, j = 30)
#'
#' PartialDependencePlot(data = training,
#' FeatureNames = colnames(training[-c(4,14)]),
#' FittedModelObject = ensemblist$ModelObject$ranger, j = 20)
#' }
PartialDependencePlot <- function(data = NULL, FeatureNames = NULL, FittedModelObject = NULL, j = 20) {
  # create a matrix for j equal spaces
  grid <- data.frame(matrix(NA, nrow = j, ncol = length(FeatureNames)))
  colnames(grid) <- FeatureNames

  for (m in seq_len(length(FeatureNames))) {
    grid[m] <- round(seq(min(data[FeatureNames[m]]), max(data[FeatureNames[m]]), length.out = j), 2)
  }

  # create a list for j copies of the original data set for each feature
  replist <- as.list(rep(NA, length(FeatureNames)))
  for (k in seq_len(length(FeatureNames))) {
    replist[[k]] <- as.list(rep(NA, j))
  }
  names(replist) <- FeatureNames

  for (i in 1:j) {
    for (k in seq_len(length(FeatureNames))) {
      replist[[k]][[i]] <- data
      replist[[k]][[i]][, FeatureNames[k]] <- grid[i, k]
    }
  }


  # create a list for the average of each copy for each feature
  avlist <- as.list(rep(NA, length(FeatureNames)))
  for (k in seq_len(length(FeatureNames))) {
    avlist[[k]] <- as.list(rep(NA, j))
  }
  names(avlist) <- FeatureNames

  for (i in 1:j) {
    for (k in seq_len(length(FeatureNames))) {
      avlist[[k]][[i]] <- as.vector(as.numeric(stats::predict(FittedModelObject, replist[[k]][[i]])))
      # code the outcome variable into a binary variable
   #   if (length(unique(avlist[[k]][[i]])) == 2) {
  #      avlist[[k]][[i]] <- ifelse(avlist[[k]][[i]] == 2, 1, 0)
  #    }
      avlist[[k]][[i]] <- avlist[[k]][[i]]-1
    }
  }


  jmeans <- data.frame(matrix(NA, nrow = j, ncol = length(FeatureNames)))
  colnames(jmeans) <- FeatureNames

  for (k in seq_len(length(FeatureNames))) {
    jmeans[, k] <- unlist(lapply(avlist[[k]], mean))
  }


  for (k in seq_len(length(FeatureNames))) {
    plot(grid[, k], jmeans[, k], type = "l", xlab = FeatureNames[k], ylab = "Mean(Y)", main = "Partial Dependence Plot")
  }


  LongJmeans <- reshape(jmeans, direction = "long", idvar = "j_n", varying = list(colnames(jmeans)))
  names(LongJmeans)[1:2] <- c("Feature", "MeanY")
  LongGrid <- reshape(grid, direction = "long", idvar = "j_n", varying = list(colnames(grid)))
  names(LongGrid)[1:2] <- c("Feature", "x_axis")

  Long <- dplyr::left_join(LongJmeans, LongGrid, by = c("Feature", "j_n"))
  Long$FeatureName <- rep(FeatureNames, each = j)

  p <- ggplot(Long, aes(x_axis, MeanY)) +
    geom_line() +
    facet_wrap(~FeatureName, scales = "free") +
    labs(title = paste("Partial Dependence Plot - ",FittedModelObject$method), x = "Feature(X)", y = "Predicted proportion of correct answer(Y)") +
    theme_bw()
  return(p)
}
