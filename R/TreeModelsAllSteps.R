#' Data Partition and Tree-based Model Training
#'
#' @param data A \code{data.frame} that contains the study’s features and the outcome variable.
#' Please name the outcome variable as "perf".
#' @param proportion A numeric value for the proportion of data to be put into model training. Default is set to 0.7.
#' @param seed A numeric value for set.seed. It is set to be 2022 by default.
#' @param methodlist A list of the tree-based methods to model. The default is methodlist = c("dt", "rf", "gbm").
#' @param iternumber A numeric value for the number of resampling iterations/number of folds for the  cross-validation scheme.
#' @param dt.gridsearch A \code{data.frame} of the tuning grid, which allows for specifying parameters for decision tree model.
#' @param rf.gridsearch A \code{data.frame} of the tuning grid, which allows for specifying parameters for random forest model.
#' @param gbm.gridsearch A \code{data.frame} of the tuning grid, which allows for specifying parameters for gradient boosting model.
#' @param checkprogress Logical. Print the modeling progress if it is TRUE. The default is FALSE.
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#'
#' @details This function performs all the steps of a predictive analysis. First, the data is partitioned in the training and testing datasets using a stratified selection by the outcome variable as performed by the createDataPartition function from the caret package. Then, the selected classifiers are used for modeling the training dataset under a cross-validation scheme. Users have the possibility to choose which model they want to compare by specifying it on the \code{methodlist} argument. The caretEnsemble package is used in the modeling process to ensure that all models follow the same resampling procedures. ROC is used to select the optimal model for each tree-based method using the largest value. Finally, a summary report is displayed.
#'
#' @return This function returns three lists:
#' @return DataPartition The partitioned datasets: training (cv_train) and testing (cv_test).
#' @return ModelObject An object with results from selected models
#' @return SummaryReport A \code{data.frame} with the summary of model parameters. The summary report is shown automatically in the output.
#' @export
#'
#' @examples
#' \donttest{
#' cp025q01.wgt <- cp025q01.wgt[,-14]
#' colnames(cp025q01.wgt)[14] <- "perf"
#'
#' ensemblist <- TreeModelsAllSteps(data = cp025q01.wgt,
#' checkprogress = TRUE)
#'
#' ensemblist <- TreeModelsAllSteps(data = cp025q01.wgt,
#' methodlist = c("dt", "gbm"), checkprogress = TRUE)
#'
#' ensemblist <- TreeModelsAllSteps(data = cp025q01.wgt,
#' methodlist = c("rf"),
#' rf.gridsearch = data.frame(mtry = 2, splitrule = "gini", min.node.size = 1),
#' checkprogress = TRUE)
#' }
TreeModelsAllSteps <- function(data = NULL, proportion = .7, seed = 2022,
                      methodlist = c("dt", "rf", "gbm"),
                      iternumber = 10,
                      dt.gridsearch = NULL, rf.gridsearch = NULL, gbm.gridsearch = NULL,
                      checkprogress = FALSE) {
  # Data partition
  set.seed(seed)
  inS <- caret::createDataPartition(data[, "perf"], p = proportion, list = FALSE)
  training <- data[inS, ]
  testing <- data[-inS, ]


  # progress bar
  pb <- utils::txtProgressBar(
    min = 0, # Minimum value of the progress bar
    max = 1, # Maximum value of the progress bar
    style = 3, # Progress bar style (also available style = 1 and style = 2)
    width = 50, # Progress bar width. Defaults to getOption("width")
    char = "="
  ) # Character used to create the bar
  for (i in 1:1) {
    # Modeling
    training$perf <- as.factor(training$perf)
    set.seed(seed)
    my_control <- caret::trainControl(
      method = "cv",
      number = iternumber,
      savePredictions = "final",
      classProbs = TRUE,
      summaryFunction = caret::twoClassSummary,
      index = caret::createFolds(training$perf, k = iternumber),
      verboseIter = checkprogress
    )

    tuneListDf <- list(
      rpart = caretEnsemble::caretModelSpec(method = "rpart"),
      ranger = caretEnsemble::caretModelSpec(method = "ranger", importance = "impurity"),
      gbm = caretEnsemble::caretModelSpec(method = "gbm", verbose = FALSE)
    )

    tuneListDfTg <- list(
      rpart = caretEnsemble::caretModelSpec(method = "rpart", tuneGrid = dt.gridsearch),
      ranger = caretEnsemble::caretModelSpec(method = "ranger", tuneGrid = rf.gridsearch),
      gbm = caretEnsemble::caretModelSpec(method = "gbm", verbose = FALSE, tuneGrid = gbm.gridsearch)
    )

    if (is.null(dt.gridsearch) & is.null(rf.gridsearch)) {
      if (is.null(gbm.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = tuneListDf
        )
      } else {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDf$rpart, tuneListDf$ranger, tuneListDfTg$gbm)
        )
      }
    }
    if (is.null(dt.gridsearch) & is.null(gbm.gridsearch)) {
      if (!is.null(rf.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDf$rpart, tuneListDf$gbm, tuneListDfTg$ranger)
        )
      }
    }
    if (is.null(rf.gridsearch) & is.null(gbm.gridsearch)) {
      if (!is.null(dt.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDf$ranger, tuneListDf$gbm, tuneListDfTg$rpart)
        )
      }
    }

    if (!is.null(dt.gridsearch) & !is.null(rf.gridsearch)) {
      if (is.null(gbm.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDfTg$rpart, tuneListDfTg$ranger, tuneListDf$gbm)
        )
      } else {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = tuneListDfTg
        )
      }
    }
    if (!is.null(dt.gridsearch) & !is.null(gbm.gridsearch)) {
      if (is.null(rf.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDfTg$rpart, tuneListDfTg$gbm, tuneListDf$ranger)
        )
      }
    }
    if (!is.null(rf.gridsearch) & !is.null(gbm.gridsearch)) {
      if (is.null(dt.gridsearch)) {
        enslist <- caretEnsemble::caretList(
          perf ~ .,
          metric = "ROC",
          data = training,
          trControl = my_control,
          tuneList = list(tuneListDfTg$ranger, tuneListDfTg$gbm, tuneListDf$rpart)
        )
      }
    }

    utils::setTxtProgressBar(pb, i)
  }

  close(pb) # Close the connection

  # Summary report
  sr <- as.data.frame(cbind(
    c(
      "Summary of Model Parameters", "", "Decision Tree", "Method", "Split rule", "Number of resampling iterations", "Final value for complexity parameter", "",
      "Random Forest", "Method", "Split rule", "Number of  resampling iterations", "Number of variables tried at each split (mtry)", "Minimum value of the node size", "Number of trees", "OOB prediction error", "",
      "Gradient boosting", "Method","Distribution", "Number of resampling iterations", "Learning rate", "Minimum value of the node size", "Tree depth", "Number of trees"
    ),
    c(
      "", "", "", paste(enslist$rpart$modelInfo$label, " by rpart", sep = ""), "gini", iternumber, round(enslist$rpart$bestTune$cp, 4), "",
      "", paste(enslist$ranger$modelInfo$label, " by ranger", sep = ""), as.character(enslist$ranger$bestTune$splitrule), iternumber, enslist$ranger$bestTune$mtry, enslist$ranger$bestTune$min.node.size, enslist$ranger$finalModel$num.trees, round(enslist$ranger$finalModel$prediction.error, 4), "",
      "", paste(enslist$gbm$modelInfo$label, " by gbm", sep = ""), "bernoulli", iternumber, enslist$gbm$bestTune$shrinkage, enslist$gbm$bestTune$n.minobsinnode, enslist$gbm$bestTune$interaction.depth, enslist$gbm$finalModel$n.trees
    )
  ))

  SameElements <- function(a, b) {
    return(identical(sort(a), sort(b)))
  }
  if (SameElements(methodlist, c("dt", "rf", "gbm"))) {
    sr <- sr
  }
  if (SameElements(methodlist, c("dt", "rf"))) {
    sr <- sr[1:16, ]
  }
  if (SameElements(methodlist, c("dt", "gbm"))) {
    sr <- sr[c(1:8, 18:25), ]
  }
  if (SameElements(methodlist, c("rf", "gbm"))) {
    sr <- sr[c(1:2, 9:25), ]
  }
  if (identical(methodlist, "gbm")) {
    sr <- sr[c(1:2, 18:25), ]
  }
  if (identical(methodlist, "rf")) {
    sr <- sr[c(1:2, 9:16), ]
  }
  if (identical(methodlist, "dt")) {
    sr <- sr[c(1:7), ]
  }

  names(sr) <- NULL

  DataPartition <- list(training, testing)
  names(DataPartition) <- c("training_set", "testing_set")
  ModelObject <- enslist
  SummaryRport <- list(sr)
  names(SummaryRport) <- "Summary of Model Parameters"

  rl <- list(DataPartition, ModelObject, SummaryRport)
  names(rl) <- c("DataPartition", "ModelObject", "SummaryReport")

  # output: 1.model objects,  2.training and testing data set,  3.summary report
  print(sr, row.names = FALSE, right = FALSE)
  return(rl)
}
