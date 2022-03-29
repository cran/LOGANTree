#' Barplot comparing the feature importance across different learning methods.
#'
#' @param DT A fitted decision tree model object
#' @param RF A fitted random forest model object
#' @param GBM A fitted gradient boosting model object
#' @return This function returns a barplot that compares the standardized feature importance across different tree-based machine learning methods. These measures are computed via the caret package.
#' @import ggplot2
#' @import stats
#' @importFrom dplyr left_join
#' @importFrom gbm relative.influence
#' @export
#'
#' @examples
#' \donttest{
#' library(gbm)
#' colnames(training)[14] <- "perf"
#' ensemblist <- TreeModels(traindata = training,
#' methodlist = c("dt", "rf","gbm"),checkprogress = TRUE)
#'
#' VariableImportancePlot(DT = ensemblist$ModelObject$rpart,
#' RF = ensemblist$ModelObject$ranger,GBM = ensemblist$ModelObject$gbm)
#'
#' VariableImportancePlot(RF = ensemblist$ModelObject$ranger,
#' GBM = ensemblist$ModelObject$gbm)
#'
#' VariableImportancePlot(DT = ensemblist$ModelObject$rpart)
#' }
VariableImportancePlot <- function(DT = NULL, RF = NULL, GBM = NULL) {
  if (!is.null(DT)) {
    dt1 <- tibble::rownames_to_column(caret::varImp(DT)$importance, "Variable")
    colnames(dt1)[2] <- "Importance.DT"
    dt <- dt1[order(dt1$Importance.DT, decreasing = TRUE), ]
    if (length(dt$Variable) > 25) {
      dt <- dt[1:25, ]
    }
  }
  if (!is.null(RF)) {
    rf1 <- tibble::rownames_to_column(caret::varImp(RF)$importance, "Variable")
    colnames(rf1)[2] <- "Importance.RF"
    rf <- rf1[order(rf1$Importance.RF, decreasing = TRUE), ]
    if (length(rf$Variable) > 25) {
      rf <- rf[1:25, ]
    }
  }
  if (!is.null(GBM)) {
    gbm1 <- tibble::rownames_to_column(caret::varImp(GBM)$importance, "Variable")
    colnames(gbm1)[2] <- "Importance.GBM"
    gbm <- gbm1[order(gbm1$Importance.GBM, decreasing = TRUE), ]
    if (length(gbm$Variable) > 25) {
      gbm <- gbm[1:25, ]
    }
  }

  # When DT and RF are missing
  if (is.null(DT) & is.null(RF)) {
    gbm$Variable <- factor(gbm$Variable, levels = gbm$Variable)
    return(ggplot(gbm, aes(x = Importance.GBM, y = Variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      labs(y = paste("Features n = ", length(gbm1$Variable), sep = ""), x = "Variable importance"))
  }

  # When GBM and RF are missing
  if (is.null(GBM) & is.null(RF)) {
    dt$Variable <- factor(dt$Variable, levels = dt$Variable)
    return(ggplot(dt, aes(x = Importance.DT, y = Variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      labs(y = paste("Features n = ", length(dt1$Variable), sep = ""), x = "Variable importance"))
  }
  # When DT and GBM are missing
  if (is.null(DT) & is.null(GBM)) {
    rf$Variable <- factor(rf$Variable, levels = rf$Variable)
    return(ggplot(rf, aes(x = Importance.RF, y = Variable)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      labs(y = paste("Features n = ", length(rf1$Variable), sep = ""), x = "Variable importance"))
  }

  # When DT is missing
  if (is.null(DT)) {
    rfgbm <- dplyr::left_join(rf, gbm, by = "Variable")

    longdata <- reshape(rfgbm, direction = "long", idvar = "Variable", varying = list(c("Importance.RF", "Importance.GBM")))
    colnames(longdata) <- c("Feature", "Method", "VariableImportance")
    rownames(longdata) <- seq_len(nrow(longdata))
    longdata$Method[longdata$Method == 1] <- "RF"
    longdata$Method[longdata$Method == 2] <- "GBM"
    c <- as.data.frame(cbind(table(rfgbm$Importance.RF == 0)[2], table(rfgbm$Importance.GBM == 0)[2]))
    colnames(c) <- c("Importance.RF", "Importance.GBM")
    minn <- names(c)[apply(c, MARGIN = 1, FUN = which.min)]
    longdata$Feature <- factor(longdata$Feature, levels = rfgbm$Variable[order(rfgbm[, minn], decreasing = TRUE)])

    pf <- ggplot(longdata, aes(fill = Method, x = VariableImportance, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
      scale_fill_manual(values = c("#6699FF", "#FF9999")) + # change the color
      labs(y = paste("Features n = ", length(rf1$Variable), sep = ""), x = "Variable importance")
    return(pf)
  }
  # When RF is missing
  if (is.null(RF)) {
    dtgbm <- dplyr::left_join(dt, gbm, by = "Variable")
    longdata <- reshape(dtgbm, direction = "long", idvar = "Variable", varying = list(c("Importance.DT", "Importance.GBM")))
    colnames(longdata) <- c("Feature", "Method", "VariableImportance")
    rownames(longdata) <- seq_len(nrow(longdata))
    longdata$Method[longdata$Method == 1] <- "DT"
    longdata$Method[longdata$Method == 2] <- "GBM"
    c <- as.data.frame(cbind(table(dtgbm$Importance.DT == 0)[2], table(dtgbm$Importance.GBM == 0)[2]))
    colnames(c) <- c("Importance.DT", "Importance.GBM")
    minn <- names(c)[apply(c, MARGIN = 1, FUN = which.min)]
    longdata$Feature <- factor(longdata$Feature, levels = dtgbm$Variable[order(dtgbm[, minn], decreasing = TRUE)])
    pf <- ggplot(longdata, aes(fill = Method, x = VariableImportance, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
      scale_fill_manual(values = c("#6699FF", "#FF9999")) + # change the color
      labs(y = paste("Features n = ", length(dt1$Variable), sep = ""), x = "Variable importance")
    return(pf)
  }
  # When GBM is missing
  if (is.null(GBM)) {
    dtrf <- dplyr::left_join(dt, rf, by = "Variable")
    longdata <- reshape(dtrf, direction = "long", idvar = "Variable", varying = list(c("Importance.DT", "Importance.RF")))
    colnames(longdata) <- c("Feature", "Method", "VariableImportance")
    rownames(longdata) <- seq_len(nrow(longdata))
    longdata$Method[longdata$Method == 1] <- "DT"
    longdata$Method[longdata$Method == 2] <- "RF"
    c <- as.data.frame(cbind(table(dtrf$Importance.DT == 0)[2], table(dtrf$Importance.RF == 0)[2]))
    colnames(c) <- c("Importance.DT", "Importance.RF")
    minn <- names(c)[apply(c, MARGIN = 1, FUN = which.min)]
    longdata$Feature <- factor(longdata$Feature, levels = dtrf$Variable[order(dtrf[, minn], decreasing = TRUE)])
    pf <- ggplot(longdata, aes(fill = Method, x = VariableImportance, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
      scale_fill_manual(values = c("#6699FF", "#FF9999")) + # change the color
      labs(y = paste("Features n = ", length(dt1$Variable), sep = ""), x = "Variable importance")
    return(pf)
  } else {
    rfgbm <- dplyr::left_join(rf, gbm, by = "Variable")
    all <- dplyr::left_join(dt, rfgbm, by = "Variable")
    longdata <- reshape(all, direction = "long", idvar = "Feature", varying = list(c("Importance.DT", "Importance.RF", "Importance.GBM")))
    colnames(longdata) <- c("Feature", "Method", "VariableImportance")
    rownames(longdata) <- seq_len(nrow(longdata))
    longdata$Method[longdata$Method == 1] <- "DT"
    longdata$Method[longdata$Method == 2] <- "RF"
    longdata$Method[longdata$Method == 3] <- "GBM"
    c <- as.data.frame(cbind(table(all$Importance.DT == 0)[2], table(all$Importance.RF == 0)[2], table(all$Importance.GBM == 0)[2]))
    colnames(c) <- c("Importance.DT", "Importance.RF", "Importance.GBM")
    minn <- names(c)[apply(c, MARGIN = 1, FUN = which.min)]
    longdata$Feature <- factor(longdata$Feature, levels = all$Variable[order(all[, minn], decreasing = TRUE)])

    pf <- ggplot(longdata, aes(fill = Method, x = VariableImportance, y = Feature)) +
      geom_bar(position = "dodge", stat = "identity") +
      scale_y_discrete(limits = rev) +
      theme_bw() +
      theme(axis.line = element_line(colour = "black"), panel.border = element_blank()) +
      scale_fill_manual(values = c("#99CCFF", "#6699FF", "#336699", "#FFCCCC", "#FF9999", "#CC6666")) +
      labs(y = paste("Features n = ", length(dt1$Variable), sep = ""), x = "Variable importance")
    return(pf)
  }
}
