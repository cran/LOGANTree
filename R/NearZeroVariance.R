#' Flag the features that have (near) zero variance
#'
#' @param data A dataset containing the study’s features.
#'
#' @return This function returns a dataframe with feature names and their frequency ratio, percentage of the unique value and logic values indicating whether the feature is zero variance or has near zero variance.
#' @return feature : name of the features.
#' @return flag.zv (Flag Zero Variance) : True/False, flagging zero variance.
#' @return fr (Frequency Ratio) : the ratio of the value with the highest frequency over the value with the second highest frequency.
#' @return puv (Percentage of Unique Values) : number of the unique values divided by the total number of samples.
#' @return flag.nzv (Flag Near Zero Variance) : True/False, flagging near zero variance.
#' @import stats
#'
#' @export
#'
#' @examples
#' NearZeroVariance(training)
#' @references
#' Boehmke, B., & Greenwell, B. M. (2019). Hands-on machine learning with R. CRC Press.p.52-55. https://doi-org.ezproxy.uio.no/10.1201/9780367816377



NearZeroVariance <- function(data) {

  # zero:only one constant value per variable
  data[is.na(data)] <- 0
  out <- as.data.frame(matrix(NA, nrow = ncol(data), ncol = 5))
  out[, 1] <- colnames(data)
  colnames(out) <- c("feature", "flag.zv", "fr", "puv", "flag.nzv")
  # to see  if variable has a variance of zero
  for (i in seq_len(ncol(data))) {
    if (is.numeric(data[, i]) == FALSE) {
      data[, i] <- as.numeric(as.factor(data[, i]))
    }
    out[i, 2] <- var(data[, i]) == 0
  }


  # the cutoff for the ratio of the most common value to the second most common value.
  cutoff1 <- 10
  cutoff2 <- 95 / 5

  # Frequency Ratio – It is the ratio of the most common value over the second most prevalent value. If the value is close to one, then the predictor is good to use. However, a substantial value indicates that the variable is unbalanced and should be eliminated.
  for (i in seq_len(ncol(data))) {
    if (var(data[, i]) == 0) {
      out[i, 3] <- 0
    } else {
      out[i, 3] <- max(table(data[, i])) / as.numeric(sort(table(data[, i]), partial = length(table(data[, i])) - 1)[length(table(data[, i])) - 1])
    }
  }


  # Percentage of Unique Values – It is calculated by dividing the number of unique values divided by the total number of samples that approaches zero as the granularity of the data increases.
  for (i in seq_len(ncol(data))) {
    out[i, 4] <- length(table(data[, i])) / nrow(data) * 100
  }
  # Flag nearzero variance - Any variable which crosses the predefined threshold for frequency ratio and has a frequency of unique values percentage less than the limit should be considered as zero variance predictor.
  for (i in seq_len(ncol(data))) {
    out[i, 5] <- out[i, 4] <= cutoff1 & out[i, 3] >= cutoff2
    if (out[i,2]==TRUE) {
      out[i,5] <- TRUE
    }
  }
  return(out)
}
