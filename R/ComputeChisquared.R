#' Compute the chi-square scores of features
#'
#' @param data A dataset containing an outcome variable and action features with either raw frequencies or weighted frequencies.
#' @param outcome Name of the outcome variable.
#' @param level The level of outcome. e.g. correct/incorrect would be of 2 levels; 0/1/2 would be 3 levels
#' @param weight If weight = TRUE, the weighted frequencies will be computed and then be utilized for the chi-square scores ; If weight = F, returning the chisquare scores computed from the raw feature frequencies.
#' @param ctable If ctable = TRUE, returning the contingency tables instead of the chi-square scores.
#' @return This function returns a data frame with ranked chi-scores or contingency tables for each feature.
#' @return To get the weighted frequencies solely, please run WeightedFeatures() in LOGAN package.
#'
#' @examples
#' ComputeChisquared(data = cp025q01.wgt[,c(7:13,15)],
#' outcome = "outcome", level = 2, weight = FALSE, ctable = FALSE)
#'
#' ComputeChisquared(data = training[,7:14],
#' outcome = "outcome", level = 2, weight = FALSE, ctable = TRUE)
#'
#' @export
#'
#' @references
#'He Q., von Davier M. (2015) Identifying Feature Sequences from Process Data in Problem-Solving Items with N-Grams. In: van der Ark L., Bolt D., Wang WC., Douglas J., Chow SM. (eds) Quantitative Psychology Research. Springer Proceedings in Mathematics & Statistics, vol 140. Springer, Cham. https://doi-org.ezproxy.uio.no/10.1007/978-3-319-19977-1_13
ComputeChisquared <- function(data,outcome,level,weight=FALSE,ctable=FALSE){

  vlen <- ncol(data)-1
  data.bi <- data
  ciact <- matrix(NA,level,vlen)
  cinonact <- matrix(NA,level,vlen)
  ma <- vector(mode = "list", length = vlen)
  chicolumn <- rep(NA,vlen)
  ngram.freqnames <- names(data)[!names(data)%in%outcome]

  if (weight==TRUE) {

    #change feature frequency to binary values
    ngram.binames <- paste0("bi.", colnames(data)[!names(data) %in% outcome])

    for (i in seq_len(length(ngram.binames))) {
      data.bi[,ngram.binames[i]] <- ifelse(data[,ngram.freqnames[i]]==0,0,1)
    }

    #computed the weighted frequency
    weightnames <- paste0("wgt.", colnames(data)[!names(data) %in% outcome])

    for (i in seq_len(vlen)) {
      for (j in seq_len(nrow(data))) {
        if (data.bi[j,ngram.binames[i]]==0) {
          data.bi[j,weightnames[i]] <- 0
        } else {
          data.bi[j,weightnames[i]] <- (1+log(data.bi[j,ngram.freqnames[i]]))*(log(nrow(data)/sum((data.bi[,ngram.binames[i]]))))
        }
      }
    }

    weighteddata <- data.bi[,-(1:(2*vlen+1))]

    data.wgt <- cbind(weighteddata,data[,outcome])
    colnames(data.wgt)[ncol(data.wgt)] <- outcome
    data.wgt[,outcome] <- as.numeric(as.factor(data.wgt[,outcome]))
    #ciact - the number of actions in each outcome level group
    #cinonact - the number of non actions in each outcome level group
    for (i in 1:level) {
      ciact[i,] <- colSums(data.wgt[data.wgt[,outcome]==i,!names(data.wgt) %in% outcome])
      cinonact[i,] <- sum(data.wgt[data.wgt[,outcome]==i,!names(data.wgt) %in% outcome])-colSums(data.wgt[data.wgt[,outcome]==i,!names(data.wgt) %in% outcome])
    }
    rownames(cinonact) <- paste0("level",seq(level))
    rownames(ciact) <- paste0("level",seq(level))
    #contingency matrix
    for (j in 1:(vlen)) {
      ma[[j]] <- t(cbind(ciact[,j],cinonact[,j]))
    }
    ma <- lapply(ma, "rownames<-", c("act","nonact"))
    names(ma) <- colnames(data.wgt[!colnames(data.wgt) %in% outcome])

    for (i in 1:(ncol(data)-1)) {
      if (sum(ma[[i]]<5)>0) {warning(paste0("Contingency table contains frequency less than 5 - feature:", colnames(data)[i]))
        warning("Please check the features by running NearZeroVariance()")}

    }

    #compute the chisquare for each contingency matrix
    for (j in 1:(vlen)) {
      chicolumn[j] <- lapply(ma,chisq.test)[[j]][1]
    }

    chimatrix <- as.data.frame(cbind(names(data.wgt)[!names(data.wgt) %in% outcome],round(as.numeric(chicolumn),4)))
    colnames(chimatrix) <- c("Feature","Chi-square Score")
    om <- chimatrix[order(as.numeric(chimatrix$`Chi-square Score`),decreasing = TRUE),]
    rownames(om) <- seq_len(nrow(om))

    if (ctable==FALSE) {
      return(om)
    }
    if (ctable==TRUE) {
      return(ma)
    }
  }
  if(weight==FALSE){

    if (is.factor(data[,outcome])==TRUE) {
      data[,outcome] <- as.numeric(data[,outcome])
    }
    if (is.factor(data[,outcome])==FALSE){
      data[,outcome] <- as.numeric(as.factor(data[,outcome]))
    }


    for (i in 1:level) {
      ciact[i,] <- colSums(data[data[,outcome]==i,!names(data) %in% outcome])
      cinonact[i,] <- sum(data[data[,outcome]==i,!names(data) %in% outcome])-colSums(data[data[,outcome]==i,!names(data) %in% outcome])
    }
    rownames(cinonact) <- paste0("level",seq(level))
    rownames(ciact) <- paste0("level",seq(level))
    for (j in 1:(vlen)) {
      ma[[j]] <- t(cbind(ciact[,j],cinonact[,j]))

    }
    ma <- lapply(ma, "rownames<-", c("act","nonact"))
    names(ma) <- colnames(data[!colnames(data) %in% outcome])

    for (i in 1:(ncol(data)-1)) {
      if (sum(ma[[i]]<5)>0) {warning(paste0("Contingency table contains frequency less than 5 - feature:", colnames(data)[i]))
        warning("Please check the features by running NearZeroVariance()")}

    }

    for (j in 1:(vlen)) {
      chicolumn[j] <- lapply(ma,chisq.test)[[j]][1]
    }

    chimatrix <- as.data.frame(cbind(names(data)[!names(data) %in% outcome],round(as.numeric(chicolumn),4)))
    colnames(chimatrix) <- c("Feature","Chi-square Score")
    om <- chimatrix[order(as.numeric(chimatrix$`Chi-square Score`),decreasing = TRUE),]
    rownames(om) <- seq_len(nrow(om))
    if (ctable==FALSE) {
      return(om)}

    if (ctable==TRUE) {
      return(ma)
    }

  }

}











