#' @title LOGANTree: Tree-based models for the analysis of log files from computer-based assessments
#'
#' @description This package enables users to model log-file data from computer-based assessments using machine-learning techniques. It allows researchers to generate new knowledge by comparing the performance of three tree-based classification models (i.e., decision trees, random forest, and gradient boosting) to predict student’s outcome. It also contains a set of handful functions for the analysis of the features’ influence on the modeling. Data from the Climate control item from the 2012 Programme for International Student Assessment (PISA, <https://www.oecd.org/pisa/>) is available for an illustration of the package’s capability. An application of the package functions for a math item in PISA 2012 is described in Qin (2022).
#'
#' @section LOGANTree functions: The LOGANTree functions can be categorized in two types: (a) tree-based modeling and (b) features’ analysis. While the first one provides tools for the specification and the evaluation of the three classification models, the second category is devoted to a careful analysis of the data features and their influence on the model’s results. We use the caret package to perform most of the analyses and we provide summary reports and data visualization tools to better compare the three classifiers.
#'
#' What follows is a list of functions organized per category:
#'
#' Tree-based modeling:
#' \itemize{
#' \item{TreeModels}{}
#' \item{DataPartition}{}
#' \item{TreeModelsAllSteps}{}
#' \item{PerformanceMatrics}{}
#' \item{RocPlot}{}
#' }
#' Features’ analysis:
#' \itemize{
#' \item{NearZeroVariance}{}
#' \item{DtResult(}{}
#' \item{VariableImportanceTable}{}
#' \item{VariableImportancePlot}{}
#' \item{ChisquareTable}{}
#' \item{ChisquarePlot}{}
#' \item{PartialDependencePlot}{}
#' }
#' @docType package
#' @name LOGANTree
#' @author \itemize{ \item{Qi Qin [aut, cre]}, \item{Denise Reis Costa [aut, ths]}}
#' @references
#' Qin, Q. (2022). Application of tree-based data mining techniques to examine log file data from a 2012 PISA computer-based Mathematics item. [Unpublished thesis]. University of Oslo.
NULL
#> NULL
