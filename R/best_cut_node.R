#' find best splitting variable and node
#'
#' A function to select the splitting variables and nodes using one of three criteria.
#'
#' @param X An n by d numeric matrix (preferable) or data frame.
#' @param y A response vector of length n.
#' @param split One of three criteria, 'gini': gini impurity index (classification), 'entropy': information gain (classification)
#' or 'mse': mean square error (regression).
#' @param lambda The argument of \code{split} is used to determine the penalty level of the partition criterion. Three options are provided including, \code{lambda=0}: no penalty; \code{lambda=2}: AIC penalty; \code{lambda='log'} (Default): BIC penalty. In Addition, lambda can be any value from 0 to n (training set size).
#' @param MinLeaf Minimal node size (Default 10).
#' @param weights A vector of values which weigh the samples when considering a split.
#' @param numLabels The number of categories.
#'
#' @return A list which contains:
#' \itemize{
#' \item BestCutVar: The best split variable.
#' \item BestCutVal: The best split points for the best split variable.
#' \item BestIndex: Each variable corresponds to maximum decrease in gini impurity index, information gain, and mean square error.
#' }
#'
#' @examples
#' ### Find the best split variable ###
#' data(iris)
#' X <- as.matrix(iris[, 1:4])
#' y <- iris[[5]]
#' bestcut <- best.cut.node(X, y, split = "gini")
#' print(bestcut)
#'
#' @export
best.cut.node <- function(X, y, split, lambda = "log", weights = 1, MinLeaf = 10,
                          numLabels = ifelse(split == "mse", 0, length(unique(y)))) {
  if (any(is.na(X))) {
    stop("data 'X' has Missing value, NA or NaN")
  }

  X <- as.matrix(X)
  if (split != "mse") {
    y <- as.integer(as.factor(y))
  } else {
    y <- c(y)
  }

  if (lambda == "log") {
    lambda <- length(y)
  }

  if (split == "mse") method <- "r"
  if (split == "entropy") method <- "i"
  if (split == "gini") method <- "g"

  # strsplit(split, split = "")[[1]][1]
  .Call("_ODRF_best_cut_node", PACKAGE = "ODRF", method, lambda, X, y, weights, MinLeaf, numLabels)
}
