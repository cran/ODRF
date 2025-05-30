#' accuracy of oblique decision random forest
#'
#' Prediction accuracy of ODRF at different tree sizes.
#'
#' @param obj An object of class \code{ODRF}, as that created by the function \code{\link{ODRF}}.
#' @param data Training data of class \code{data.frame} in \code{\link{ODRF}} is used to calculate the OOB error.
#' @param newdata A data frame or matrix containing new data is used to calculate the test error. If it is missing, then it is replaced by \code{data}.
#'
#' @return OOB error and test error, misclassification rate (MR) for classification or mean square error (MSE) for regression.
#'
#' @seealso \code{\link{ODRF}} \code{\link{VarImp}} \code{\link{plot.Accuracy}}
#'
#' @examples
#' data(breast_cancer)
#' set.seed(221212)
#' train <- sample(1:569, 80)
#' train_data <- data.frame(breast_cancer[train, -1])
#' test_data <- data.frame(breast_cancer[-train, -1])
#'
#' forest <- ODRF(diagnosis ~ ., train_data,
#'   split = "gini",
#'   parallel = FALSE, ntrees = 50
#' )
#' (error <- Accuracy(forest, train_data, test_data))
#'
#' @keywords forest
#' @export
Accuracy <- function(obj, data, newdata = NULL) {
  vars <- all.vars(obj$terms)
  if (!all(vars[-1] %in% colnames(data))) {
    stop("The column name of 'data' does not match the training data.")
  }

  y <- data[, setdiff(colnames(data), vars[-1])]
  if (is.null(newdata)) newdata <- data
  ynew <- newdata[, setdiff(colnames(newdata), vars[-1])]
  Xnew <- newdata[, vars[-1]]
  Xnew <- as.matrix(Xnew)
  if (obj$split %in% c("gini", "entropy")) {
    y <- factor(y, levels = obj$Levels)
  }

  n <- length(y)
  nt <- ntrees <- obj$forest$ntrees
  nC <- length(obj$Levels)
  ny <- length(ynew)

  treeVotes <- predict(obj, Xnew, type = "tree")
  err.test <- rep(0, ntrees)
  if (!obj$split %in% c("gini", "entropy")) {
    pred <- rowSums(treeVotes)
    err.test[nt] <- mean((ynew - pred / nt)^2) # /e.0;
    for (t in seq(nt - 1, 1)) {
      pred <- pred - treeVotes[, t + 1]
      err.test[t] <- mean((ynew - pred / t)^2) # /e.0;
    }
  } else {
    weights <- rep(1, ny * nt)
    Votes <- factor(c(t(treeVotes)), levels = obj$Levels)
    treeVotes <- matrix(as.integer(Votes), nt, ny)

    Votes <- c(treeVotes) + nC * rep(0:(ny - 1), rep(nt, ny))
    Votes <- aggregate(c(rep(0, ny * nC), weights), by = list(c(1:(ny * nC), Votes)), sum)[, 2]
    # Votes=aggregate(c(rep(0,ny*nC),weights), by=list(c(1:(ny*nC),Votes)),cumsum)[,2];

    # prob=matrix(Votes,ny,nC,byrow = TRUE);
    Votes <- matrix(Votes, ny, nC, byrow = TRUE)
    pred <- obj$Levels[max.col(Votes)] ## "random"
    err.test[nt] <- mean(ynew != pred)
    treeC <- matrix(seq(nC), ny, nC, byrow = TRUE)
    for (t in seq(nt - 1, 1)) {
      Votes <- Votes - (treeC == matrix(treeVotes[t + 1, ], ny, nC)) * 1
      # pred=apply(prob,1,which.max);
      pred <- obj$Levels[max.col(Votes)] ## "random"
      err.test[t] <- mean(ynew != pred)
    }
  }

  if (!obj$forest$storeOOB) {
    stop("out-of-bag indices for each tree are not stored, so can't calculation accuracy!")
  }

  err.oob <- rep(0, ntrees)
  for (tt in 1:ntrees) {
    oobVotes <- matrix(NA, n, tt)
    for (t in 1:tt) {
      oobVotes[obj$structure[[t]]$oobIndex, t] <- obj$structure[[t]]$oobPred
    }
    idx <- which(rowSums(is.na(oobVotes)) < tt)
    oobVotes <- oobVotes[idx, , drop = FALSE]

    if (!obj$split %in% c("gini", "entropy")) {
      pred <- rowMeans(oobVotes, na.rm = TRUE)
      err <- mean((y[idx] - pred)^2) # / mean((y[idx] - mean(y))^2)
    } else {
      ny <- length(y[idx])
      nt <- ncol(oobVotes)
      weights <- rep(1, ny * nt)
      Votes <- factor(c(t(oobVotes)), levels = obj$Levels)
      Votes <- as.integer(Votes) + nC * rep(0:(ny - 1), rep(nt, ny))
      Votes <- aggregate(c(rep(0, ny * nC), weights), by = list(c(1:(ny * nC), Votes)), sum)[, 2]

      prob <- matrix(Votes, ny, nC, byrow = TRUE)
      # pred=apply(prob,1,which.max);
      pred <- max.col(prob) ## "random"
      pred <- obj$Levels[pred]
      err <- mean(y[idx] != pred)
    }
    err.oob[tt] <- err
  }

  error <- list(err.oob = err.oob, err.test = err.test, split = obj$split)

  class(error) <- "Accuracy"
  return(error)
}
