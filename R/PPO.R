#' Projection Pursuit Optimization
#'
#' Find the optimal projection using various projectin pursuit models.
#'
#' @param X An n by d numeric matrix (preferable) or data frame.
#' @param y A response vector of length n.
#' @param model Model for projection pursuit.
#' \itemize{
#' \item{"PPR"(default): projection projection regression from \code{\link{ppr}}. When y is a category label, it is
#' expanded to K binary features.}
#' \item{"Log": logistic based on \code{\link[nnet]{nnet}}.}
#' \item{"Rand": The random projection generated from \eqn{\{-1, 1\}}.
#' The following models can only be used for classification, i.e. the \code{split} must be ''entropy'' or 'gini'.}
#' \item{"LDA", "PDA", "Lr", "GINI", and "ENTROPY" from library \code{PPtreeViz}.}
#' \item{The following models based on \code{\link[Pursuit]{Pursuit}}.
#' \itemize{
#' \item{"holes": Holes index}
#' \item{"cm": Central Mass index}
#' \item{"holes": Holes index}
#' \item{"friedmantukey": Friedman Tukey index}
#' \item{"legendre": Legendre index}
#' \item{"laguerrefourier": Laguerre Fourier index}
#' \item{"hermite": Hermite index}
#' \item{"naturalhermite": Natural Hermite index}
#' \item{"kurtosismax": Maximum kurtosis index}
#' \item{"kurtosismin": Minimum kurtosis index}
#' \item{"moment": Moment index}
#' \item{"mf": MF index}
#' \item{"chi": Chi-square index}
#' }}
#' }
#' @param split The criterion used for splitting the variable. 'gini': gini impurity index (classification, default),
#'        'entropy': information gain (classification) or 'mse': mean square error (regression).
#' @param weights Vector of non-negative observational weights; fractional weights are allowed (default NULL).
#' @param ... optional parameters to be passed to the low level function.
#'
#' @return Optimal projection direction.
#'
#' @keywords rotation
#'
#' @references Friedman, J. H., & Stuetzle, W. (1981). Projection pursuit regression. Journal of the American statistical Association, 76(376), 817-823.
#' @references Ripley, B. D. (1996) Pattern Recognition and Neural Networks. Cambridge.
#' @references Lee, YD, Cook, D., Park JW, and Lee, EK(2013) PPtree: Projection Pursuit Classification Tree, Electronic Journal of Statistics, 7:1369-1386.
#' @references Cook, D., Buja, A., Lee, E. K., & Wickham, H. (2008). Grand tours, projection pursuit guided tours, and manual controls. In Handbook of data visualization (pp. 295-314). Springer, Berlin, Heidelberg.
#'
#' @seealso \code{\link{RotMatPPO}}
#'
#' @examples
#' # classification
#' data(seeds)
#' (PP <- PPO(seeds[, 1:7], seeds[, 8], model = "Log", split = "entropy"))
#' (PP <- PPO(seeds[, 1:7], seeds[, 8], model = "PPR", split = "entropy"))
#' (PP <- PPO(seeds[, 1:7], seeds[, 8], model = "LDA", split = "entropy"))
#'
#' # regression
#' data(body_fat)
#' (PP <- PPO(body_fat[, 2:15], body_fat[, 1], model = "Log", split = "mse"))
#' (PP <- PPO(body_fat[, 2:15], body_fat[, 1], model = "Rand", split = "mse"))
#' (PP <- PPO(body_fat[, 2:15], body_fat[, 1], model = "PPR", split = "mse"))
#'
#' @import Pursuit Rcpp
#' @importFrom stats ppr
#' @importFrom nnet nnet
#' @export
PPO <- function(X, y, model = "PPR", split = "gini", weights = NULL, ...) {
  X <- as.matrix(X)
  p <- ncol(X)

  PP <- 1
  if (p > 1) {
    n <- length(y)
    weights <- if (is.null(weights)) rep(1, n)

    Y <- c(y)
    indC <- 0L
    if (split %in% c("gini", "entropy")) {
      y <- as.factor(y)
      indC <- levels(y)
      if (length(indC) > 2) {
        Y <- (matrix(y, n, length(indC)) == matrix(indC, n, length(indC), byrow = TRUE)) + 0
      } else {
        Y <- as.integer(y)
      }
    }

    if ((split == "mse") && (!model %in% c("PPR", "Rand", "Log"))) {
      stop(paste0("'model = ", model, "' can only be used for classification"))
    }

    if (model == "PPR") {
      PP <- ppr(X, Y, weights = weights, nterms = 1, bass = 1)$alpha # sm.method="spline",sm.method="gcvspline"
    } else if (model == "Rand") {
      PP <- sample(c(1L, -1L), p, replace = TRUE, prob = c(0.5, 0.5))
    } else if (model == "Log") {
      # if((n>5*p)&(p<10)){
      # PP <- ppr(X, Y,weights,nterms = 1, bass=1)$alpha
      # }else{
      # PP = try(nnet(Xi, y, size=1,trace=FALSE)$wts[2:(1+pi)], silent = TRUE)
      PP <- nnet(X, Y, weights = weights, size = 1, linout = TRUE, trace = FALSE)$wts[2:(1 + p)] #
      # }
    } else if (model %in% c("LDA", "PDA", "Lr", "GINI", "ENTROPY")) {
      PP <- ppOptCpp(as.integer(y), X, q = 1, PPmethod = model, weight = TRUE, r = 1, lambda = 0.1, energy = 0, cooling = 0.9, TOL = 0.0001, maxiter = 1000L)$projbest
    } else {
      PP <- PP_Optimizer(
        data = X, class = y, findex = model,
        optmethod = "GTSA", dimproj = 1, sphere = TRUE,
        weight = TRUE, lambda = 0.1, r = 1, cooling = 0.9,
        eps = 0.0001, maxiter = 1000L, half = 30
      )$vector.opt # invisible()
      # projbest=res$vector.opt
      # indexbest=res$index[length(res$index)]
    }
  }

  return(as.vector(PP))
}
