% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ODT.R
\name{ODT}
\alias{ODT}
\alias{ODT.formula}
\alias{ODT.default}
\title{Classification and Regression with Oblique Decision Tree}
\usage{
ODT(X, ...)

\method{ODT}{formula}(
  formula,
  data = NULL,
  Xsplit = NULL,
  split = "auto",
  lambda = "log",
  NodeRotateFun = "RotMatPPO",
  FunDir = getwd(),
  paramList = NULL,
  glmnetParList = NULL,
  MaxDepth = Inf,
  numNode = Inf,
  MinLeaf = 10,
  Levels = NULL,
  subset = NULL,
  weights = NULL,
  na.action = na.fail,
  catLabel = NULL,
  Xcat = 0,
  Xscale = "Min-max",
  TreeRandRotate = FALSE,
  ...
)

\method{ODT}{default}(
  X,
  y,
  Xsplit = NULL,
  split = "auto",
  lambda = "log",
  NodeRotateFun = "RotMatPPO",
  FunDir = getwd(),
  paramList = NULL,
  glmnetParList = NULL,
  MaxDepth = Inf,
  numNode = Inf,
  MinLeaf = 10,
  Levels = NULL,
  subset = NULL,
  weights = NULL,
  na.action = na.fail,
  catLabel = NULL,
  Xcat = 0,
  Xscale = "Min-max",
  TreeRandRotate = FALSE,
  ...
)
}
\arguments{
\item{X}{An n by d numeric matrix (preferable) or data frame.}

\item{...}{Optional parameters to be passed to the low level function.}

\item{formula}{Object of class \code{formula} with a response describing the model to fit. If this is a data frame, it is taken as the model frame. (see \code{\link{model.frame}})}

\item{data}{Training data of class \code{data.frame} containing variables named in the formula. If \code{data} is missing it is obtained from the current environment by \code{formula}.}

\item{Xsplit}{Splitting variables used to construct linear model trees. The default value is NULL and is only valid when split="linear".}

\item{split}{The criterion used for splitting the nodes. "entropy": information gain and "gini": gini impurity index for classification; "mse": mean square error for regression; "linear": mean square error for linear model.
'auto' (default): If the response in \code{data} or \code{y} is a factor, "gini" is used, otherwise "mse" is assumed.}

\item{lambda}{The argument of \code{split} is used to determine the penalty level of the partition criterion. Three options are provided including, \code{lambda=0}: no penalty; \code{lambda=2}: AIC penalty; \code{lambda='log'} (Default): BIC penalty. In Addition, lambda can be any value from 0 to n (training set size).}

\item{NodeRotateFun}{Name of the function of class \code{character} that implements a linear combination of predictors in the split node.
including \itemize{
\item{"RotMatPPO": projection pursuit optimization model (\code{\link{PPO}}), see \code{\link{RotMatPPO}} (default, model="PPR").}
\item{"RotMatRF": single feature similar to CART, see \code{\link{RotMatRF}}.}
\item{"RotMatRand": random rotation, see \code{\link{RotMatRand}}.}
\item{"RotMatMake": users can define this function, for details see \code{\link{RotMatMake}}.}
}}

\item{FunDir}{The path to the \code{function} of the user-defined \code{NodeRotateFun} (default current working directory).}

\item{paramList}{List of parameters used by the functions \code{NodeRotateFun}. If left unchanged, default values will be used, for details see \code{\link[ODRF]{defaults}}.}

\item{glmnetParList}{List of parameters used by the functions \code{glmnet} and \code{cv.glmnet} in package \code{glmnet}.
\code{glmnetParList=list(lambda = 0)} is Ordinary Least Squares (OLS) regression, \code{glmnetParList=list(family = "gaussian")} (default) is regression model
and \code{glmnetParList=list(family = "binomial" or "multinomial")} is classification model. If left unchanged, default values will be used, for details see \code{\link[glmnet]{glmnet}} and \code{\link[glmnet]{cv.glmnet}}.}

\item{MaxDepth}{The maximum depth of the tree (default \code{Inf}).}

\item{numNode}{Number of nodes that can be used by the tree (default \code{Inf}).}

\item{MinLeaf}{Minimal node size (Default 10).}

\item{Levels}{The category label of the response variable when \code{split} is not equal to 'mse'.}

\item{subset}{An index vector indicating which rows should be used. (NOTE: If given, this argument must be named.)}

\item{weights}{Vector of non-negative observational weights; fractional weights are allowed (default NULL).}

\item{na.action}{A function to specify the action to be taken if NAs are found. (NOTE: If given, this argument must be named.)}

\item{catLabel}{A category labels of class \code{list} in predictors. (default NULL, for details see Examples)}

\item{Xcat}{A class \code{vector} is used to indicate which predictor is the categorical variable. The default Xcat=0 means that no special treatment is given to category variables.
When Xcat=NULL, the predictor x that satisfies the condition "\code{(length(table(x))<10) & (length(x)>20)}" is judged to be a category variable.}

\item{Xscale}{Predictor standardization methods. " Min-max" (default), "Quantile", "No" denote Min-max transformation, Quantile transformation and No transformation respectively.}

\item{TreeRandRotate}{If or not to randomly rotate the training data before building the tree (default FALSE, see \code{\link[ODRF]{RandRot}}).}

\item{y}{A response vector of length n.}
}
\value{
An object of class ODT containing a list of components::
\itemize{
\item{\code{call}: The original call to ODT.}
\item{\code{terms}: An object of class \code{c("terms", "formula")} (see \code{\link{terms.object}}) summarizing the formula. Used by various methods, but typically not of direct relevance to users.}
\item{\code{split}, \code{Levels} and \code{NodeRotateFun} are important parameters for building the tree.}
\item{\code{predicted}: the predicted values of the training data.}
\item{\code{projections}: Projection direction for each split node.}
\item{\code{paramList}: Parameters in a named list to be used by \code{NodeRotateFun}.}
\item{\code{data}: The list of data related parameters used to build the tree.}
\item{\code{tree}: The list of tree related parameters used to build the tree.}
\item{\code{structure}: A set of tree structure data records.
\itemize{
\item{\code{nodeRotaMat}: Record the split variables (first column), split node serial number (second column) and rotation direction (third column) for each node. (The first column and the third column are 0 means leaf nodes)}
\item{\code{nodeNumLabel}: Record each leaf node's category for classification or predicted value for regression (second column is data size). (Each column is 0 means it is not a leaf node)}
\item{\code{nodeCutValue}: Record the split point of each node. (0 means leaf nodes)}
\item{\code{nodeCutIndex}: Record the index values of the partitioning variables selected based on the partition criterion \code{split}.}
\item{\code{childNode}: Record the number of child nodes after each splitting.}
\item{\code{nodeDepth}: Record the depth of the tree where each node is located.}
\item{\code{nodeIndex}: Record the indices of the data used in each node.}
\item{\code{glmnetFit}: Record the model fitted by function \code{glmnet} used in each node.}
}}
}
}
\description{
Classification and regression using an oblique decision tree (ODT) in which each node is split by a linear combination of predictors. Different methods are provided for selecting the linear combinations, while the splitting values are chosen by one of three criteria.
}
\examples{
# Classification with Oblique Decision Tree.
data(seeds)
set.seed(221212)
train <- sample(1:209, 100)
train_data <- data.frame(seeds[train, ])
test_data <- data.frame(seeds[-train, ])
tree <- ODT(varieties_of_wheat ~ ., train_data, split = "entropy")
pred <- predict(tree, test_data[, -8])
# classification error
(mean(pred != test_data[, 8]))

# Regression with Oblique Decision Tree.
data(body_fat)
set.seed(221212)
train <- sample(1:252, 100)
train_data <- data.frame(body_fat[train, ])
test_data <- data.frame(body_fat[-train, ])
tree <- ODT(Density ~ ., train_data,
  split = "mse",
  NodeRotateFun = "RotMatPPO", paramList = list(model = "Log", dimProj = "Rand")
)
pred <- predict(tree, test_data[, -1])
# estimation error
mean((pred - test_data[, 1])^2)

# Use "Z" as the splitting variable to build a linear model tree for "X" and "y".
set.seed(10)
cutpoint <- 50
X <- matrix(rnorm(100 * 10), 100, 10)
age <- sample(seq(20, 80), 100, replace = TRUE)
height <- sample(seq(50, 200), 100, replace = TRUE)
weight <- sample(seq(5, 150), 100, replace = TRUE)
Z <- cbind(age = age, height = height, weight = weight)
mu <- rep(0, 100)
mu[age <= cutpoint] <- X[age <= cutpoint, 1] + X[age <= cutpoint, 2]
mu[age > cutpoint] <- X[age > cutpoint, 1] + X[age > cutpoint, 3]
y <- mu + rnorm(100)
# Regression model tree
my.tree <- ODT(
  X = X, y = y, Xsplit = Z, split = "linear", lambda = 0,
  NodeRotateFun = "RotMatRF",
  glmnetParList = list(lambda = 0, family = "gaussian")
)
pred <- predict(my.tree, X, Xsplit = Z)
# fitting error
mean((pred - y)^2)
mean((my.tree$predicted - y)^2)
# Classification model tree
y1 <- (y > 0) * 1
my.tree <- ODT(
  X = X, y = y1, Xsplit = Z, split = "linear", lambda = 0,
  NodeRotateFun = "RotMatRF", MinLeaf = 10, MaxDepth = 5,
  glmnetParList = list(family = "binomial")
)
(class <- predict(my.tree, X, Xsplit = Z, type = "pred"))
(prob <- predict(my.tree, X, Xsplit = Z, type = "prob"))

# Projection analysis of the oblique decision tree.
data(iris)
tree <- ODT(Species ~ .,
  data = iris, split = "gini",
  paramList = list(model = "PPR", numProj = 1)
)
print(round(tree[["projections"]], 3))

### Train ODT on one-of-K encoded categorical data ###
# Note that the category variable must be placed at the beginning of the predictor X
# as in the following example.
set.seed(22)
Xcol1 <- sample(c("A", "B", "C"), 100, replace = TRUE)
Xcol2 <- sample(c("1", "2", "3", "4", "5"), 100, replace = TRUE)
Xcon <- matrix(rnorm(100 * 3), 100, 3)
X <- data.frame(Xcol1, Xcol2, Xcon)
Xcat <- c(1, 2)
catLabel <- NULL
y <- as.factor(sample(c(0, 1), 100, replace = TRUE))
tree <- ODT(X, y, split = "entropy", Xcat = NULL)
head(X)
#>   Xcol1 Xcol2          X1         X2          X3
#> 1     B     5 -0.04178453  2.3962339 -0.01443979
#> 2     A     4 -1.66084623 -0.4397486  0.57251733
#> 3     B     2 -0.57973333 -0.2878683  1.24475578
#> 4     B     1 -0.82075051  1.3702900  0.01716528
#> 5     C     5 -0.76337897 -0.9620213  0.25846351
#> 6     A     5 -0.37720294 -0.1853976  1.04872159

# one-of-K encode each categorical feature and store in X1
numCat <- apply(X[, Xcat, drop = FALSE], 2, function(x) length(unique(x)))
# initialize training data matrix X
X1 <- matrix(0, nrow = nrow(X), ncol = sum(numCat))
catLabel <- vector("list", length(Xcat))
names(catLabel) <- colnames(X)[Xcat]
col.idx <- 0L
# convert categorical feature to K dummy variables
for (j in seq_along(Xcat)) {
  catMap <- (col.idx + 1):(col.idx + numCat[j])
  catLabel[[j]] <- levels(as.factor(X[, Xcat[j]]))
  X1[, catMap] <- (matrix(X[, Xcat[j]], nrow(X), numCat[j]) ==
    matrix(catLabel[[j]], nrow(X), numCat[j], byrow = TRUE)) + 0
  col.idx <- col.idx + numCat[j]
}
X <- cbind(X1, X[, -Xcat])
colnames(X) <- c(paste(rep(seq_along(numCat), numCat), unlist(catLabel),
  sep = "."
), "X1", "X2", "X3")

# Print the result after processing of category variables.
head(X)
#>   1.A 1.B 1.C 2.1 2.2 2.3 2.4 2.5          X1         X2          X3
#> 1   0   1   0   0   0   0   0   1 -0.04178453  2.3962339 -0.01443979
#> 2   1   0   0   0   0   0   1   0 -1.66084623 -0.4397486  0.57251733
#> 3   0   1   0   0   1   0   0   0 -0.57973333 -0.2878683  1.24475578
#> 4   0   1   0   1   0   0   0   0 -0.82075051  1.3702900  0.01716528
#> 5   0   0   1   0   0   0   0   1 -0.76337897 -0.9620213  0.25846351
#> 6   1   0   0   0   0   0   0   1 -0.37720294 -0.1853976  1.04872159
catLabel
#> $Xcol1
#> [1] "A" "B" "C"
#>
#> $Xcol2
#> [1] "1" "2" "3" "4" "5"

tree <- ODT(X, y, split = "gini", Xcat = c(1, 2), catLabel = catLabel, NodeRotateFun = "RotMatRF")

}
\references{
Zhan, H., Liu, Y., & Xia, Y. (2022). Consistency of The Oblique Decision Tree and Its Random Forest. arXiv preprint arXiv:2211.12653.
}
\seealso{
\code{\link{online.ODT}} \code{\link{prune.ODT}} \code{\link{as.party.ODT}} \code{\link{predict.ODT}} \code{\link{print.ODT}} \code{\link{plot.ODT}} \code{\link{plot_ODT_depth}}
}
\author{
Yu Liu and Yingcun Xia
}
\keyword{tree}
