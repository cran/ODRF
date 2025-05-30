#' Pruning of class \code{ODRF}.
#'
#' Prune \code{ODRF} from bottom to top with test data based on prediction error.
#'
#' @param obj An object of class \code{\link{ODRF}}.
#' @param X An n by d numeric matrix (preferable) or data frame is used to prune the object of class \code{ODRF}.
#' @param y A response vector of length n.
#' @param MaxDepth The maximum depth of the tree after pruning (Default 1).
#' @param useOOB Whether to use OOB for pruning (Default TRUE). Note that when \code{useOOB=TRUE}, \code{X} and \code{y} must be the training data in \code{\link{ODRF}}.
#' @param ... Optional parameters to be passed to the low level function.
#'
#' @return An object of class \code{ODRF} and \code{prune.ODRF}.
#' \itemize{
#' \item{\code{ppForest} The same result as \code{ODRF}.}
#' \item{\code{pruneError} Error of test data or OOB after each pruning in each tree, misclassification rate (MR) for classification or mean square error (MSE) for regression.}
#' }
#' @seealso \code{\link{ODRF}} \code{\link{online.ODRF}} \code{\link{prune.ODT}}
#'
#' @examples
#' # Classification with Oblique Decision Random Forest
#' data(seeds)
#' set.seed(221212)
#' train <- sample(1:209, 80)
#' train_data <- data.frame(seeds[train, ])
#' test_data <- data.frame(seeds[-train, ])
#' forest <- ODRF(varieties_of_wheat ~ ., train_data,
#'   split = "entropy", parallel = FALSE, ntrees = 50
#' )
#' prune_forest <- prune(forest, train_data[, -8], train_data[, 8])
#' pred <- predict(prune_forest, test_data[, -8])
#' # classification error
#' (mean(pred != test_data[, 8]))
#' \donttest{
#' # Regression with Oblique Decision Random Forest
#' data(body_fat)
#' set.seed(221212)
#' train <- sample(1:252, 80)
#' train_data <- data.frame(body_fat[train, ])
#' test_data <- data.frame(body_fat[-train, ])
#' index <- seq(floor(nrow(train_data) / 2))
#' forest <- ODRF(Density ~ ., train_data[index, ], split = "mse", parallel = FALSE, ntrees = 50)
#' prune_forest <- prune(forest, train_data[-index, -1], train_data[-index, 1], useOOB = FALSE)
#' pred <- predict(prune_forest, test_data[, -1])
#' # estimation error
#' mean((pred - test_data[, 1])^2)
#' }
#' @keywords forest prune
#' @rdname prune.ODRF
#' @aliases prune.ODRF
#' @method prune ODRF
#' @export
prune.ODRF <- function(obj, X, y, MaxDepth = 1, useOOB = TRUE, ...) {
  if (length(obj[["structure"]][[1]][["structure"]][["nodeDepth"]]) == 1) {
    stop("No tree structure to use 'prune'!")
  }

  structure <- obj$structure
  split <- obj$split
  ratOOB <- obj$forest$ratOOB
  storeOOB <- obj$forest$storeOOB
  replacement <- obj$forest$replacement
  stratify <- obj$forest$stratify
  numCores <- obj$forest$numCores
  parallel <- obj$forest$parallel

  if ((!storeOOB) && useOOB) {
    stop("out-of-bag indices for each tree are not stored. ODRF must be called with storeOOB = TRUE.")
  }

  Xcat <- obj$data$Xcat
  catLabel <- obj$data$catLabel
  # vars=all.vars(ppTree$terms)
  if ((sum(Xcat) > 0) && is.null(catLabel)) {
    # vars=vars[-(1+seq(length(unlist(catLabel))))]
    # }else{
    stop("'Xcat!=0' however 'catLabel' does not exist!")
  }
  # vars=all.vars(obj$terms)
  Xna <- is.na(X)
  if (any(Xna)) {
    xj <- which(colSums(Xna) > 0)
    warning("There are NA values in columns ", paste(xj, collapse = ", "), " of the data 'X', which will be replaced with the average value.")
    for (j in xj) {
      X[Xna[, j], j] <- mean(X[, j], na.rm = TRUE)
    }
  }
  Xnew <- as.matrix(X)
  ynew <- y
  # ynew= data[,setdiff(colnames(data),vars[-1])]
  # Xnew= data[,vars[-1]]
  # ynew <- data[, 1]
  # Xnew <- data[, -1]
  # rm(data)
  rm(X)
  rm(y)


  p <- ncol(Xnew)
  n <- nrow(Xnew)
  nC <- length(obj$Levels)
  numClass <- nC
  ntrees <- length(structure)

  if (split %in% c("gini", "entropy")) {
    classCt <- cumsum(table(ynew))
    if (stratify) {
      Cindex <- vector("list", numClass)
      for (m in 1L:numClass) {
        Cindex[[m]] <- which(ynew == obj$Levels[m])
      }
    }
  }

  numCat <- 0
  if (sum(Xcat) > 0) {
    xj <- 1
    Xnew1 <- matrix(0, nrow = n, ncol = length(unlist(catLabel))) # initialize training data matrix X
    # one-of-K encode each categorical feature and store in X
    for (j in seq_along(Xcat)) {
      catMap <- which(catLabel[[j]] %in% unique(Xnew[, Xcat[j]]))
      indC <- catLabel[[j]][catMap]
      Xnewj <- (matrix(Xnew[, Xcat[j]], n, length(indC)) == matrix(indC, n, length(indC), byrow = TRUE)) + 0

      if (length(indC) > length(catLabel[[j]])) {
        Xnewj <- Xnewj[, seq_along(catLabel[[j]])]
      }

      xj1 <- xj + length(catLabel[[j]])
      Xnew1[, (xj:(xj1 - 1))[catMap]] <- Xnewj
      xj <- xj1
    }

    Xnew <- cbind(Xnew1, apply(Xnew[, -Xcat], 2, as.numeric))
    p <- ncol(Xnew)
    numCat <- length(unlist(catLabel))
    rm(Xnew1)
    rm(Xnewj)
  }
  Xnew <- as.matrix(Xnew)
  colnames(Xnew) <- obj$data$varName

  if (useOOB) {
    if (prod(dim(Xnew)) != obj$data$n * obj$data$p) {
      stop("'data' must be the training data 'data' in class ODRF.")
    }
  }

  # if (!is.null(obj$data$subset)) {
  #  Xnew <- Xnew[obj$data$subset, ]
  # }

  # Variable scaling.
  if (obj$data$Xscale != "No") {
    indp <- (sum(numCat) + 1):p
    Xnew[, indp] <- (Xnew[, indp] - matrix(obj$data$minCol, n, length(indp), byrow = T)) /
      matrix(obj$data$maxminCol, n, length(indp), byrow = T)
  }


  PPtree <- function(itree, ...) {
    # set.seed(seed + itree)
    ppTree <- obj[seq(7)]
    ppTree$data <- c(obj$data, structure[[itree]][c(1, 2)])
    ppTree$data$Xcat <- 0L
    ppTree$data$Xscale <- "No"
    ppTree$tree <- obj$tree
    ppTree$structure <- structure[[itree]][-c(1, 2)]
    class(ppTree) <- "ODT"

    if (useOOB) {
      ppTree <- prune(ppTree, Xnew[ppTree$structure$oobIndex, ], ynew[ppTree$structure$oobIndex], MaxDepth) # [seq(7)]
      TreeRotate <- list(rotdims = ppTree[["data"]][["rotdims"]], rotmat = ppTree[["data"]][["rotmat"]])
      ppTree <- ppTree$structure # [-length(ppForestT)]
    } else {
      TDindx0 <- seq(n)
      TDindx <- TDindx0
      if (replacement) {
        go <- TRUE
        while (go) {
          # make sure each class is represented in proportion to classes in initial dataset
          if (stratify && (split %in% c("gini", "entropy"))) {
            if (classCt[1L] != 0L) {
              TDindx[1:classCt[1L]] <- sample(Cindex[[1L]], classCt[1L], replace = TRUE)
            }
            for (z in 2:numClass) {
              if (classCt[z - 1L] != classCt[z]) {
                TDindx[(classCt[z - 1L] + 1L):classCt[z]] <- sample(Cindex[[z]], classCt[z] - classCt[z - 1L], replace = TRUE)
              }
            }
          } else {
            TDindx <- sample(TDindx0, n, replace = TRUE)
          }
          go <- all(TDindx0 %in% TDindx)
        }
      } else {
        TDindx <- sample.int(TDindx0, ceiling(n * (1 - ratOOB)), replace = FALSE)
      }

      if ((ratOOB > 0) && storeOOB) {
        ppTree$structure <- ppTree$structure[-(length(ppTree$structure) - c(2, 1, 0))]
      }
      # data=data.frame(y=ynew[TDindx],Xnew[TDindx,])
      # colnames(data)=vars
      # weights1=weights[TDindx]
      class(ppTree) <- "ODT"
      ppTree <- prune(ppTree, Xnew[TDindx, ], ynew[TDindx], MaxDepth)
      ppTree <- ppTree[-length(ppTree)]
      class(ppTree) <- "ODT"

      TreeRotate <- list(rotdims = ppTree[["data"]][["rotdims"]], rotmat = ppTree[["data"]][["rotmat"]])

      if ((ratOOB > 0) && storeOOB) {
        oobErr <- 1
        # if(useOOB){
        #  NTD = ppTree$oobIndex
        # }else{
        NTD <- setdiff(TDindx0, TDindx)
        # }
        pred <- predict(ppTree, Xnew[NTD, ])

        if (split %in% c("gini", "entropy")) {
          oobErr <- mean(pred != ynew[NTD])
        } else {
          oobErr <- mean((pred - ynew[NTD])^2)
        }

        ppTree <- c(ppTree$structure, list(oobErr = oobErr, oobIndex = NTD, oobPred = pred))
      } else {
        ppTree <- ppTree$structure
      }
    }

    return(c(TreeRotate, ppTree))
  }


  if (parallel) {
    # RNGkind("L'Ecuyer-CMRG")
    if (is.infinite(numCores)) {
      # Use all but 1 core if numCores=0.
      numCores <- parallel::detectCores() - 1L # logical = FALSE
    }
    numCores <- min(numCores, ntrees)
    gc()

    # cl <- parallel::makePSOCKcluster(num.cores)
    # library("ODRF1")
    # library(foreach)
    # foreach::registerDoSEQ()
    cl <- parallel::makeCluster(numCores, type = ifelse(.Platform$OS.type == "windows", "PSOCK", "FORK"))
    chunks <- parallel::clusterSplit(cl, seq(ntrees))
    doParallel::registerDoParallel(cl, numCores)


    # set.seed(seed)
    icore <- NULL
    ppForestT <- foreach::foreach(
      icore = seq_along(chunks), .combine = list, .multicombine = TRUE, .export = c("ODT_compute"),
      .packages = "ODRF"
    ) %dopar% {
      lapply(chunks[[icore]], PPtree)
    }
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)

    # do.call(rbind.fill,list1)
    obj$structure <- do.call("c", ppForestT)
    # ppForest$structure=NULL
    # for (i in 1:numCores) {
    #  ppForest$structure=c(ppForest$structure,ppForestT[[i]])
    # }
  } else {
    # Use just one core.
    obj$structure <- lapply(1:ntrees, PPtree)
  }


  ####################################
  if ((ratOOB > 0) && storeOOB && (!useOOB)) {
    oobVotes <- matrix(NA, n, ntrees)
    for (t in 1:ntrees) {
      oobVotes[obj$structure[[t]]$oobIndex, t] <- obj$structure[[t]]$oobPred
    }
    idx <- which(rowSums(is.na(oobVotes)) < ntrees)
    oobVotes <- oobVotes[idx, , drop = FALSE]
    yy <- ynew[idx]

    if (split %in% c("gini", "entropy")) {
      ny <- length(yy)
      nC <- numClass
      weights <- rep(1, ny * ntrees)
      Votes <- factor(c(t(oobVotes)), levels = obj$Levels)
      Votes <- as.integer(Votes) + nC * rep(0:(ny - 1), rep(ntrees, ny))
      Votes <- aggregate(c(rep(0, ny * nC), weights), by = list(c(1:(ny * nC), Votes)), sum)[, 2]

      prob <- matrix(Votes, ny, nC, byrow = TRUE)
      pred <- max.col(prob) ## "random"
      oobPred <- obj$Levels[pred]
      obj$oobErr <- mean(yy != oobPred)

      # oobPred=rep(NA,noob)
      # for (i in 1:noob) {
      #  oobTable = table(oobVotes[i,])
      #  oobPred[i]=names(oobTable)[which.max(oobTable)];
      # }

      # oobErr=mean(oobPred!=Levels[y[idx]]);
      XConfusionMat <- table(oobPred, yy)
      class_error <- (rowSums(XConfusionMat) - diag(XConfusionMat)) / rowSums(XConfusionMat)
      XConfusionMat <- cbind(XConfusionMat, class_error)
      obj$oobConfusionMat <- XConfusionMat
    } else {
      oobPred <- rowMeans(oobVotes, na.rm = TRUE)
      obj$oobErr <- mean((oobPred - yy)^2) # / mean((yy - mean(yy))^2)
    }

    obj$predicted <- oobPred
  }

  # class(ppTree) <- append(class(ppTree),"ODRF")
  class(obj) <- c("ODRF", "prune.ODRF")
  return(obj)
}
