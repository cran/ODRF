## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----classification and regression--------------------------------------------
library(ODRF)
data(seeds, package = "ODRF")
set.seed(12)
train <- sample(1:209, 150)
seeds_train <- data.frame(seeds[train, ])
seeds_test <- data.frame(seeds[-train, ])
forest <- ODRF(varieties_of_wheat ~ ., seeds_train,
  split = "gini",
  parallel = FALSE
)
pred <- predict(forest, seeds_test[, -8])
(e.forest <- mean(pred != seeds_test[, 8]))
data(body_fat, package = "ODRF")
train <- sample(1:252, 200)
bodyfat_train <- data.frame(body_fat[train, ])
bodyfat_test <- data.frame(body_fat[-train, ])
tree <- ODT(Density ~ ., bodyfat_train, split = "mse")
pred <- predict(tree, bodyfat_test[, -1])
(e.tree <- mean((pred - bodyfat_test[, 1])^2))

## ----online-------------------------------------------------------------------
set.seed(17)
index <- sample(nrow(seeds_train), floor(nrow(seeds_train) / 2))
forest1 <- ODRF(varieties_of_wheat ~ ., seeds_train[index, ],
  split = "gini", parallel = FALSE
)
pred <- predict(forest1, seeds_test[, -8])
(e.forest.1 <- mean(pred != seeds_test[, 8]))
forest2 <- online(forest1, seeds_train[-index, -8], seeds_train[-index, 8])
pred <- predict(forest2, seeds_test[, -8])
(e.forest.online <- mean(pred != seeds_test[, 8]))
index <- seq(floor(nrow(bodyfat_train) / 2))
tree1 <- ODT(Density ~ ., bodyfat_train[index, ], split = "mse")
pred <- predict(tree1, bodyfat_test[, -1])
(e.tree.1 <- mean((pred - bodyfat_test[, 1])^2))
tree2 <- online(tree1, bodyfat_train[-index, -1], bodyfat_train[-index, 1])
pred <- predict(tree2, bodyfat_test[, -1])
(e.tree.online <- mean((pred - bodyfat_test[, 1])^2))

## ----prune--------------------------------------------------------------------
set.seed(4)
bodyfat_train <- rbind(as.matrix(bodyfat_train), matrix(rnorm(3000 * 5), 5 * 200, 15))
seeds_train <- rbind(as.matrix(seeds_train), matrix(rnorm(1200 * 5), 5 * 150, 8))
bodyfat_train[-seq(200), 1] <- sample(bodyfat_train[seq(200), 1], 5 * 200,
  replace = TRUE
)
seeds_train[-seq(150), 8] <- sample(seeds_train[seq(150), 8], 5 * 150,
  replace = TRUE
)
index <- sample(nrow(seeds_train), floor(nrow(seeds_train) / 2))
forest1 <- ODRF(seeds_train[index, -8], seeds_train[index, 8],
  split = "gini", parallel = FALSE
)
pred <- predict(forest1, seeds_test[, -8])
(e.forest.1 <- mean(pred != seeds_test[, 8]))
forest2 <- prune(forest1, seeds_train[-index, -8], seeds_train[-index, 8],
  useOOB = FALSE
)
pred <- predict(forest2, seeds_test[, -8])
(e.forest.prune1 <- mean(pred != seeds_test[, 8]))
forest3 <- prune(forest1, seeds_train[index, -8], seeds_train[index, 8])
pred <- predict(forest3, seeds_test[, -8])
(e.forest.prune2 <- mean(pred != seeds_test[, 8]))
index <- sample(nrow(bodyfat_train), floor(nrow(bodyfat_train) / 2))
tree1 <- ODT(bodyfat_train[index, -1], bodyfat_train[index, 1], split = "mse")
pred <- predict(tree1, bodyfat_test[, -1])
(e.tree.1 <- mean((pred - bodyfat_test[, 1])^2))
tree2 <- prune(tree1, bodyfat_train[-index, -1], bodyfat_train[-index, 1])
pred <- predict(tree2, bodyfat_test[, -1])
(e.tree.prune <- mean((pred - bodyfat_test[, 1])^2))

## ----print--------------------------------------------------------------------
data(iris, package = "datasets")
tree <- ODT(Species ~ ., data = iris)
print(tree)
party.tree <- as.party(tree, data = iris)
print(party.tree)
forest <- ODRF(Species ~ ., data = iris, parallel = FALSE)
print(forest)

## ----plot, fig.height=4.0,fig.width=7.0---------------------------------------
plot(tree)

