
knn_best_k <- function(){
ks <- seq(1, 100, 1)
set.seed(123)

idx = sample(1:nrow(iris), size = 100)
train.idx = 1:nrow(iris) %in% idx
test.idx =  ! 1:nrow(iris) %in% idx

train = iris[train.idx, 1:5]
test = iris[test.idx, 1:5]

library(purrr)
library(tidyverse)
accuracy <- map_df(ks, function(k){
  fit <- knn3(Species ~., data =train, k = k)
  y_hat <- predict(fit, train, type = "class")
  cm_train <- confusionMatrix(y_hat, train$Species)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, test, type = "class")
  cm_test <- confusionMatrix(y_hat, test$Species)
  test_error <- cm_test$overall["Accuracy"]
  tibble(train = train_error, test = test_error)
})

return(
ks[which.max(accuracy$test)]
)
}
knn_best_k()
