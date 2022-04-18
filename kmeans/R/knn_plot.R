library(plyr)
library(ggplot2)
library(class)
library(caret)



knn_plot <- function(v1, v2, k){
  
set.seed(123)
# Create training and testing data sets
idx = sample(1:nrow(iris), size = 100)
train.idx = 1:nrow(iris) %in% idx
test.idx =  ! 1:nrow(iris) %in% idx

train = iris[train.idx, 1:5]
test = iris[test.idx, 1:5]


# Do knn
fit = knn3(Species ~ ., data = train, k = k)
y_hat_knn <- predict(fit, test, type = "class")


# Create a dataframe to simplify charting
plot.df = data.frame(test, predicted = y_hat_knn)

# Use ggplot
# 2-D plots example only



return(ggplot(plot.df, aes_string(v1, v2, color = "predicted", fill = "predicted")) + 
  geom_point(size = 5) )+
  ggtitle("KNN Plot") +
  theme(plot.title = element_text(hjust = 0.5))
  

}

