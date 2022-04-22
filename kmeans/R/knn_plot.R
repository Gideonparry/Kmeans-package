#' Plots KNN of iris data
#' 
#' Take checked colums from the iris dataset and plots the kmeans output with the top 2 selected on the x 
#' and y axis
#'
#' @param varlist The list of variables

#' @return Graph of kmeans colored by species
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' knn_best_k(c('Sepal.Length','Sepal.Width','Petal.Length')) 



knn_plot <- function(vlist, k){
  
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



return(ggplot(plot.df, aes_string(vlist[1],vlist[2], color = "predicted", fill = "predicted")) + 
  geom_point(size = 5) )+
  ggtitle("KNN Plot") +
  theme(plot.title = element_text(hjust = 0.5))
  

}

