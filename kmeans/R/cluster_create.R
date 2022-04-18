#' creates k means clusters
#' 
#' Takes variables and number of clusters and uses that to create the k means clusters
#'
#' @param cols The varibles of data uses
#' @param cluster_num Number of clusters
#'
#'
#'
#' 
#'
#' 
#' 

cluser_create <- function(cols, cluster_num){
  kmeans(cols, cluster_num)
}
