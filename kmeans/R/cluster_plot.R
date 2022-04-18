#' Plots kmeans clusters
#' 
#'
#' @param data The data frame or tibble used in the plot
#' @param cluster_num Number of clusters
#' @param pc Used in place of pch to plot
#' @param ce used in place of cex to plot
#'
#' @return output A description of the object the function outputs 
#'
#' 
#'
#' 
#' 

cluster_plot <- function(data, cluster_num, pc, ce){
  plot(data,
       col = cluster_num,
       pch = pc, 
       cex = ce)
}
