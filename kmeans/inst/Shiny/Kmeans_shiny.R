library(shiny)
library(umap)
library(caret)
library(purrr)
library(tidyverse)
library(kmeans)
library(plyr)
library(class)


ui <- shinyUI(fluidPage(
  
  titlePanel("Iris K means"),
  
  fluidRow(
  
    column(8,
           plotOutput("kmeansplot")),
    column(2,
           checkboxGroupInput('vars', 'Clustering Variables', names(iris[,-5]),
                              selected = names(iris[,c(1,2)]))),
  
    column(2,
           numericInput('clusters', 'Cluster count', 3,
                        min = 1, max = 9),
    ),
    
  ),
  titlePanel("Iris KNN"),
  numericInput('k', 'KNN K value', 3,
               min = 1, max = 9),
  textOutput('best_k'),
  plotOutput("knnplot"),
  plotOutput("PCA"),
  plotOutput("UMAP")
  
))

server <- shinyServer(function(input, output) {
  
  
  
  selectedData <- reactive({
    iris[, c(input$vars)]
  })
  

  
  clusters <- reactive({
    cluser_create(selectedData(), input$clusters)
  })
  
  output$kmeansplot <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    cluster_plot(selectedData(),
                 clusters()$cluster,
                 20,3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
  output$best_k <- renderText({
    paste("KNN Best K =",knn_best_k(c(input$vars)))
  })
  
  output$knnplot <- renderPlot({
    knn_plot(input$vars, input$k)
  })
  output$PCA <- renderPlot({
    x = data.matrix(iris[,input$vars])
    pca_bball <- prcomp(x)
    summary(pca_bball)
    
    data.frame(pca_bball$x[,1:2], Species = iris$Species) %>%
      ggplot(aes(PC1,PC2, fill = Species))+
      geom_point(cex=3, pch=21, ) +
      coord_fixed(ratio = 1)+
      labs(title = "PCA of Iris")
  })
  output$UMAP <- renderPlot({
    umap_iris <- umap(iris[,input$vars])
    data.frame(umap_iris$layout, Species = iris$Species) %>%
      ggplot(aes(X1,X2, fill = Species))+
      geom_point(cex=3, pch=21) +
      coord_fixed(ratio = 1)+
      labs(title = "Umap of Iris")
  })
  
})

shinyApp(ui = ui, server = server)
