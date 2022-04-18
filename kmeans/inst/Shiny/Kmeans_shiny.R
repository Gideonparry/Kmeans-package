library(shiny)
library(ggplot2)

ui <- shinyUI(fluidPage(
  
  titlePanel("Iris K means"),
  
  fluidRow(
    column(2,
           textInput(inputId = "number", label = "number of selectInput",value = 2)
    ),
    column(8,
           plotOutput("kmeansplot")),
    column(2,
           uiOutput(outputId = "putselect")),
    column(2,
           numericInput('clusters', 'Cluster count', 3,
                        min = 1, max = 9),
    ),
    column(2,
           numericInput('k', 'KNN K value', 3,
                        min = 1, max = 9),
    ),
    
    #column(2,
     #      textOutput('best_k', 'Best K value')
    #),
    
    column(8,
           plotOutput("knnplot"))
  )
))

server <- shinyServer(function(input, output) {
  
  
  
  output$putselect = renderUI(
    if(input$number != 0 ){
      lapply(1:(input$number), function(i){
        selectInput(inputId = paste0("var",i), label = paste0("input ",i), choices   = names(iris))
      })
    }
  )
  
  selectedData <- reactive({
    iris[, c(input$var1, input$var2)]
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
  
  
  
  output$knnplot <- renderPlot({
    knn_plot(input$var1,input$var2, input$k)
  })
})

shinyApp(ui = ui, server = server)
