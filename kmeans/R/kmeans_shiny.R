#' Runs a shiny app of kemans for given data.
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param data The data frame or tibble used in the shiny app.
#' 
#'
#'
#' @examples
#' kmeans_shiny(iris)

kmeans_shiny <- function (data = iris){
  library(shiny)
  ui <- fluidPage(
    
    # Put a titlePanel here
    titlePanel("k-means clustering"),
    
    sidebarLayout(
      # Sidebar. Put your inputs inside the sidebarPanel
      sidebarPanel(
        selectInput('xcol', 'X Variable', names(data)),
        selectInput('ycol', 'Y Variable', names(data),
                    selected=names(data)[[2]]),
        numericInput('clusters', 'Cluster count', 3,
                     min = 1, max = 9)
      ),
      
      # Main panel. put your output plot here
      mainPanel(
        plotOutput('plot1')
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    selectedData <- reactive({
      data[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      cluser_create(selectedData(), input$clusters)
    })
    
    output$plot1 <- renderPlot({
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      
      par(mar = c(5.1, 4.1, 0, 1))
      cluster_plot(selectedData(),
                   clusters()$cluster,
                   20,3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
  }
  
  shinyApp(ui = ui, server = server)
}

