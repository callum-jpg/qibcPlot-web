library(ggplot2)
library(plotly)
library(shiny)
library(data.table)

input_file <- 'example_data/Nuclei.csv'
x.axis <- 'Intensity_IntegratedIntensity_rescaledapichannel'
y.axis <-  'Intensity_MeanIntensity_rescalecy5channel'

input_data <- fread(input_file, stringsAsFactors = TRUE)

input_data <- as.data.frame(input_data)

nms <- names(input_data)

ui <- fluidPage(
  titlePanel("QIBC"),
  sidebarPanel(
    selectInput('x', 'X', choices = nms, selected = x.axis),
    selectInput('y', 'Y', choices = nms, selected = y.axis)
  ),
mainPanel(
  plotlyOutput('qibcPlot', height = '600px')
  )
)

server <- function(input, output) {
  output$qibcPlot <- renderPlotly({
    plot <- ggplot(input_data[Metadata_FolderName == 'dt-0.1ug'], 
                   aes_string(x = input$x, 
                              y = input$y)) +
      geom_point(shape = 21, size = 3, colour = "#aaaaaa", stroke = 0.5)
  })
  
    
}


shinyApp(ui, server)




