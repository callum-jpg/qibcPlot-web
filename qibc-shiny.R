library(ggplot2)
library(plotly)
library(shiny)
library(data.table)

input_file <- 'example_data/Nuclei.csv'
x.axis <- 'Intensity_IntegratedIntensity_rescaledapichannel'
y.axis <-  'Intensity_MeanIntensity_rescalecy5channel'
metadata <- 'Metadata_FolderName'

input_data <- fread(input_file, stringsAsFactors = TRUE)

nms <- names(input_data)
condition_names <- levels(input_data[, get(metadata)])

ui <- fluidPage(
  titlePanel('QIBC'),
  fluidRow(
    column(12, align = 'center', plotlyOutput('qibcPlot', height = '500px', width = '500px'))
    ),
  fluidRow(
    column(6,
           selectInput('x', 'X', choices = nms, selected = x.axis, width = '100%'),
           selectInput('meta', 'Condition Name', choices = condition_names),
           sliderInput('xlim', 'X min/max', min = 1, max = 1e7, value = c(1, 1e7))),
    column(6, 
           selectInput('y', 'Y', choices = nms, selected = y.axis, width = '100%'),
           numericInput('xmin', 'x min', value = 10, min = 1, max = 10))
  )

)

server <- function(input, output, session) {
  
  ## Updating a slider based on condition selected
  observe({
    val <- input$x
    updateSliderInput(session, 'xlim', value = c(round(min(input_data[get(metadata) == input$meta][[input$x]])),
                                                 round(max(input_data[get(metadata) == input$meta][[input$x]]))), 
                      min = round(min(input_data[get(metadata) == input$meta][[input$x]])),
                      max = round(max(input_data[get(metadata) == input$meta][[input$x]]))
    )
  })
  
  dataset <- reactive({
    input_data[get(metadata) == input$meta & input$x >= 4e6]
  })
  
  output$qibcPlot <- renderPlotly({
    plot <- ggplot(dataset(), 
                   aes_string(x = input$x, 
                              y = input$y)) +
      geom_point(shape = 21, size = 3, colour = "#aaaaaa", stroke = 0.5)
  })
  
    
}


shinyApp(ui, server)




