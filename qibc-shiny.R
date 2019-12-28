library(ggplot2)
library(plotly)
library(shiny)
library(data.table)

input_file <- 'example_data/Nuclei.csv'
x.axis <- 'Intensity_IntegratedIntensity_rescaledapichannel'
y.axis <-  'Intensity_MeanIntensity_rescalecy5channel'
point.colour <- 'Intensity_MeanIntensity_rescalemcherrychannel'
metadata <- 'Metadata_FolderName'

input_data <- fread(input_file, stringsAsFactors = TRUE)

nms <- names(input_data)
condition_names <- levels(input_data[, get(metadata)])
trans <- c('linear', 'log10')

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
           selectInput('colour', 'Point colour', choices = nms, selected = point.colour, width = '100%'),
           sliderInput('ylim', 'y min/max', min = 1, max = 1e7, value = c(1, 1e7)),
           selectInput('ytrans', 'y transformation', choices = trans, selected = 'log10', width = '100%'),
           numericInput('xmin', 'x min', value = 1),
           textOutput('test_print'))
  )

)

server <- function(input, output, session) {
  
  output$test_print <- renderText ({
    paste('print test:', input$x)
  })
  
  
  observe({
    ## Updating a xlim slider based on condition selected
    updateSliderInput(session, 'xlim', value = c(round(min(input_data[get(metadata) == input$meta][[input$x]])),
                                                 round(max(input_data[get(metadata) == input$meta][[input$x]]))), 
                      min = round(min(input_data[get(metadata) == input$meta][[input$x]])),
                      max = round(max(input_data[get(metadata) == input$meta][[input$x]]))
    )
  })
  
  observe({
    ## Updating a xlim slider based on condition selected
    updateSliderInput(session, 'ylim', value = c(round(min(input_data[get(metadata) == input$meta][[input$y]])),
                                                 round(max(input_data[get(metadata) == input$meta][[input$y]]))), 
                      min = round(min(input_data[get(metadata) == input$meta][[input$y]])),
                      max = round(max(input_data[get(metadata) == input$meta][[input$y]]))
    )
  })
  
  
  observe({
    # For updating numeric field
    updateNumericInput(session, 'xmin', value = round(min(input_data[get(metadata) == input$meta][[input$x]])),
                       min = round(min(input_data[get(metadata) == input$meta][[input$x]])),
                       max = round(max(input_data[get(metadata) == input$meta][[input$x]])))
  })
  
  dataset <- reactive({
    input_data[get(metadata) == input$meta
    & get(input$x) %inrange% input$xlim
    & get(input$y) %inrange% input$ylim] # Filtering data based on x-axis
  })
  
  output$qibcPlot <- renderPlotly({
    plot <- ggplot(dataset(), 
                   aes_string(x = input$x, 
                              y = input$y)) +
      geom_point(shape = 21, size = 3, colour = "#aaaaaa", stroke = 0.5) +
      {if(input$ytrans == 'log10')scale_y_log10()} + # Select y log10
      {if(input$ytrans == 'linear')scale_y_continuous()} # Select ylog continuous
  })
  
    
}


shinyApp(ui, server)




