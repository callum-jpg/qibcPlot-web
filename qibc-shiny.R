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
meta.nms <- nms[grepl('Meta', nms)]
condition_names <- levels(input_data[, get(metadata)])
trans <- c('linear', 'log10')

ui <- fluidPage(
  titlePanel('QIBC'),
  fluidRow(
    column(12, align = 'center', plotlyOutput('qibcPlot', height = '500px', width = '700px'))
    ),
  fluidRow(
    column(6,
           selectInput('x', 'X', choices = nms, selected = x.axis, width = '100%'),
           #br(),
           h3('use metadata?'),
           checkboxInput('meta_check', 'Use metadata?', FALSE, width = '100%'),
           #br(),
           selectInput('meta', 'Condition Name', ""),
           sliderInput('xlim', 'X min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           selectInput('colour', 'Point colour', choices = nms, selected = point.colour, width = '100%')),
    column(6, 
           selectInput('y', 'Y', choices = nms, selected = y.axis, width = '100%'),
           selectInput('meta_col', 'Metadata Column', "", width = "100%"),
           selectInput('ytrans', 'y transformation', choices = trans, selected = 'log10', width = '100%'),
           sliderInput('ylim', 'y min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           sliderInput('colour_lim', 'colour min/max', min = 1, max = 3000, value = c(1, 100), width = '100%'),
           numericInput('xmin', 'x min', value = 1),
           textOutput('test_print'))
  )

)

server <- function(input, output, session) {
  
  output$test_print <- renderText ({
    paste('print test:', input$meta_col == input$meta)
  })
  observe({
    ## Update meta_col with column names containing Metadata if meta_check is TRUE
    {if(input$meta_check == TRUE)updateSelectInput(session, 'meta_col' , choices = meta.nms)
      else updateSelectInput(session, 'meta_col', choices = "")}
  })
  
  observe({
    ## Update meta_col with column names containing Metadata if meta_check is TRUE
    {if(nchar(input$meta_col) >= 1)updateSelectInput(session, 'meta' , choices = levels(input_data[,get(input$meta_col)]))
      else updateSelectInput(session, 'meta' , choices = "")}
  })
  
  # observe({
  #   ## Updating a xlim slider based on condition selected
  #   updateSliderInput(session, 'xlim', value = c(round(min(input_data[get(metadata) == input$meta][[input$x]])),
  #                                                round(max(input_data[get(metadata) == input$meta][[input$x]]))), 
  #                     min = round(min(input_data[get(metadata) == input$meta][[input$x]])),
  #                     max = round(max(input_data[get(metadata) == input$meta][[input$x]]))
  #   )
  # })
  # observe({
  #   ## Updating a xlim slider based on condition selected
  #   updateSliderInput(session, 'ylim', value = c(round(min(input_data[get(metadata) == input$meta][[input$y]])),
  #                                                round(max(input_data[get(metadata) == input$meta][[input$y]]))), 
  #                     min = round(min(input_data[get(metadata) == input$meta][[input$y]])),
  #                     max = round(max(input_data[get(metadata) == input$meta][[input$y]]))
  #   )
  # })
  # observe({
  #   ## Updating a point colour limit slider based on condition selected
  #   updateSliderInput(session, 'colour_lim', value = c(round(min(input_data[get(metadata) == input$meta][[input$colour]])),
  #                                                round(max(input_data[get(metadata) == input$meta][[input$colour]]))), 
  #                     min = round(min(input_data[get(metadata) == input$meta][[input$colour]])),
  #                     max = round(max(input_data[get(metadata) == input$meta][[input$colour]]))
  #   )
  # })
  # observe({
  #   # For updating numeric field
  #   updateNumericInput(session, 'xmin', value = round(min(input_data[get(metadata) == input$meta][[input$x]])),
  #                      min = round(min(input_data[get(metadata) == input$meta][[input$x]])),
  #                      max = round(max(input_data[get(metadata) == input$meta][[input$x]])))
  # })
  # 
  dataset <- reactive({
    input_data[
      # If metadata is selected, filter dataset based on metadata values
      {if(nchar(input$meta) >= 1)get(input$meta_col) == input$meta
      # else, plot all available data
      else '' %like% ''}
    & get(input$x) %inrange% input$xlim
    & get(input$y) %inrange% input$ylim
    & get(input$colour) %inrange% input$colour_lim
    ]
  })
  
  output$qibcPlot <- renderPlotly({
    plot <- ggplot(dataset(), 
                   aes_string(x = input$x, 
                              y = input$y)) +
      # First geom_point added to give second geom_points an outline
      geom_point(shape = 21, size = 2, stroke = 0.5, colour = "#aaaaaa") +
      # Second geom_point
      geom_point(shape = 21, size = 1, 
                 #colour = "#aaaaaa", 
                 stroke = 1,
                 # Fill does not work within plotly
                 # https://github.com/ropensci/plotly/issues/1234
                 # Use colour (outline of pch 21) and set the stroke to occupy the whole geom_point
                 # This mimics fill, but loses the ability to add a point outline
                 aes(colour = get(input$colour))) + 

      scale_colour_gradient(trans = 'pseudo_log', low = 'gray', high = 'red') +
      {if(input$ytrans == 'log10')scale_y_log10()} + # Select ylog10
      {if(input$ytrans == 'linear')scale_y_continuous()} # Select y continuous
  })
}


shinyApp(ui, server)




