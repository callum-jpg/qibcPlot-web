library(ggplot2)
library(plotly)
library(shiny)
library(data.table)
library(RColorBrewer)

# for deployment-branch

input_file <- 'example_data/shiny_data.csv'

x.axis <- 'Total_DAPI_Intensity'
y.axis <-  'Mean_PCNA_Intensity'
point.colour <- 'Mean_γH2AX_intensity'
metadata <- 'Condition'

input_data <- fread(input_file, stringsAsFactors = TRUE)

nms <- names(input_data)
#meta.nms <- nms[grepl('Meta', nms)]
meta.nms <- nms
condition_names <- levels(input_data[, get(metadata)])
trans <- c('linear', 'log10')

# Plotting colours
# Orange, red, blue, pink, purple, green, gray
colour.palette <- c('#ffb86c', '#ff5555', '#8be9fd', '#ff79c6', '#bd93f9', '#50fa7b', '#ebebeb')
gray.to.red <- colorRampPalette(c(colour.palette[c(7, 1, 2)]))

ui <- fluidPage(
  titlePanel('qibcPlot'),
  fluidRow(
    column(12, align = 'center', plotlyOutput('qibcPlot', height = '500px', width = '700px'))
  ),
  fluidRow(
    column(6,
           selectInput('x', 'X axis', choices = nms, selected = x.axis, width = '100%'),
           selectInput('meta', 'Condition Name', choices = condition_names, width = "100%"),
           sliderInput('xlim', 'X min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           sliderInput('colour_lim', 'colour min/max', min = 1, max = 3000, value = c(1, 100), width = '100%'),
           br()),
           
    column(6, 
           selectInput('y', 'Y axis', choices = nms, selected = y.axis, width = '100%'),
           selectInput('colour', 'Point colour', choices = nms, selected = point.colour, width = '100%'),
           sliderInput('ylim', 'y min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           selectInput('ytrans', 'y transformation', choices = trans, selected = 'log10', width = '100%'),
           br()),
  )
)

server <- function(input, output, session) {

  output$test_print <- renderText ({
    paste('print test:', nchar(input$xmin))
  })

  
  observe({
    ## Updates min/max slider based on the min max values of the entire dataset
        updateSliderInput(session, 'xlim', value = c(round(min(input_data[,get(input$x)])),
                                                     round(max(input_data[,get(input$x)]))),
                          min = round(min(input_data[,get(input$x)])),
                          max = round(max(input_data[,get(input$x)])))
  })
  
  observe({
    ## Updates min/max slider based on the min max values of the entire dataset
        updateSliderInput(session, 'ylim', value = c(round(min(input_data[,get(input$y)])),
                                                     round(max(input_data[,get(input$y)]))),
                          min = round(min(input_data[,get(input$y)])),
                          max = round(max(input_data[,get(input$y)])))
  })
  
  observe({
    ## ## Updates min/max slider based on the min max values of the entire dataset
        updateSliderInput(session, 'colour_lim', value = c(round(min(input_data[,get(input$colour)])),
                                                           round(max(input_data[,get(input$colour)]))),
                          min = round(min(input_data[,get(input$colour)])),
                          max = round(max(input_data[,get(input$colour)])))
  })
  
  dataset <- reactive({
    input_data[get(metadata) == input$meta
               & get(input$x) %inrange% input$xlim
               & get(input$y) %inrange% input$ylim 
               & get(input$colour) %inrange% input$colour_lim] 
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
      scale_colour_gradientn(colours = gray.to.red(3),
                           values = c(0, 0.6, 1)
                           #limits = colour.mm,
                           #oob = scales::squish
      ) +
      {if(input$ytrans == 'log10')scale_y_log10()} + # Select ylog10
      {if(input$ytrans == 'linear')scale_y_continuous()} + # Select y continuous
      labs(colour = paste(input$colour)) +
      # Theme
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle=20, hjust=1),
        # Remove gridlines
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color="gray70", size = 1),
        axis.line.y = element_line(color="gray70", size = 1))
  })
}


shinyApp(ui, server)

