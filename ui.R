library(ggplot2)
library(plotly)
library(shiny)
library(data.table)
library(RColorBrewer)
library(rsconnect) # For hosting shiny app

input_file <- 'example_data/shiny_data.csv'

x.axis <- 'Total_DAPI_Intensity'
y.axis <-  'Mean_PCNA_Intensity'
point.colour <- 'Mean_γH2AX_intensity'
metadata <- 'Condition'


input_data <- fread(input_file, stringsAsFactors = TRUE)

nms <- names(input_data)
intensity_values <- nms[!nms %in% c('V1', 'Condition')] # Remove V1 and Condition from selectable plotting columns
condition_names <- levels(input_data[, get(metadata)]) # Extract condition names from metadata column
trans <- c('linear', 'log10')

# Plotting colours
# Orange, red, blue, pink, purple, green, gray
colour.palette <- c('#ffb86c', '#ff5555', '#8be9fd', '#ff79c6', '#bd93f9', '#50fa7b', '#ebebeb')
gray.to.red <- colorRampPalette(c(colour.palette[c(7, 1, 2)]))

ui <- fluidPage(
  titlePanel('qibcPlot'),
  mainPanel('Quantative Image Based Cytometry (QIBC) data for TARG1 KO cells with +DarT WT or -DarT WT conditions.', width = '100%'),
  mainPanel('This is a demo version for the Shiny app you can run locally with your own QIBC data.', width = '100%'),
  fluidRow(
    column(12, align = 'center', plotlyOutput('qibcPlot', height = '500px', width = '700px'))
  ),
  fluidRow(
    column(6,
           selectInput('x', 'X axis', choices = intensity_values, selected = x.axis, width = '100%'),
           selectInput('meta', 'Condition Name', choices = condition_names, width = "100%"),
           sliderInput('xlim', 'X min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           sliderInput('colour_lim', 'colour min/max', min = 1, max = 3000, value = c(1, 100), width = '100%'),
           br()),
    
    column(6, 
           selectInput('y', 'Y axis', choices = intensity_values, selected = y.axis, width = '100%'),
           selectInput('colour', 'Point colour', choices = intensity_values, selected = point.colour, width = '100%'),
           sliderInput('ylim', 'y min/max', min = 1, max = 1e7, value = c(1, 1e7), width = '100%'),
           selectInput('ytrans', 'y transformation', choices = trans, selected = 'log10', width = '100%'),
           br()),
  )
)