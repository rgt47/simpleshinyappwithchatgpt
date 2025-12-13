library(shiny)
library(bslib)
library(bsicons)
library(palmerpenguins)
library(ggplot2)
library(corrgram)
library(dplyr)

# Load the dataset
# Remove rows with NA values for simplicity
data <- na.omit(penguins)

# Helper function for tooltips
# This function wraps a given input widget and adds a tooltip icon
# that displays help text on hover.
add_tooltip <- function(input_ui, tooltip_text) {
  input_ui$children[[1]] <- div(
    input_ui$children[[1]], # Original label of the input
    span(
      bs_icon("info-circle-fill"), # Bootstrap info icon
      class = "tooltip-icon ms-2", # Add spacing with `ms-2` (margin start)
      `data-bs-toggle` = "tooltip", # Enable tooltip functionality
      `data-bs-placement` = "right", # Position tooltip to the right
      title = tooltip_text # Tooltip text
    ),
    style = "display: flex; align-items: center;" # Align icon with label
  )
  input_ui
}

# INPUT MODULE
# Creates UI for user inputs, including boxplot and scatterplot controls.
inputsUI <- function(id) {
  ns <- NS(id) # Namespace to avoid ID conflicts
  tagList(
    h4("Boxplot Controls"),
    add_tooltip(
      selectInput(
        ns("xvar"), # Continuous variable for boxplot
        "Continuous Variable for Boxplot:",
        choices = names(data)[3:6]
      ),
      "Select a continuous variable to display on the Y-axis of the boxplot."
    ),
    add_tooltip(
      selectInput(
        ns("groupvar"), # Grouping variable for boxplot
        "Group by (Categorical Variable):",
        choices = c("species", "sex", "island"),
        selected = "species"
      ),
      "Select a categorical variable to group data in the boxplot."
    ),
    hr(),
    h4("Scatterplot Controls"),
    add_tooltip(
      selectInput(
        ns("scatter_x"), # X-axis variable for scatterplot
        "X-axis for Scatterplot:",
        choices = names(data)[3:6]
      ),
      "Select a variable for the X-axis of the scatterplot."
    ),
    add_tooltip(
      selectInput(
        ns("scatter_y"), # Y-axis variable for scatterplot
        "Y-axis for Scatterplot:",
        choices = names(data)[3:6]
      ),
      "Select a variable for the Y-axis of the scatterplot."
    ),
    add_tooltip(
      selectInput(
        ns("groupvar_scatter"), # Grouping variable for scatterplot
        "Group by (Scatterplot):",
        choices = c("species", "sex", "island"),
        selected = "species"
      ),
      "Select a variable to group points in the scatterplot by color."
    )
  )
}

# Server logic for the inputs module.
inputsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return all user inputs as a reactive object
    reactive(input)
  })
}

# BOXPLOT MODULE
# UI for the boxplot, which visualizes a continuous variable grouped by a
# categorical variable.
boxplotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("boxPlot"), height = "400px") # Plot with fixed height
}

# Server logic for rendering the boxplot.
boxplotServer <- function(id, data, inputs) {
  moduleServer(id, function(input, output, session) {
    output$boxPlot <- renderPlot({
      req(data(), inputs()) # Ensure data and inputs are available
      ggplot(
        data(),
        aes(
          x = .data[[inputs()$groupvar]], # Grouping variable
          y = .data[[inputs()$xvar]], # Continuous variable
          fill = .data[[inputs()$groupvar]] # Color by group
        )
      ) +
        geom_boxplot(alpha = 0.7) + # Add boxplot
        theme_minimal() + # Minimal theme for cleaner look
        labs(
          x = inputs()$groupvar, # X-axis label
          y = inputs()$xvar, # Y-axis label
          fill = inputs()$groupvar # Legend title
        ) +
        theme(legend.position = "bottom") # Position legend at the bottom
    })
  })
}

# SCATTERPLOT MODULE
# UI for the scatterplot, which visualizes relationships between two continuous
# variables, optionally grouped by a categorical variable.
scatterplotUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("scatterPlot"), height = "400px") # Plot with fixed height
}

# Server logic for rendering the scatterplot.
scatterplotServer <- function(id, data, inputs) {
  moduleServer(id, function(input, output, session) {
    output$scatterPlot <- renderPlot({
      req(data(), inputs()) # Ensure data and inputs are available
      ggplot(
        data(),
        aes(
          x = .data[[inputs()$scatter_x]], # X-axis variable
          y = .data[[inputs()$scatter_y]], # Y-axis variable
          color = .data[[inputs()$groupvar_scatter]] # Grouping variable
        )
      ) +
        geom_point(alpha = 0.7, size = 2) + # Add points
        geom_smooth(
          method = "lm", # Add linear regression line
          se = FALSE,
          aes(group = 1), # Global trend line
          color = "black",
          linetype = "dashed"
        ) +
        geom_smooth(method = "lm", se = FALSE) + # Group-specific trend lines
        theme_minimal() + # Minimal theme for cleaner look
        labs(
          x = inputs()$scatter_x, # X-axis label
          y = inputs()$scatter_y, # Y-axis label
          color = inputs()$groupvar_scatter # Legend title
        ) +
        theme(legend.position = "bottom") # Position legend at the bottom
    })
  })
}

# CORRELATION MATRIX MODULE
# UI for the correlation matrix, which visualizes pairwise correlations between
# numeric variables as a corrgram.
correlationUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("correlationMatrix"), height = "400px") # Plot with fixed height
}

# Server logic for rendering the correlation matrix.
correlationServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    output$correlationMatrix <- renderPlot({
      req(data()) # Ensure data is available
      numericData <- data()[, sapply(data(), is.numeric)] # Select numeric columns
      corrgram(
        numericData,
        order = TRUE, # Reorder variables for better visualization
        lower.panel = panel.shade, # Shading for lower triangle
        upper.panel = panel.pie, # Pie charts for upper triangle
        text.panel = panel.txt, # Variable names
        main = "Correlation Matrix" # Plot title
      )
    })
  })
}

# MAIN UI AND SERVER
# Define the app's layout and behavior.
ui <- fluidPage(
  theme = bs_theme(bootswatch = "litera"), # Apply a bslib theme
  titlePanel("Palmer Penguins Explorer"), # App title
  fluidRow(
    column(3, inputsUI("inputs"), style = "overflow-y: auto; max-height: 600px;"),
    column(
      6,
      fluidRow(
        column(12, h4("Boxplot"), boxplotUI("boxplot"), style = "height: 50%;"),
        column(12, h4("Scatterplot"), scatterplotUI("scatterplot"), style = "height: 50%;")
      ),
      style = "overflow-y: auto;"
    ),
    column(3, h4("Correlation Matrix"), correlationUI("correlation"))
  )
)

server <- function(input, output, session) {
  # Reactive dataset
  filteredData <- reactive({
    data
  })
  
  # Call modules
  inputs <- inputsServer("inputs")
  boxplotServer("boxplot", filteredData, inputs)
  scatterplotServer("scatterplot", filteredData, inputs)
  correlationServer("correlation", filteredData)
}

shinyApp(ui, server)
