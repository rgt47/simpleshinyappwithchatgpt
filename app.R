library(shiny)
library(palmerpenguins)
library(ggplot2)

# Load the dataset
data <- na.omit(penguins) # Remove rows with NA for simplicity

ui <- fluidPage(
  titlePanel("Palmer Penguins Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("xvar", "Continuous Variable for Boxplot:", choices = names(data)[3:6]),
      selectInput("groupvar", "Group by (Categorical Variable):", 
                  choices = c("species", "sex", "island"), selected = "species"),
      hr(),
      selectInput("scatter_x", "X-axis for Scatterplot:", choices = names(data)[3:6]),
      selectInput("scatter_y", "Y-axis for Scatterplot:", choices = names(data)[3:6]),
      selectInput("groupvar_scatter", "Group by (Scatterplot):", 
                  choices = c("species", "sex", "island"), selected = "species")
    ),
    mainPanel(
      plotOutput("boxPlot"),
      plotOutput("scatterPlot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output) {
  # Reactive dataset (no filtering applied anymore)
  filteredData <- reactive({
    data
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    req(filteredData()) # Ensure data is available
    ggplot(filteredData(), aes_string(x = input$groupvar, y = input$xvar, fill = input$groupvar)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(x = input$groupvar, y = input$xvar, fill = input$groupvar) +
      theme(legend.position = "bottom")
  })
  
  # Scatterplot with grouping
  output$scatterPlot <- renderPlot({
    req(filteredData()) # Ensure data is available
    ggplot(filteredData(), aes_string(x = input$scatter_x, y = input$scatter_y, color = input$groupvar_scatter)) +
      geom_point(alpha = 0.7, size = 2) +
      theme_minimal() +
      labs(x = input$scatter_x, y = input$scatter_y, color = input$groupvar_scatter) +
      theme(legend.position = "bottom")
  })
  
  # Summary of the dataset
  output$summary <- renderPrint({
    req(filteredData()) # Ensure data is available
    summary(filteredData())
  })
}

shinyApp(ui, server)
