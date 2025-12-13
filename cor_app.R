shiny::runApp(
  list(
    ui = pageWithSidebar(

      headerPanel("TEST"),

      sidebarPanel(
        helpText('This matrix is quite nice:')
      ),

      mainPanel(    
        uiOutput('matrix')     
      )
    )
    , 
    server = function(input,output){
      library(googleVis)
      output$matrix <- renderGvis({
        df <- as.data.frame(matrix(rnorm(9),nrow=3))      
        rownames(df) <- c('a','b','c')
        gvisTable(df);

      })
    }
  )
)
