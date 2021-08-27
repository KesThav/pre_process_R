library(DT)
library(dplyr)

shinyServer(function(input,output,session){
  
  output$message <- renderText({
    if(is.null(input$file)){
      paste("Upload a file")
    }
  })
  
  listen <- reactive({list(input$file,input$na)})
  observeEvent(listen(),{
    if(!is.null(input$file)){
      file <- input$file
      dataset <- read.csv(file$datapath,header=TRUE)
      output$display_file <- DT::renderDataTable({dataset})
      
      
      output$str <- renderPrint({str(dataset)})
      output$desc <-renderPrint({psych::describe(dataset)})
      
      if(input$na){
        data <- dataset %>% tidyr::drop_na()
        output$display_file <- DT::renderDataTable({data})
        output$str <- renderPrint({str(data)})
        output$desc <-renderPrint({psych::describe(data)})
      }
    }
  })
  
  
  
})