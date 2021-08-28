library(DT)
library(dplyr)
library(shinyjs)


shinyServer(function(input,output,session){
  


  data <- reactiveVal()

  listen <- reactive({list(input$file,input$na,input$delete,input$merge,input$delete_r)})
  
  observeEvent(input$file,{
    if(!is.null(input$file)){
      print(input$file)
      file <- input$file
      d <- read.csv(file$datapath,header=TRUE)
      rownames(d) <- NULL
      data(d)
    }
  })
  
  observeEvent(listen(),{
    result <- data()
    if(!is.null(input$file)){
      output$display_file <- DT::renderDataTable({
      if(input$na){
        result <- result %>% tidyr::drop_na()
        output$display_file <- DT::renderDataTable({result})
        output$str <- renderPrint({str(result)})
        output$desc <-renderPrint({psych::describe(result)})
        data(result)
      }
      
      
      else if(length(input$delete_col) != 0 & input$delete){
        result <- as.data.frame(result[,-which(names(result) %in% input$delete_col)])
        data(result)
        output$display_file <- DT::renderDataTable({result})
      }
        
      else if(length(input$delete_row) != 0 & input$delete_r){
        for(row in input$delete_row){
          result <- result[-as.numeric(row),]
          print(row)
        }
        data(result)
      }

      
      else if(input$merge & input$merge_col_name != ""){
        result[,input$merge_col_name] <- NA
        for(name in input$merge_col){
          result[,input$merge_col_name] <- paste(result[,input$merge_col_name], result[,name])
        }
        result <- as.data.frame(result[,-which(names(result) %in% input$merge_col)])
        data(result)
        output$display_file <- DT::renderDataTable({result})

        
      }else{
        output$display_file <- DT::renderDataTable({result})
        output$str <- renderPrint({str(result)})
        output$desc <-renderPrint({psych::describe(result)})

      }
      })

      

    }
  })
  
  observeEvent(data(),{
    
  output$na <- renderUI({
    checkboxInput("na","Drop na")
  })
  output$merge_col <- renderUI({
    selectInput("merge_col","Select columns to merge",choices=colnames(data()), multiple=TRUE)
    })
  output$delete_col <- renderUI({
    selectInput("delete_col","Select columns to delete",choices=colnames(data()),multiple=TRUE)
    })

  
  output$merge_col_name <- renderUI({
    textInput("merge_col_name","Enter merge name")
  })
  
  output$delete <- renderUI({
    actionButton("delete","delete columns")
  })
  
  output$merge <- renderUI({
    actionButton("merge","merge columns")
  })
  
  output$delete_row <- renderUI({
    selectInput("delete_row","Select rows to delete",choices=as.numeric(rownames(data())),multiple=TRUE)
  })
  
  output$delete_r <- renderUI({
    actionButton("delete_r","delete rows")
  })
  
  
  
  })
  

  
  
  
})