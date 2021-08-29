library(DT)
library(dplyr)
library(shinyjs)
library(shinyWidgets)


shinyServer(function(input,output,session){
  
#Preprocess

  data <- reactiveVal()
  counter <- reactiveVal()

  listen <- reactive({list(counter(),input$na,input$delete,input$merge,input$delete_r,input$reset,input$convert_val,input$convert_type,input$col_to_convert)})
  
  observeEvent(input$file,{
    if(!is.null(input$file)){
      req(input$file)
      file <- input$file
      d <- read.csv(file$datapath,header=TRUE)
      rownames(d) <- NULL
      data(as.data.frame(d))
    }
  })
  
  observeEvent(input$reset,{
    file <- input$file
    d <- read.csv(file$datapath,header=TRUE)
    rownames(d) <- NULL
    data(as.data.frame(d))
  })


  observeEvent(listen(),{
    tryCatch({
    result <- data()
    if(!is.null(input$file) & length(result) > 0){
      print(input$na)
      if(!is.null(input$na) & input$na){
        result <- result %>% tidyr::drop_na()
        data(result)
      }
      
      else if(length(input$delete_col) != 0 & input$delete){
        result <- as.data.frame(result[,-which(names(result) %in% input$delete_col)])
        data(result)
      }
      
      else if(length(input$delete_row) != 0 & input$delete_r){
        for(row in input$delete_row){
          result <- result[-as.integer(c(row)),]
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
        
      }else if(length(input$col_to_convert) != 0 & length(input$convert_type) != 0 & input$convert_val){
        
        if(input$convert_type == 'integer'){
          print(input$convert_type)
          result[,input$col_to_convert] <- as.integer(result[,input$col_to_convert])
          data(result)
          
        }else if(input$convert_type == 'double'){
          print(input$convert_type)
          result[,input$col_to_convert] <- as.double(result[,input$col_to_convert])
          data(result)
          
        }else if(input$convert_type == 'chr'){
          print(input$convert_type)
          result[,input$col_to_convert] <- as.character(result[,input$col_to_convert])
          data(result)
          
        }else{
          data(result)
        }
        
        
        
      }else{
        data(result)
      }
      
      output$display_file <- DT::renderDataTable({result})
      output$str <- renderPrint({str(result)})
      output$desc <-renderPrint({psych::describe(result)})
      }},    warning = function(warn){
        showNotification(paste0(warn), type = 'warning')
      },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    
},ignoreNULL=TRUE, ignoreInit=TRUE)
  
  
  
  observeEvent(data(), {
    
    if(length(data()) != 0){
      shinyjs::hide(id = "notif")
      shinyjs::show(id="hidden")
      shinyjs::show(id="hidden2")
    }else{
      shinyjs::show(id = "notif")
      shinyjs::hide(id="hidden")
      shinyjs::hide(id="hidden2")
    }
  })
  
  observeEvent(data(),{
    if(length(data()) > 0){
        output$na <- renderUI({
        checkboxInput("na","Drop na",value = FALSE)
      })
      output$merge_col <- renderUI({
        pickerInput("merge_col",label="Select columns to merge",choices=colnames(data()), multiple=TRUE)
        })
      output$delete_col <- renderUI({
        pickerInput("delete_col",label="Select columns to delete",choices=colnames(data()),multiple=TRUE)
        })
    
      
      output$merge_col_name <- renderUI({
        textInput("merge_col_name","Enter merge column's name")
      })
      
      output$delete <- renderUI({
        actionButton("delete","delete columns")
      })
      
      output$merge <- renderUI({
        actionButton("merge","merge columns")
      })
      
      output$delete_row <- renderUI({
        pickerInput("delete_row","Select rows to delete",choices=as.numeric(rownames(data())),multiple=TRUE)
      })
      
      output$delete_r <- renderUI({
        actionButton("delete_r","delete rows")
      })
      
      output$col_to_convert <- renderUI({
        pickerInput("col_to_convert","Select column to convert",choices=colnames(data()))
      })
      
      output$convert_type <- renderUI({
        pickerInput("convert_type","Select what to convert to",choices=c('integer','double','chr'))
      })
      
      output$convert_val <- renderUI({
        actionButton("convert_val","Convert")
      })
      
      output$show_unique <- renderUI({
        pickerInput("show_unique","Show unique",choices=colnames(data()),select=colnames(data())[1])
      })
      
      output$reset <- renderUI({
        actionButton("reset","Reset")
      })
      
      count <- 1
      counter(count)
    }


  
  })
  
  observeEvent(input$show_unique,{
    result <- data()
    output$unique <- renderPrint({unique(result[,input$show_unique])})
  })
  
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
#Analyze

  plots <- c("scatter plot","line plot","displot","bar chart","histogram","density plot","box plot","violin plot","pie chart","correlogram","Dendrogram")
  
  
  observeEvent(data(),{
    if(length(data()) > 0){

      output$plots <- renderUI({
        pickerInput("plot",label="Select your plot",choices=plots)
      })

      
      count <- 1
      counter(count)
    }
    
    
    
  })
  
  
  
  
})

  