library(DT)
library(dplyr)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(caret)
library(datasets)
library(mltools)

shinyServer(function(input,output,session){
  
#Preprocess

  data <- reactiveVal()

  listen <- reactive({list(input$na,input$delete,input$merge,input$delete_r,input$reset,input$convert_val,input$convert_type,input$col_to_convert,input$col_to_encode,input$val_encode,input$encode_type)})
  
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
          result[,input$col_to_convert] <- as.integer(result[,input$col_to_convert])
          data(result)
          
        }else if(input$convert_type == 'double'){
          result[,input$col_to_convert] <- as.double(result[,input$col_to_convert])
          data(result)
          
        }else if(input$convert_type == 'chr'){
          result[,input$col_to_convert] <- as.character(result[,input$col_to_convert])
          data(result)
        }else if(input$convert_type == 'factor'){
          result[,input$col_to_convert] <- as.factor(result[,input$col_to_convert])
          data(result)
          
        }else{
          data(result)
        }
        
      }else if(length(input$col_to_encode) != 0 & input$val_encode){
        if(input$encode_type %in% "Numerical"){
          result[,input$col_to_encode] <- as.numeric(as.factor(result[,input$col_to_encode]))
          data(result)
        }else if(input$encode_type %in% "One hot"){
          col <- result[,input$col_to_encode]
          col <- as.data.frame(col)
          dummy <- dummyVars(" ~ .",data=col)
          col_encoded <- data.frame(predict(dummy, newdata = col))
          result <- result[,-which(names(result) %in% input$col_to_encode)]
          result <- cbind(result,col_encoded)
          data(result)
        
        
        }else{
          data(result)
        }
      }
      
      output$display_file <- DT::renderDataTable(result,extensions='Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','csv','excel','print')),editable=TRUE,selection='none')
      output$str <- renderPrint({str(result)})
      output$desc <-renderPrint({psych::describe(result)})
      }}, warning = function(warn){
        showNotification(paste0(warn), type = 'warning')
      },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    
},ignoreNULL=TRUE, ignoreInit=TRUE)
  
  observeEvent(input$display_file_cell_edit,{
    tryCatch({
      result <- data()
      result[input$display_file_cell_edit$row,input$display_file_cell_edit$col] <- input$display_file_cell_edit$value
      data(result)
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})


  })
  
  
  
  observeEvent(data(), {
    
    if(length(data()) != 0){
      shinyjs::hide(id = "notif")
      shinyjs::show(id="main_div")
    }else{
      shinyjs::show(id = "notif")
      shinyjs::hide(id="main_div")
    }
  })
  
  observeEvent(data(),{
    tryCatch({
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
        pickerInput("convert_type","Select what to convert to",choices=c('integer','double','chr','factor'))
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
      
      output$col_to_encode <- renderUI({
        pickerInput("col_to_encode","Select column to encode",choices=colnames(data()),select=colnames(data())[1])
      })
      
      output$encode_type <- renderUI({
        pickerInput("encode_type","Select type of encoding",choices=c("Numerical","One hot"),select="Normal")
      })
      
      
      output$val_encode <- renderUI({
        actionButton("val_encode","Encode")
      })
      
      
      output$reset <- renderUI({
        actionButton("reset","Reset")
      })
      
      output$data_rows <- renderInfoBox({
        infoBox(
          "Rows", length(rownames(data())), icon = icon("list"),
          color = "purple"
        )
      })
      
      output$data_columns <- renderInfoBox({
        infoBox(
          "Columns", length(colnames(data())), icon = icon("list"),
          color = "purple"
        )
      })
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    


  
  })
  
  observeEvent(input$show_unique,{
    result <- data()
    output$unique <- renderPrint({unique(result[,input$show_unique])})
  })
  
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
#Analyze

  plots <- c("scatter plot","bar chart","boxplot","violin plot","pie chart","correlogram")
  
  observeEvent(data(),{
    tryCatch({
    if(length(data()) > 0){

      output$select_plot <- renderUI({
        pickerInput("select_plot",label="Select your plot",choices=plots)
      })
      
      output$select_x <- renderUI({
        pickerInput("select_x",label="Select your x axis",choices=colnames(data()),selected="")
      })
      
      output$dodge <- renderUI({
        radioButtons("dodge", "Position",choices=c("dodge","identity"),selected="identity")
      })
      
      output$select_y <- renderUI({
        pickerInput("select_y",label="Select your y axis",choices=colnames(data()),selected="")
      })
      
      output$select_color <- renderUI({
        pickerInput("select_color",label="Select your label color",choices=c(colnames(data()),"None"),selected="")
      })
      
      output$plot_graph <- renderUI({
        actionButton("plot_graph","Plot")
      })

    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

    
  })
  
  observeEvent(input$select_plot,{
    tryCatch({
    if(input$select_plot %in% c("scatter plot","boxplot","violin plot")){
      
      shinyjs::show(id="x_value")
      shinyjs::show(id="y_value")
      shinyjs::hide(id="dodge")
      shinyjs::show(id="color")
      
    }
    
    else if(input$select_plot %in% c("bar chart")){
      
      shinyjs::show(id="x_value")
      shinyjs::hide(id="y_value")
      shinyjs::show(id="dodge")
      shinyjs::show(id="color")
      
    }else if(input$select_plot %in% c("pie chart")){
      shinyjs::hide(id="x_value")
      shinyjs::show(id="y_value")
      shinyjs::hide(id="dodge")
      shinyjs::hide(id="color")
      
    }else if(input$select_plot %in% c("correlogram")){
      shinyjs::hide(id="x_value")
      shinyjs::hide(id="y_value")
      shinyjs::hide(id="dodge")
      shinyjs::hide(id="color")
    
    }else if(input$select_plot %in% c("dendrogram")){
      
    }else{
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  
  
  observeEvent(input$plot_graph,{
    tryCatch({
      output$graphic <- renderPlotly({
        if(input$select_plot %in% "scatter plot"){
          ggplot(data(),aes_string(x=input$select_x,y=input$select_y,color=input$select_color)) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
        }
        
        else if(input$select_plot %in% "bar chart"){
          ggplot(data(), aes_string(x=input$select_x,fill=input$select_color)) +
            geom_bar(position=input$dodge,alpha=0.5)
          
        }else if(input$select_plot %in% "boxplot"){
          ggplot(data(), aes_string(x=input$select_x,y=input$select_y,color=input$select_color)) +
            geom_boxplot()
        
        }else if(input$select_plot %in% "violin plot"){
          ggplot(data(), aes_string(x=input$select_x,y=input$select_y,color=input$select_color)) +
            geom_violin()
          
        }else if(input$select_plot %in% "pie chart"){
          if(!is.null(data())){
            result <- data()
            data_to_plot <- NULL
            data_to_plot <- result %>% group_by(get(input$select_y)) %>% count()
            colnames(data_to_plot) <- c("cat","value")
            
            
            plot_ly(data_to_plot, labels=~cat,values=~value,type="pie")
          }
        
        }else if(input$select_plot %in% "correlogram"){
          result <- data()
          p.mat <- cor_pmat(result)
          ggcorrplot::ggcorrplot(p.mat)
          
          
        }else{
          
        }
      })

      
      
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  
  
  
})

  