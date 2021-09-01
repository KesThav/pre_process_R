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
library(ggcorrplot)

shinyServer(function(input,output,session){
  
#Preprocess
  data <- reactiveVal()

  listen <- reactive({list(input$na,input$delete,input$merge,input$delete_r,input$reset,input$convert_val,
                           input$convert_type,input$col_to_convert,input$col_to_encode,input$val_encode,input$encode_type,input$merge_col_sep,input$rename,input$rename_col_name,input$rename_col,
                           input$split,input$split_col, input$split_col_sep)})
  
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

  counter <- reactiveVal(0)
  observeEvent(listen(),{
    req(data())
    tryCatch({

    result <- data()
    if(!is.null(input$file) & length(result) > 0){
      if(input$na){
        result <- result %>% tidyr::drop_na()
        data(result)
      }
      
      else if(length(input$delete_col) != 0 & input$delete){
        result <- as.data.frame(result[,-which(names(result) %in% input$delete_col)])
        data(result)
      }
      
      else if(!is.null(input$display_file_rows_selected) & input$delete_r){
        result <- result[-as.numeric(input$display_file_rows_selected),]
        data(result)
      }
      
      else if(input$merge & input$merge_col_name != ""){
        result[,input$merge_col_name] <- NA
        for(name in input$merge_col){
          result[,input$merge_col_name] <- paste(result[,input$merge_col_name],input$merge_col_sep, result[,name])
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
      }else if(length(input$rename_col) != 0 & length(input$rename_col_name) != 0 & input$rename){
        names(result)[names(result) == input$rename_col] <- input$rename_col_name
        data(result)
        
      }else if(length(input$split_col) != 0 & length(input$split_col_sep) != 0 & input$split){
        splited <- stringr::str_split_fixed(result[,input$split_col],input$split_col_sep,2)
        splited <- as.data.frame(splited)
        result <- cbind(result,splited)
        data(result)
        
      }else{
        data(result)
      }

      output$display_file <- DT::renderDataTable(data(),extensions='Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','csv','excel','print'),search = list(regex = TRUE)),editable=TRUE,server = FALSE)
      input$display_file_
      output$str <- suppressWarnings(renderPrint({str(result[input$display_file_rows_all,])}))
      output$desc <-suppressWarnings(renderPrint({psych::describe(result[input$display_file_rows_all,])}))
      
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
  
  
  
  
  observeEvent(input$file, {
    
    if(length(data() > 0)){
      shinyjs::hide(id = "notif")
      shinyjs::show(id="main_div")
      shinyjs::hide(id = "notif_2")
      shinyjs::show(id="main_div_2")
    }else{
      shinyjs::show(id = "notif")
      shinyjs::hide(id="main_div")
      shinyjs::show(id = "notif_2")
      shinyjs::hide(id="main_div_2")
    }
  })
  
  l <- reactive({list(data(),input$file)})
  observeEvent(l(),{
    req(data())
    tryCatch({
      if(!is.null(input$file)){
        output$na <- renderUI({
        checkboxInput("na","Drop na",value = FALSE)
      })
        outputOptions(output, "na", suspendWhenHidden = FALSE)
        
      output$merge_col <- renderUI({
        pickerInput("merge_col",label="Select columns to merge",choices=colnames(data()), multiple=TRUE)
        })
      
      outputOptions(output, "merge_col", suspendWhenHidden = FALSE)
      
      output$delete_col <- renderUI({
        pickerInput("delete_col",label="Select columns to delete",choices=colnames(data()),multiple=TRUE)
        })
      
      outputOptions(output, "delete_col", suspendWhenHidden = FALSE)
    
      
      output$merge_col_name <- renderUI({
        textInput("merge_col_name","Enter merge column's name")
      })
      
      outputOptions(output, "merge_col_name", suspendWhenHidden = FALSE)
      
      output$merge_col_sep <- renderUI({
        textInput("merge_col_sep","Enter separator",value=" ")
      })
      
      outputOptions(output, "merge_col_sep", suspendWhenHidden = FALSE)
      
      output$rename_col <- renderUI({
        pickerInput("rename_col",label="Select column to rename",choices=colnames(data()))
      })
      
      outputOptions(output, "rename_col", suspendWhenHidden = FALSE)
      
      
      output$rename_col_name <- renderUI({
        textInput("rename_col_name","Enter new name")
      })
      
      outputOptions(output, "rename_col_name", suspendWhenHidden = FALSE)
      
      output$rename <- renderUI({
        actionButton("rename","rename")
      })
      
      outputOptions(output, "rename", suspendWhenHidden = FALSE)
      
      
      output$delete <- renderUI({
        actionButton("delete","delete columns")
      })
      
      outputOptions(output, "delete", suspendWhenHidden = FALSE)
      
      output$merge <- renderUI({
        actionButton("merge","merge columns")
      })
      
      outputOptions(output, "merge", suspendWhenHidden = FALSE)
      
      output$delete_r <- renderUI({
        actionButton("delete_r","delete rows")
      })
      
      outputOptions(output, "delete_r", suspendWhenHidden = FALSE)
      
      
      output$col_to_convert <- renderUI({
        pickerInput("col_to_convert","Select column to convert",choices=colnames(data()))
      })
      
      outputOptions(output, "col_to_convert", suspendWhenHidden = FALSE)
      
      output$convert_type <- renderUI({
        pickerInput("convert_type","Select what to convert to",choices=c('integer','double','chr','factor'))
      })
      
      outputOptions(output, "convert_type", suspendWhenHidden = FALSE)
      
      output$convert_val <- renderUI({
        actionButton("convert_val","Convert")
      })
      
      outputOptions(output, "convert_val", suspendWhenHidden = FALSE)
      
      output$show_unique <- renderUI({
        pickerInput("show_unique","Show unique",choices=colnames(data()),select=colnames(data())[1])
      })
      
      outputOptions(output, "show_unique", suspendWhenHidden = FALSE)
      
      output$reset <- renderUI({
        actionButton("reset","Reset")
      })
      
      outputOptions(output, "reset", suspendWhenHidden = FALSE)
      
      output$col_to_encode <- renderUI({
        pickerInput("col_to_encode","Select column to encode",choices=colnames(data()),select=colnames(data())[1])
      })
      
      outputOptions(output, "col_to_encode", suspendWhenHidden = FALSE)
      
      output$encode_type <- renderUI({
        pickerInput("encode_type","Select type of encoding",choices=c("Numerical","One hot"),select="Normal")
      })
      
      outputOptions(output, "encode_type", suspendWhenHidden = FALSE)
      
      
      output$val_encode <- renderUI({
        actionButton("val_encode","Encode")
      })
      
      outputOptions(output, "val_encode", suspendWhenHidden = FALSE)
      

      
      output$data_rows <- renderInfoBox({
        suppressWarnings(infoBox(
          "Rows", length(input$display_file_rows_all), icon = icon("list"),
          color = "purple"
        ))
      })
      
      outputOptions(output, "data_rows", suspendWhenHidden = FALSE)
      
      output$data_columns <- renderInfoBox({
        infoBox(
          "Columns", length(colnames(data())), icon = icon("list"),
          color = "purple"
        )
      })
      
      outputOptions(output, "data_columns", suspendWhenHidden = FALSE)
      
      output$split_col <- renderUI({
        pickerInput("split_col","Select column to split",choices=colnames(data()),selected=colnames(data())[1])
      })
      
      outputOptions(output, "split_col", suspendWhenHidden = FALSE)
      
      output$split_col_sep <- renderUI({
        textInput("split_col_sep","Enter separator",value=" ")
      })
      
      outputOptions(output, "split_col_sep", suspendWhenHidden = FALSE)
      
      output$split <- renderUI({
        actionButton("split","split")
      })
      
      outputOptions(output, "split", suspendWhenHidden = FALSE)
      
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    


  
  })
  
  observeEvent(input$show_unique,{
    result <- data()
    output$unique <- suppressWarnings(renderPrint({unique(result[input$display_file_rows_all,input$show_unique])}))
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
        pickerInput("select_color",label="Select your label color",choices=c(colnames(data())),selected="")
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
  
  ##############################################################################################################################################################
  ##############################################################################################################################################################
  ##############################################################################################################################################################
  #Join
  
  file_data <- reactiveValues(f2="",merge="")
  
  join_event <- reactive({list(data(),input$file2)})
  observeEvent(join_event(),{
    tryCatch({
      
      if(!is.null(input$file)){
        output$f1_table <- DT::renderDataTable(data())
      }
      if(!is.null(input$file2)){
        d2 <- input$file2
        file_2 <- read.csv(d2$datapath,header=TRUE)
        rownames(file_2) <- NULL
        file_2 <- as.data.frame(file_2)
        file_data$f2 <- file_2
        
        output$f2_table <- DT::renderDataTable(file_data$f2)
      }
      
      if(!is.null(data()) & !is.null(input$file2)){
        result <- data()
        c <- which(names(result) %in% names(file_data$f2))
        c <- colnames(result)[c]
        
        if(length(c) > 0){
          output$select_join <- renderUI({
            selectInput("select_join","Select join type",choices=c("Inner join","Left join","Right join","Full join","Semi join","Anti join"),selected="Inner join") 
          })
          
          
          output$join_by <- renderUI({
            selectInput("join_by","Select columns to join",choices=c,multiple = TRUE) 
          })
          
          output$save <- renderUI({
            downloadButton("downloadData","Download")
          })
          
          output$override <- renderUI({
            actionButton(style="background-color : #f50057; color : #ffffff;","override","Override current data")
          })
        }else{
          showNotification("files does not have common columns", type="error")
        }


      }
      output$clear <- renderUI({
        actionButton("clear","clear")
      })
      
      
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  
  join <- reactive({list(input$select_join, input$join_by)})
  
  observeEvent(join(),{
    result <- data()
    tryCatch({
          if(!is.null(input$select_join) & !is.null(input$join_by)){
      if(input$select_join %in% "Inner join"){
        file_data$merge <- inner_join(result,file_data$f2,by=c(input$join_by))
        
      }else if(input$select_join %in% "Left join"){
        file_data$merge <- left_join(result,file_data$f2,by=c(input$join_by))
        
      }else if(input$select_join %in% "Right join"){
        file_data$merge <- right_join(result,file_data$f2,by=c(input$join_by))
        
      }else if(input$select_join %in% "Full join"){
        file_data$merge <- full_join(result,file_data$f2,by=c(input$join_by))
        
      }else if(input$select_join %in% "Semi join"){
        file_data$merge <- semi_join(result,file_data$f2,by=c(input$join_by))
        
      }else if(input$select_join %in% "Anti join"){
        file_data$merge <- anti_join(result,file_data$f2,by=c(input$join_by))
        
      }else{
        
      }
      
      output$table_merge <- DT::renderDataTable(file_data$merge)
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  
  observeEvent(input$clear, {
    file_data$merge <- ""
  })
  
  observeEvent(input$override,{
    showModal(modalDialog(
      title = "Warning",
      "The current data will be overrided. Please confirm and dismiss.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton(style="background-color : #f50057; color : #ffffff;","confirm", "Confirm")
      )
    ))
  })
  
  observeEvent(input$confirm,{
    data(as.data.frame(file_data$merge))
    removeModal()
    showNotification("Current data has been replaced", type="message")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(file_data$merge, file, row.names = FALSE)
    }
  )
  
  
})

  