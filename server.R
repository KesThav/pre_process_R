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
library(GGally)
library(e1071)
library(caret)
library(plotly)
library(purrr)
library(kernlab)
library(MASS)
library(randomForest)

shinyServer(function(input,output,session){
  
#Preprocess
  data <- reactiveVal()
  
#replace NA function
  completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
  }


  #read file
  observeEvent(input$file,{
    tryCatch({
    if(!is.null(input$file)){
      req(input$file)
      file <- input$file
      d <- read.csv(file$datapath,header=TRUE)
      rownames(d) <- NULL
      data(as.data.frame(d))
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  #reset
  observeEvent(input$reset,{
    tryCatch({
    file <- input$file
    d <- read.csv(file$datapath,header=TRUE)
    rownames(d) <- NULL
    data(as.data.frame(d))
  }, warning = function(warn){
    showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste0(err), type = 'err')})
  })

  js <- c(
    "table.on('column-reorder', function(e, settings, details){",
    "  Shiny.setInputValue('colOrder', details.mapping);",
    "});"
  )
  #show dataset
  observeEvent(data(),{
    tryCatch({
    result <- data()
    output$display_file <- DT::renderDataTable(data(),filter = "top",extensions=list('Buttons'=TRUE,'ColReorder'=TRUE),callback = JS(js),options=list(dom='Bfrtip',buttons=list('copy','pdf','csv','excel','print'),search = list(regex = TRUE),colReorder = TRUE),editable=TRUE,server = FALSE)
    output$str <- suppressWarnings(renderPrint({str(result[input$display_file_rows_all,])}))
    output$desc <-suppressWarnings(renderPrint({psych::describe(result[input$display_file_rows_all,])}))
  }, warning = function(warn){
    showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste0(err), type = 'err')})
  })
  
  observeEvent(input[["colOrder"]],{
    result <- data()
    result <- result[,input[["colOrder"]]]
    data(result)
  })
  
  #delete na
  observeEvent(input$na,{
    tryCatch({
      result <- data()
      result <- completeFun(result,c(input$na_select))
      data(result)
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  #replace na
  observeEvent(input$replace_na,{
    req(input$na_select_replace,input$na_replace_by)
    result <- data()
    if(input$na_replace_by == 'Mean'){
      for(item in input$na_select_replace){
        print(item)
        mean_ <- mean(result[,item],na.rm=TRUE)
        result[,item] <- ifelse(is.na(result[,item]),mean_,result[,item])
        print(mean_)
        data(result)
      }
      
    }else if(input$na_replace_by == "Median"){
      
      for(item in input$na_select_replace){
        median_ <- median(result[,item],na.rm=TRUE)
        result[,item] <- ifelse(is.na(result[,item]),median_,result[,item])
        data(result)
      }
    }else if(input$na_replace_by == 0){
      for(item in input$na_select_replace){
        result[,item] <- ifelse(is.na(result[,item]),0,result[,item])
        data(result)
      }
    }
      
    else{
      for(item in input$na_select_replace){
        result[,item] <- ifelse(is.na(result[,item]),input$na_fill_with,result[,item])
        data(result)
      }
      
    }
  })
  
  #delete columns
  observeEvent(c(input$delete_col,input$delete),{
    tryCatch({
    if(length(input$delete_col) != 0 & input$delete){
      result <- data()
      result <- as.data.frame(result[,-which(names(result) %in% input$delete_col)])
      data(result)
    }
  }, warning = function(warn){
    showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste0(err), type = 'err')})
  })
  
  #delete rows
  observeEvent(c(input$display_file_rows_selected,input$delete_r),{
    tryCatch({
    if(!is.null(input$display_file_rows_selected) & input$delete_r){
      result <- data()
      result <- result[-as.numeric(input$display_file_rows_selected),]
      data(result)
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  #merge columns
  observeEvent(c(input$merge,input$merge_col_name,input$merge_col_sep),{
    tryCatch({
    if(input$merge & input$merge_col_name != "" & length(input$merge_col_sep) != 0){
      result <- data()
      result[,input$merge_col_name] <- NA
      for(name in input$merge_col){
        result[,input$merge_col_name] <- paste(result[,input$merge_col_name],input$merge_col_sep, result[,name])
      }
      result <- as.data.frame(result[,-which(names(result) %in% input$merge_col)])
      data(result)
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  #convert col
  observeEvent(c(input$col_to_convert,input$convert_type,input$convert_val),{
    tryCatch({
    if(length(input$col_to_convert) != 0 & length(input$convert_type) != 0 & input$convert_val){
      result <- data()
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
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  })
  
  #encode columns
  observeEvent(c(input$col_to_encode,input$val_encode),{
    tryCatch({
  if(length(input$col_to_encode) != 0 & input$val_encode){
    result <- data()
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
  }, warning = function(warn){
    showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste0(err), type = 'err')})  
  })
  
  #edit cells
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
  
  #rename columns
  observeEvent(c(input$rename_col,input$rename_col_name,input$rename),{
  tryCatch({
  if(length(input$rename_col) != 0 & length(input$rename_col_name) != 0 & input$rename){
    result <- data()
    names(result)[names(result) == input$rename_col] <- input$rename_col_name
    data(result)
  }
  }, warning = function(warn){
    showNotification(paste0(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste0(err), type = 'err')})
  })
  
  
  observeEvent(c(input$split_col,input$split_col_sep,input$split),{
  tryCatch({
  if(length(input$split_col) != 0 & length(input$split_col_sep) != 0 & input$split){
    result <- data()
    splited <- stringr::str_split(result[,input$split_col],input$split_col_sep,simplify = TRUE)
    splited <- as.data.frame(splited)
    result <- cbind(result,splited)
    data(result)
  }
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
      shinyjs::hide(id = "notif_3")
      shinyjs::show(id="main_div_3")
      shinyjs::hide(id = "notif_4")
      shinyjs::show(id="main_div_4")
    }else{
      shinyjs::show(id = "notif")
      shinyjs::hide(id="main_div")
      shinyjs::show(id = "notif_2")
      shinyjs::hide(id="main_div_2")
      shinyjs::show(id = "notif_3")
      shinyjs::hide(id="main_div_3")
      shinyjs::show(id = "notif_4")
      shinyjs::hide(id="main_div_4")
    }
  })
  
  observeEvent(input$na_replace_by,{
    req(input$na_replace_by)
    if(input$na_replace_by == "Other"){
      shinyjs::show(id = "na_fill_with")
    }else{
      shinyjs::hide(id = "na_fill_with")
    }

  })
  
  observeEvent(c(data(),input$file),{
    req(data())
    
    colnames_with_na <- colnames(data())[colSums(is.na(data())) > 0]
    tryCatch({
      if(!is.null(input$file)){
        output$na <- renderUI({
        actionButton("na","Drop na",value = FALSE)
      })
        
      output$na_select <- renderUI({
        pickerInput("na_select","Drop na from columns : ",choices=colnames_with_na,multiple=TRUE,selected=colnames_with_na)
      })
      outputOptions(output, "na", suspendWhenHidden = FALSE)
      
      output$na_fill_with <- renderUI({
        textInput("na_fill_with","Fill na with :")
      })
      
      outputOptions(output, "na_fill_with", suspendWhenHidden = FALSE)
      
      outputOptions(output, "na_select", suspendWhenHidden = FALSE)
      
      output$na_select_replace <- renderUI({
        pickerInput("na_select_replace","Replace na from columns : ",choices=colnames_with_na,multiple=TRUE,selected=colnames_with_na)
      })
      
      output$na_replace_by <- renderUI({
        pickerInput("na_replace_by","By :",choices=c("Mean","Median","0","Other"),options = list(create = TRUE))
      })
      
      output$replace_na <- renderUI({
        actionButton("replace_na","Replace na",value = FALSE)
      })
      
      outputOptions(output, "na_replace_by", suspendWhenHidden = FALSE)
      
      outputOptions(output, "na_select_replace", suspendWhenHidden = FALSE)
      
      outputOptions(output, "replace_na", suspendWhenHidden = FALSE)
        
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
          color = "blue"
        ))
      })
      
      outputOptions(output, "data_rows", suspendWhenHidden = FALSE)
      
      output$data_columns <- renderInfoBox({
        infoBox(
          "Columns", length(colnames(data())), icon = icon("list"),
          color = "blue"
        )
      })
      
      output$data_na <- renderInfoBox({
        suppressWarnings(infoBox(
          "Number of na", sum(is.na(data())), icon = icon("list"),
          color = "blue"
        ))
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
      
      output$override_datasets <- renderUI({
        actionButton(style="margin-bottom : 20px; background-color :#f50057;color:#ffffff;","override_datasets","Override datasets")
      })
      
      outputOptions(output, "override_datasets", suspendWhenHidden = FALSE)
      
      
      output$col_to_encode_special <- renderUI({
        pickerInput("col_to_encode_special", "Select column to encode",choices=colnames(data()))
      })
      
      outputOptions(output, "col_to_encode_special", suspendWhenHidden = FALSE)
      
      output$encode_data <- renderUI({
        numericInput("encode_data","Encode with",value=1)
      })
      
      outputOptions(output, "encode_data", suspendWhenHidden = FALSE)
      
      output$val_encode_special <- renderUI({
        actionButton("val_encode_special","Apply special encoding")
      })
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    
})
  
    #show variable of column to encode
    observeEvent(input$col_to_encode_special,{
      req(input$col_to_encode_special)
      output$variables <- renderUI({
        result <- data()
        pickerInput("variables","Select variables to encode",choices=(unique(result[,input$col_to_encode_special])),multiple=TRUE)
      })
    })
    
    #special encode validation
    observeEvent(c(input$val_encode_special),{
      result <- data()
      result[,c(input$col_to_encode_special)] <- ifelse(result[,c(input$col_to_encode_special)] %in% input$variables,input$encode_data,result[,c(input$col_to_encode_special)])
      data(result)
    })

    observeEvent(input$override_datasets,{
      showModal(modalDialog(
        title = "Warning",
        "The current data will be overrided by the filter one. Please confirm and dismiss.",
        footer = tagList(
          modalButton("Dimiss"),
          actionButton(style="background-color : #f50057; color : #ffffff;","confirm_override", "Confirm")
        )
      ))
    })
  
    
    observeEvent(input$confirm_override,{
      tryCatch({
        if(input$confirm_override){
          result <- data()
          print(input$display_file_rows_all)
          result <- result[input$display_file_rows_all,]
          removeModal()
          data(result)
        }
        
      })
      
    })

  
  observeEvent(input$show_unique,{
    result <- data()
    output$unique <- suppressWarnings(renderPrint({unique(result[input$display_file_rows_all,input$show_unique])}))
  })
  
  
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
#Analyze

  plots <- c("scatter plot","bar chart","boxplot","violin plot","pie chart","correlogram","pairplot")
  
  observeEvent(data(),{
    tryCatch({
    if(length(data()) > 0){

      output$select_plot <- renderUI({
        pickerInput("select_plot",label="Select your plot",choices=plots)
      })
      
      outputOptions(output, "select_plot", suspendWhenHidden = FALSE)
      
      output$select_x <- renderUI({
        pickerInput("select_x",label="Select your x axis",choices=colnames(data()),selected="")
      })
      
      outputOptions(output, "select_x", suspendWhenHidden = FALSE)
      
      output$dodge <- renderUI({
        radioButtons("dodge", "Position",choices=c("dodge","identity"),selected="identity")
      })
      
      outputOptions(output, "dodge", suspendWhenHidden = FALSE)
      
      output$select_y <- renderUI({
        pickerInput("select_y",label="Select your y axis",choices=colnames(data()),selected="")
      })
      
      outputOptions(output, "select_y", suspendWhenHidden = FALSE)
      
      output$select_color <- renderUI({
        pickerInput("select_color",label="Select your label color",choices=c(colnames(data()),"None"=""),selected="")
      })
      outputOptions(output, "select_color", suspendWhenHidden = FALSE)

      output$plot_graph <- renderUI({
        actionButton("plot_graph","Plot")
      })
      
      outputOptions(output, "plot_graph", suspendWhenHidden = FALSE)
      
      output$facet_wrap <- renderUI({
        pickerInput("facet_wrap",label="Select facet wrap",choices=c(colnames(data())),multiple=TRUE,options = pickerOptions(maxOptions = 2))
      })
      
      outputOptions(output, "facet_wrap", suspendWhenHidden = FALSE)
      
      output$facet_orientation <- renderUI({
        radioButtons("facet_orientation", "facet orientation",choices=c("Vertical","Horizontal"),selected="Vertical")
      })
      
      outputOptions(output, "facet_orientation", suspendWhenHidden = FALSE)
      
      output$x_axis_slider <- renderUI({
        sliderInput("x_axis_slider","x label orientation",min=0,max=90,value=45)
      })
      
      output$y_axis_slider <- renderUI({
        sliderInput("y_axis_slider","y label orientation",min=0,max=90,value=45)
      })
      
      outputOptions(output, "y_axis_slider", suspendWhenHidden = FALSE)
      
      output$vertical_adjustment_x <- renderUI({
        sliderInput("vertical_adjustment_x","vertical adjustment for x axis",min=0,max=1,value=0.5)
      })
      
      outputOptions(output, "vertical_adjustment_x", suspendWhenHidden = FALSE)
      
      output$horizontal_adjustment_x <- renderUI({
        sliderInput("horizontal_adjustment_x","horizontal adjustment for x axis",min=0,max=1,value=1)
      })
      
      outputOptions(output, "horizontal_adjustment_x", suspendWhenHidden = FALSE)
      
      output$vertical_adjustment_y <- renderUI({
        sliderInput("vertical_adjustment_y","vertical adjustment for y axis",min=0,max=1,value=0.5)
      })
      
      outputOptions(output, "vertical_adjustment_y", suspendWhenHidden = FALSE)
      
      output$horizontal_adjustment_y <- renderUI({
        sliderInput("horizontal_adjustment_y","horizontal adjustment for y axis",min=0,max=1,value=1)
      })
      
      outputOptions(output, "horizontal_adjustment_y", suspendWhenHidden = FALSE)
      

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
      shinyjs::show(id="facet_wrap")
      shinyjs::show(id="facet_orientation")
      
    }
    
    else if(input$select_plot %in% c("bar chart")){
      
      shinyjs::show(id="x_value")
      shinyjs::hide(id="y_value")
      shinyjs::show(id="dodge")
      shinyjs::show(id="color")
      shinyjs::show(id="facet_wrap")
      shinyjs::show(id="facet_orientation")
      
    }else if(input$select_plot %in% c("pie chart")){
      shinyjs::hide(id="x_value")
      shinyjs::show(id="y_value")
      shinyjs::hide(id="dodge")
      shinyjs::hide(id="color")
      shinyjs::hide(id="facet_wrap")
      shinyjs::hide(id="facet_orientation")
      
    }else if(input$select_plot %in% c("correlogram")){
      shinyjs::hide(id="x_value")
      shinyjs::hide(id="y_value")
      shinyjs::hide(id="dodge")
      shinyjs::hide(id="color")
      shinyjs::hide(id="facet_wrap")
      shinyjs::hide(id="facet_orientation")
    
    }else if(input$select_plot %in% c("pairplot")){
      
    }else{
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  
  
  change_to_observe <- reactive({list(input$plot_graph,input$x_axis_slider,input$vertical_adjustment_x,hjust=input$horizontal_adjustment_x,input$y_axis_slider,
                                      input$vertical_adjustment_y,hjust=input$horizontal_adjustment_y)})
  observeEvent(change_to_observe(),{
    tryCatch({
      t <- theme(axis.text.x = element_text(angle = input$x_axis_slider, vjust = input$vertical_adjustment_x, hjust=input$horizontal_adjustment_x), 
                 axis.text.y = element_text(angle=input$y_axis_slider, vjust = input$vertical_adjustment_y, hjust=input$horizontal_adjustment_y))
      output$graphic <- renderPlotly({
        if(input$select_plot %in% "scatter plot"){
          p <- ggplot(data(),aes_string(x=input$select_x,y=input$select_y,color=ifelse(input$select_color != "",input$select_color,"NULL"))) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange=TRUE) + t
          if(!is.null(input$facet_wrap) & length(input$facet_wrap) != 0){
            if(length(input$facet_wrap) == 1){
             if(input$facet_orientation == "Vertical"){
               p + facet_grid(reformulate(".",input$facet_wrap))
             }else{
               p + facet_grid(reformulate(input$facet_wrap,"."))
             }
            }else{
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(input$facet_wrap[1],input$facet_wrap[2]))
              }else{
                p + facet_grid(reformulate(input$facet_wrap[2],input$facet_wrap[1]))
              }
            }
          }else{
            p
          }
        }
        
        else if(input$select_plot %in% "bar chart"){
          p <- ggplot(data(), aes_string(x=input$select_x,fill=ifelse(input$select_color != "",input$select_color,"NULL"))) +
            geom_bar(position=input$dodge,alpha=0.5) + t
          if(!is.null(input$facet_wrap) & length(input$facet_wrap) != 0){
            if(length(input$facet_wrap) == 1){
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(".",input$facet_wrap))
              }else{
                p + facet_grid(reformulate(input$facet_wrap,"."))
              }
            }else{
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(input$facet_wrap[1],input$facet_wrap[2]))
              }else{
                p + facet_grid(reformulate(input$facet_wrap[2],input$facet_wrap[1]))
              }
            }
          }else{
            p
          }
          
        }else if(input$select_plot %in% "boxplot"){
          p<- ggplot(data(), aes_string(x=input$select_x,y=input$select_y,color=ifelse(input$select_color != "",input$select_color,"NULL"))) +
            geom_boxplot()+ t
          if(!is.null(input$facet_wrap) & length(input$facet_wrap) != 0){
            if(length(input$facet_wrap) == 1){
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(".",input$facet_wrap))
              }else{
                p + facet_grid(reformulate(input$facet_wrap,"."))
              }
            }else{
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(input$facet_wrap[1],input$facet_wrap[2]))
              }else{
                p + facet_grid(reformulate(input$facet_wrap[2],input$facet_wrap[1]))
              }
            }
          }else{
            p
          }
        
        }else if(input$select_plot %in% "violin plot"){
          p <- ggplot(data(), aes_string(x=input$select_x,y=input$select_y,color=ifelse(input$select_color != "",input$select_color,"NULL"))) +
            geom_violin()+ t
          if(!is.null(input$facet_wrap) & length(input$facet_wrap) != 0){
            if(length(input$facet_wrap) == 1){
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(".",input$facet_wrap))
              }else{
                p + facet_grid(reformulate(input$facet_wrap,"."))
              }
            }else{
              if(input$facet_orientation == "Vertical"){
                p + facet_grid(reformulate(input$facet_wrap[1],input$facet_wrap[2]))
              }else{
                p + facet_grid(reformulate(input$facet_wrap[2],input$facet_wrap[1]))
              }
            }
          }else{
            p
          }
          
        }else if(input$select_plot %in% "pie chart"){
          if(!is.null(data())){
            result <- data()
            data_to_plot <- NULL
            data_to_plot <- result %>% dplyr::group_by(get(input$select_y)) %>% count()
            colnames(data_to_plot) <- c("cat","value")
            
            
            plot_ly(data_to_plot, labels=~cat,values=~value,type="pie")
          }
        
        }else if(input$select_plot %in% "correlogram"){
          result <- data()
          p.mat <- cor_pmat(result)
          ggcorrplot::ggcorrplot(p.mat)
          
          
        }else if(input$select_plot %in% "pairplot"){
          result <- data()
          ggpairs(data=result)
        
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
  
  
  observeEvent(c(input$select_join, input$join_by),{
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
        modalButton("Dismiss"),
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
  

  
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
#ML models
  
  
  observeEvent(data(),{
    tryCatch({
      output$select_models <- renderUI({
        pickerInput("select_models","Select your model",choices=c("Linear regression"="lm","SVM (Linear)"="svmLinear","SVM (Polynomial)"="svmPoly","Random Forest"="rf","Linear Discriminant Analysis"="lda"))
      })
      
      output$split_or_load <- renderUI({
        radioButtons("split_or_load", "Split or load",choices=c("Split dataset (train/test)","load dataset (test)"),selected="Split dataset (train/test)")
      })
      
      output$load_test <- renderUI({
        fileInput("load_test","Load test file",multiple=FALSE,accept = c("text/csv",
                                                                         "text/comma-separated-values,text/plain",
                                                                         ".csv"))
      })
      
      output$load_predict_datasets <- renderUI({
        fileInput("load_predict_datasets","Load dataset to predict",multiple=FALSE,accept = c("text/csv",
                                                                         "text/comma-separated-values,text/plain",
                                                                         ".csv"))
      })
      
      output$split_size <- renderUI({
        sliderInput("split_size","Split size",min=0.1, max=0.95,value = 0.75)
      })
      
      output$label_column <- renderUI({
        pickerInput("label_column","Select the label column",choices=colnames(data()),selected=colnames(data()))
      })
      
      output$label_used_to_predict <- renderUI({
        col <- colnames(data())
        col <- col[col != input$label_column]
        pickerInput("label_used_to_predict","Select the columns used to predict",choices=col,multiple=TRUE,selected=(colnames(data())))
      })
      output$train_model <- renderUI({
        actionButton("train_model","Train model")
      })

      outputOptions(output, "split_or_load", suspendWhenHidden = FALSE)
      outputOptions(output, "split_size", suspendWhenHidden = FALSE)
      outputOptions(output, "load_test", suspendWhenHidden = FALSE)
      
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
    
  },ignoreNULL = TRUE,ignoreInit = TRUE)
  

  
  observeEvent(c(input$split_or_load,input$select_models),{
    if(input$split_or_load == "Split dataset (train/test)"){
      shinyjs::hide(id="load_test")
      shinyjs::show(id="split_size")
    }else{
      shinyjs::show(id="load_test")
      shinyjs::hide(id="split_size")
    }

  })
  
  df <- reactiveValues(train=NULL,test=NULL,pred=NULL,test_y=NULL,finalModel=NULL,final_pred=NULL)
  
  observeEvent(c(data(),input$split_or_load,input$split_size,input$load_test,input$label_used_to_predict),{
    req(input$split_or_load)
    tryCatch({
    if(input$split_or_load == "Split dataset (train/test)" & !is.null(input$split_size)){
      req(input$split_or_load,input$split_size)
      result <- data()
      sample <- sample.int(n = nrow(result), size = floor(input$split_size*nrow(result)), replace = F)
      train <- result[sample,]
      test <- result[-sample,]
      df$train <- train
      df$test <- test[,names(test) %in% input$label_used_to_predict]
      df$test_y <- test[,names(test) %in% input$label_column]
      output$train_set <- renderDataTable({
        df$train
      })
      output$test_set <- renderDataTable({
        df$test
      })
    }
    else if(input$split_or_load == "load dataset (test)" & !is.null(input$load_test)){
      req(input$split_or_load,input$load_test)
      result <- data()
      train <- result
      f_ <- input$load_test
      test <- read.csv(f_$datapath,header=TRUE)
      df$train <- train
      df$test <- test
      output$train_set <- renderDataTable({
        df$train
      })
      output$test_set <- renderDataTable({
        df$test
      })
    }else{
      
    }
    }, warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})
  },ignoreNULL = TRUE,ignoreInit = TRUE)
  
  
  
  ml_train <- function(method,train_x,train_y){
    showModal(modalDialog("Loading...", footer=NULL))
    fitControl <- trainControl(method="repeatedcv",number=10,repeats=10)
    if(method=="lm"){
      train_model <- train(train_x,train_y,method=method,trControl=fitControl)
    }else{
      train_model <- train(train_x,as.factor(train_y),method=method,trControl=fitControl)
    }
    removeModal()
    return(train_model)
  }
  
  
  observeEvent(input$train_model,{
    tryCatch({
    train_x <- df$train[,c(input$label_used_to_predict)]
    train_y <- df$train[,c(input$label_column)]
    ml_train_m <- ml_train(input$select_models,train_x=train_x,train_y=train_y)
    output$model_error <- renderPrint({ml_train_m})
    output$final_model <- renderPrint({ml_train_m$finalModel})
    df$finalModel <- ml_train_m
    
    output$predict <- renderUI({
      actionButton("predict","Predict")
    })
    
    #show prediction on test set with accuracy or mse
    pred <- predict(df$finalModel,newdata=df$test)
    df$pred <- cbind(df$test,as.data.frame(pred))
    df$pred <- cbind(df$pred,df$test_y)
    output$test_set <-  renderDataTable({
      df$pred
    })
    if(input$select_models == "lm"){
      test_accuracy <- sum(as.data.frame(pred) - df$test_y)^2/length(df$test_y)
      output$test_set_accuracy <- renderPrint({paste("test set MSE : ", test_accuracy)})
    }else{
      test_accuracy <- sum(as.data.frame(pred) == df$test_y,na.rm = TRUE)/length(df$test_y)
      output$test_set_accuracy <- renderPrint({paste("test set accuracy : ", test_accuracy*100,"%")})
    }
    
    },warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  

  observeEvent(input$predict,{
    tryCatch({
      file <- input$load_predict_datasets
      data_to_predict <- read.csv(file$datapath,header=TRUE)
      req(input$load_predict_datasets)
      pred <- predict(df$finalModel,newdata=data_to_predict[,c(input$label_used_to_predict)])
      df$final_pred <- as.data.frame(pred)
      df$final_pred <- cbind(data_to_predict[,c(input$label_used_to_predict)],df$final_pred)
    
      output$prediction <- renderDataTable(
        df$final_pred,extensions='Buttons',options=list(dom='Bfrtip',buttons=list('copy','pdf','csv','excel','print'),search = list(regex = TRUE)),editable=TRUE,server = FALSE
      )
    },warning = function(warn){
      showNotification(paste0(warn), type = 'warning')
    },
    error = function(err){
      showNotification(paste0(err), type = 'err')})

  })
  

})

