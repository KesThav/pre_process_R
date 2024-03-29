library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)
#library(rsconnect)


ui <- dashboardPage(
                  dashboardHeader(title="PPR"),
                  dashboardSidebar(width = 350,
                                   useShinyjs(),
                                  sidebarMenu(
                                    menuItem("Dashboard", tabName = "dashboard", icon = icon("blackboard",lib="glyphicon")),
                                    menuItem("Join files", icon = icon("link",lib="glyphicon"), tabName = "join"
                                             ),
                                    menuItem("Plot", icon = icon("stats",lib = "glyphicon"), tabName = "plot"),
                                    menuItem("Machine Learning", icon = icon("wrench",lib="glyphicon"), tabName = "ml")
                                    
                                  ),
                    
                                  fileInput("file","Upload CSV file",multiple=FALSE,accept = c("text/csv",
                                                                                  "text/comma-separated-values,text/plain",
                                                                                  ".csv"))
                                  
                                  ),
                  dashboardBody(
                                tabItems(
                                  tabItem(tabName="dashboard",
                                        fluidRow(
                                              box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif", width=12, h1("Upload a file"))),
                                              
                                        hidden(div(id="main_div",
                                                   fluidRow(
                                                       infoBoxOutput("data_rows"),
                                                       infoBoxOutput("data_columns"),
                                                       infoBoxOutput("data_na")),
                                        
                                        fluidRow(
                                          column(style='padding:0px;',offset=0,width=4,div(id="hidden",
                                                                 tabBox(width=12,
                                                                        tabPanel("Merge / Split",
                                                                                 uiOutput("merge_col"),
                                                                                 uiOutput("merge_col_name"),
                                                                                 uiOutput("merge_col_sep"),
                                                                                 uiOutput("merge"),
                                                                                 hr(),
                                                                                 uiOutput("split_col"),
                                                                                 uiOutput("split_col_sep"),
                                                                                 uiOutput("split")
                                                                                 ),
                                                                                
                                                                        tabPanel("Rename",
                                                                                 uiOutput("rename_col"),
                                                                                 uiOutput("rename_col_name"),
                                                                                 uiOutput("rename")
                                                                                 ),
                                                                        tabPanel("Convert types",
                                                                                 uiOutput("col_to_convert"),
                                                                                 uiOutput("convert_type"),
                                                                                 uiOutput("convert_val")),
                                                                        tabPanel("Encode variables",
                                                                                 uiOutput("col_to_encode"),
                                                                                 uiOutput("encode_type"),
                                                                                 uiOutput("val_encode"),
                                                                                 hr(),
                                                                                 uiOutput("col_to_encode_special"),
                                                                                 uiOutput("variables"),
                                                                                 uiOutput("encode_data"),
                                                                                 uiOutput("val_encode_special")),
                                                                        tabPanel("Handle NA",
                                                                                 uiOutput("na_select"),
                                                                                 uiOutput("na"),
                                                                                 hr(),
                                                                                 uiOutput("na_select_replace"),
                                                                                 uiOutput("na_replace_by"),
                                                                                 hidden(div(id="na_fill_with",uiOutput("na_fill_with"))),
                                                                                 uiOutput("replace_na")
                                                                                 ),
                                                                        tabPanel("Delete col/row",
                                                                                 uiOutput("delete_col"),
                                                                                 uiOutput("delete"),
                                                                                 hr("Select rows then click on delete"),
                                                                                 uiOutput("delete_r")
                                                                                 ),
                                                                        tabPanel("More options",
                                                                                 uiOutput("reset"))))),
                                          
                                          column(style='padding:0px;',offset=0,width=8,box(style="overflow-x : scroll;height=40em",solidHeader = TRUE,id="data_table",width=12,
                                                             column(uiOutput("override_datasets"),width=12,DT::dataTableOutput("display_file"))))
                                                    ),
                                        
                                        fluidRow(
                                          column(style='padding:0px;',width=6,box(solidHeader = TRUE,title="details",id="str_table",width=12,
                                                             column(width=12,verbatimTextOutput("str")))),
                                          column(style='padding:0px;',width=6,box(solidHeader = TRUE,title="describe",id="desc_table",width=12,
                                                             column(width=12,verbatimTextOutput("desc"))))),
                                  
                                        fluidRow(
                                          box(solidHeader = TRUE,title="Show unique",id="unique_",width=12,
                                              column(width=12,uiOutput("show_unique")),
                                              column(width=12,verbatimTextOutput("unique"))))))
                                                    
                                                  
                                          ),
                                  tabItem(tabName="plot",
                                          fluidRow(
                                            box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif_2", width=12, style="backgroud: #ccff90", h1("Upload a file"))),
                                          hidden(
                                            div(id="main_div_2",
                                          
                                          fluidRow(
                                            
                                            column(style='padding:0px;',width=4,tabBox(width=12,
                                                                                       tabPanel("Plot",
                                                                                                uiOutput("select_plot"),
                                                                                                hidden(div(id="x_value",uiOutput("select_x"))),
                                                                                                hidden(div(id="y_value",uiOutput("select_y"))),
                                                                                                hidden(div(id="dodge",uiOutput("dodge"))),
                                                                                                hidden(div(id="color",uiOutput("select_color"))),
                                                                                                hidden(div(id="facet_wrap",uiOutput("facet_wrap"))),
                                                                                                hidden(div(id="facet_orientation",uiOutput("facet_orientation"))),
                                                                                                uiOutput("plot_graph")
                                                                                                ),
                                                                                       tabPanel("Params",
                                                                                                uiOutput("x_axis_slider"),
                                                                                                uiOutput("y_axis_slider"),
                                                                                                uiOutput("vertical_adjustment_x"),
                                                                                                uiOutput("horizontal_adjustment_x"),
                                                                                                uiOutput("vertical_adjustment_y"),
                                                                                                uiOutput("horizontal_adjustment_y"))
                                                                                        )),
                                            
                                            column(style='padding:0px;',width=8, box(solidHeader = TRUE,width=12,
                                                                                     plotlyOutput("graphic")
                                            ))),
                                           ))),
                                  
                                  tabItem(tabName="join",
                                          fluidRow(
                                            box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif_3", width=12, style="backgroud: #ccff90", h1("Upload a file"))),
                                          hidden(div(id="main_div_3",
                                          
                                          fluidRow(
                                            
                                            column(style='padding:0px;',width=4,box(solidHeader = TRUE,width=12,
                                                                                fileInput("file2","Load second file",multiple=FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                                                                uiOutput("select_join"),
                                                                                uiOutput("join_by"),
                                                                                div(style="width : 100%; display : flex; justify-content : space-between;",div(style="display : flex; flex-direction : row;",uiOutput("clear"),uiOutput("save")),
                                                                                div(uiOutput("override")))
                                                                                )),
                                            
                                            column(style='padding:0px;',width=8, tabBox(width=12,
                                                                                        tabPanel("first file",DT::dataTableOutput("f1_table")),
                                                                                        tabPanel("second file",DT::dataTableOutput("f2_table")),
                                                                                        tabPanel(style="overflow-x : scroll;","merged file",DT::dataTableOutput("table_merge"))
                                                                                        
                                            )))
                                          
                                          ))),
                                  
                                  
                                  tabItem(tabName="ml",
                                          fluidRow(
                                            box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif_4", width=12, style="backgroud: #ccff90", h1("Upload a file"))),
                                          hidden(div(id="main_div_4",
                                          
                                          
                                          fluidRow(
                                            column(width=4, box(width=12,solidHeader=TRUE,
                                                                uiOutput("select_models"),
                                                                uiOutput("split_or_load"),
                                                                hidden(div(id="split_size",uiOutput("split_size"))),
                                                                hidden(div(id="load_test",uiOutput("load_test"))),
                                                                uiOutput("label_column"),
                                                                uiOutput("label_used_to_predict"),
                                                                uiOutput("load_predict_datasets"),
                                                                div(style="display : flex; flex-direction : row; justify-content : space-between; width : 175px;",uiOutput("train_model"),uiOutput("predict"))
                                                                )),
                                            
                                            column(width=8,tabBox(width=12,
                                                                tabPanel(style="overflow-x : scroll;","train_set",
                                                                         DT::dataTableOutput("train_set")),
                                                                tabPanel(style="overflow-x : scroll;","test set",
                                                                         DT::dataTableOutput("test_set")),
                                                                tabPanel(style="overflow-x : scroll;","Prediction",
                                                                         DT::dataTableOutput("prediction"))
                                                                
                                            )),
                                            
                                          ),
                                          fluidRow(
                                            column(width=4),
                                            column(width=8,tabBox(width=12,
                                                                  tabPanel("model_error",verbatimTextOutput("model_error"),verbatimTextOutput("test_set_accuracy")),
                                                                  tabPanel("finalModel",verbatimTextOutput("final_model"))))
                                          )
                                           )

                                  
                                  


                                  )))

                                
                                )
                                  



                              
          
)