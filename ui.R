library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)

box_height='40em'


shinyUI(dashboardPage(
                  dashboardHeader(title="PPR"),
                  dashboardSidebar(width = 350,
                                   useShinyjs(),
                                  sidebarMenu(
                                    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                    menuItem("Join files", icon = icon("th"), tabName = "join",
                                             badgeLabel = "new", badgeColor = "green"),
                                    menuItem("Analyze", icon = icon("th"), tabName = "analyze",
                                             badgeLabel = "new", badgeColor = "green"),
                                    menuItem("Machine Learning", icon = icon("th"), tabName = "ml",
                                             badgeLabel = "new", badgeColor = "green")
                                    
                                  ),
                    
                                  fileInput("file","Upload CSV file",multiple=FALSE,accept = c("text/csv",
                                                                                  "text/comma-separated-values,text/plain",
                                                                                  ".csv"))
                                  
                                  ),
                  dashboardBody(
                                tabItems(
                                  tabItem(tabName="dashboard",
                                        fluidRow(
                                              box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif", width=12, style="backgroud: #ccff90", h1("Upload a file"))),
                                              
                                        hidden(div(id="main_div",
                                                   fluidRow(
                                                       infoBoxOutput("data_rows"),
                                                       infoBoxOutput("data_columns")),
                                        
                                        fluidRow(
                                          column(style='padding:0px;',offset=0,width=4,div(id="hidden",
                                                                 tabBox(width=12,height=box_height,
                                                                        tabPanel("Handle columns",
                                                                                 uiOutput("delete_col"),
                                                                                 uiOutput("delete"),
                                                                                 uiOutput("merge_col"),
                                                                                 uiOutput("merge_col_name"),
                                                                                 uiOutput("merge_col_sep"),
                                                                                 uiOutput("merge")),
                                                                        tabPanel("Handle rows",
                                                                                 uiOutput("delete_row"),
                                                                                 uiOutput("delete_r")),
                                                                        tabPanel("Convert types",
                                                                                 uiOutput("col_to_convert"),
                                                                                 uiOutput("convert_type"),
                                                                                 uiOutput("convert_val")),
                                                                        tabPanel("Encode variables",
                                                                                 uiOutput("col_to_encode"),
                                                                                 uiOutput("encode_type"),
                                                                                 uiOutput("val_encode")),
                                                                        tabPanel("More options",
                                                                                 uiOutput("na"),
                                                                                 uiOutput("reset"))))),
                                          
                                          column(style='padding:0px;',offset=0,width=8,box(style="overflow-x : scroll;height=40em",solidHeader = TRUE,id="data_table",width=12,
                                                             column(width=12,DT::dataTableOutput("display_file"))))
                                                    ),
                                        
                                        fluidRow(
                                          column(style='padding:0px;',width=6,box(solidHeader = TRUE,title="details",id="str_table",width=12,
                                                             column(width=12,verbatimTextOutput("str")))),
                                          column(style='padding:0px;',width=6,box(solidHeader = TRUE,title="describe()",id="desc_table",width=12,
                                                             column(width=12,verbatimTextOutput("desc"))))),
                                  
                                        fluidRow(
                                          box(solidHeader = TRUE,title="Show unique",id="unique_",width=12,
                                              column(width=12,uiOutput("show_unique")),
                                              column(width=12,verbatimTextOutput("unique"))))))
                                                    
                                                  
                                          ),
                                  tabItem(tabName="analyze",
                                          fluidRow(
                                            box(style="height : 100vh; width : 100%; display : flex; justify-content : center;align-items : center;",solidHeader = TRUE,id="notif_2", width=12, style="backgroud: #ccff90", h1("Upload a file"))),
                                          hidden(
                                            div(id="main_div_2",
                                                fluidRow(
                                            h1("Analyze")),
                                          
                                          fluidRow(
                                            
                                            column(style='padding:0px;',width=4,box(solidHeader=TRUE,width=12,
                                                                                     uiOutput("select_plot"),
                                                                                     hidden(div(id="x_value",uiOutput("select_x"))),
                                                                                     hidden(div(id="y_value",uiOutput("select_y"))),
                                                                                     hidden(div(id="dodge",uiOutput("dodge"))),
                                                                                     hidden(div(id="color",uiOutput("select_color"))),
                                                                                     uiOutput("plot_graph"))),
                                            
                                            column(style='padding:0px;',width=8, box(solidHeader = TRUE,width=12,
                                                                                     plotlyOutput("graphic")
                                            )))))),
                                  
                                  tabItem(tabName="join",
                                          
                                          fluidRow(h1("Join file")),
                                          
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
                                          
                                          ),
                                  
                                  
                                  tabItem(tabName="ml",h1("ML models"))

                                  
                                  


                                  )

                                
                                )
                                  



                              
          
))