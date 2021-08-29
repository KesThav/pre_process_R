library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


shinyUI(dashboardPage(
                  dashboardHeader(title="Pre-process dataset without coding"),
                  dashboardSidebar(width = 350,
                                   useShinyjs(),
                                  sidebarMenu(
                                    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                             
                                    menuItem("Analyze", icon = icon("th"), tabName = "analyze",
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
                                              box(solidHeader = TRUE,id="notif", width=12, style="backgroud: #ccff90", h1("Upload a file")),
                                              hidden(
                                                    div(id="hidden",
                                                      box(solidHeader = TRUE,id="menu",width=4,
                                                          tabBox(width=12,
                                                                  tabPanel("Handle columns",
                                                                           uiOutput("delete_col"),
                                                                           uiOutput("delete"),
                                                                           uiOutput("merge_col"),
                                                                           uiOutput("merge_col_name"),
                                                                           uiOutput("merge")),
                                                                  tabPanel("Handle rows",
                                                                           uiOutput("delete_row"),
                                                                           uiOutput("delete_r")),
                                                                  tabPanel("Convert types",
                                                                           uiOutput("col_to_convert"),
                                                                           uiOutput("convert_type"),
                                                                           uiOutput("convert_val")),
                                                                  tabPanel("More options",
                                                                           uiOutput("na"),
                                                                           actionButton("reset","Reset"))
                                                            
                                                                  )
                                                  
                                                      ),
                                                    box(solidHeader = TRUE,id="data_table",width=8,style="overflow-x : scroll",
                                                          column(width=12,DT::dataTableOutput("display_file"))),
                                                      
                                                    box(solidHeader = TRUE,title="details",id="str_table",width=6,
                                                    column(width=12,verbatimTextOutput("str"))),
                                                    
                                                    box(solidHeader = TRUE,title="describe()",id="desc_table",width=6,
                                                    column(width=12,verbatimTextOutput("desc"))),
                                                    
                                                    box(solidHeader = TRUE,title="Show unique",id="unique_",width=6,
                                                    column(width=12,uiOutput("show_unique")),
                                                    column(width=12,verbatimTextOutput("unique")))
                                          )))),
                                tabItem("analyze",h1("Analyze"))


                                
                                )
          
)))