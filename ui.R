library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)


shinyUI(dashboardPage(
                  dashboardHeader(title="Pre-process dataset without coding"),
                  dashboardSidebar(
                                  sidebarMenu(
                                    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"),
                                             uiOutput("na"),
                                             uiOutput("delete_col"),
                                             uiOutput("delete"),
                                             uiOutput("merge_col"),
                                             uiOutput("merge_col_name"),
                                             uiOutput("merge"),
                                             uiOutput("delete_row"),
                                             uiOutput("delete_r")),
                                             
                                    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
                                             badgeLabel = "new", badgeColor = "green")
                                  ),
                    
                                  fileInput("file","Upload CSV file",multiple=FALSE,accept = c("text/csv",
                                                                                  "text/comma-separated-values,text/plain",
                                                                                  ".csv"))

                                  
                                  ),
                  dashboardBody(
                    
                                useShinyjs(),
                                fluidRow(
                                        box(id="notif", width=12, style="backgroud: #ccff90", "Upload a file"),
                                        box(id="data_table",width=12,style="overflow-x : scroll",
                                            column(width=12,
                                                            DT::dataTableOutput("display_file")
                                                   )
                                            )  
                                        ),
                                        box(id="str_table",width=12,
                                            column(width=12,verbatimTextOutput("str")
                                              
                                                  )
                                            ),
                                        box(id="desc_table",width=12,
                                            column(width=12,verbatimTextOutput("desc")
                                           
                                                  )
                                            )
                                
                                )
          
))