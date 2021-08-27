library(shiny)
library(shinydashboard)
library(DT)


shinyUI(dashboardPage(
                  dashboardHeader(title="Pre-process dataset without coding"),
                  dashboardSidebar(
                                  fileInput("file","Upload CSV file",multiple=FALSE,accept = c("text/csv",
                                                                                  "text/comma-separated-values,text/plain",
                                                                                  ".csv")),
                                  checkboxInput("na","Drop na")
                                  ),
                  dashboardBody(
                                fluidRow(
                                        box(width=12,style="overflow-x : scroll",
                                            column(width=12,textOutput("message"),
                                                            DT::dataTableOutput("display_file")
                                                   )
                                            )  
                                        ),
                                        box(width=12,
                                            column(width=12,verbatimTextOutput("str")
                                              
                                                  )
                                            ),
                                        box(width=12,
                                            column(width=12,verbatimTextOutput("desc")
                                           
                                                  )
                                            )
                                
                                )
          
))