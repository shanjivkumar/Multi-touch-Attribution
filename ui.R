#install.packages("shinythemes")
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Multi-Touch Attribution"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Summary Dashboard", tabName = "sumdashboard", icon = icon("bar-chart")),
                        menuItem("Attribution Dashboard", tabName = "attrdashboard", icon = icon("list-alt")),
                        menuItem("Channel Performance", tabName = "channelreport", icon = icon("line-chart")),
                        menuItem("Campaign Performance", tabName = "campaignreport", icon = icon("pie-chart")),
                        menuItem("Path Report", tabName = "pathreport", icon = icon("arrows-alt"))
                      ),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      sidebarUserPanel("Powered By : I2 Decisions", subtitle = NULL, image = NULL),
                      sidebarUserPanel("www.i2decisions.com",image = "AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png")
                    ),
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "sumdashboard",
                                fluidRow(
                                  # A static infoBox
                                  
                                  box(tags$b("Budget"),br(), 250000,width = 2, background = "olive"),
                                  box(tags$b("Budget Used"),br(), 50000,width = 2, background = "olive"),
                                  box(tags$b("Conversion"),br(),120000,width = 2, background = "olive"),
                                  box(tags$b("Cost per Conversions"),br(),3.6,width = 2, background = "olive"),
                                  box(tags$b("Revenue Generated"),br(), paste0(120000),width = 2, background = "olive"),
                                  box(tags$b("Revenue Generated"),br(), paste0(120000),width = 2, background = "olive")
                                ),
                                fluidRow(
                                  box(plotOutput("plot2", height = 250)),
                                  box(plotOutput("plot3", height = 250)),
                                  frow1 <- bootstrapPage(
                                    column(6,
                                           dataTableOutput("mytable2", width="100%"))  ##Table we're trying to display##
                                  ),
                                  box(plotOutput("plot4", height = 250))
                                )#,
                                #fluidRow(
                                #  
                                #  tabBox(
                                #    title = "First tabBox",
                                #    # The id lets us use input$tabset1 on the server to find the current tab
                                #    id = "tabset1", height = "250px",
                                #    tabPanel("Tab1", "First tab content"),
                                #    tabPanel("Tab2", "Tab content 2")
                                #  )
                                #)
                                
                        ),
                        ################################################################################################################################################
                        
                        tabItem(tabName = "attrdashboard",
                                
                                frow1 <- fluidRow(column(4,align ='center', h1('Selection')),
                                      sidebarPanel(
                                  selectInput("Objective", "Choose an objective:",
                                              choices = c("Awareness", "Engagement", "ROI")),
                                  
                                  numericInput("obs", "Observations:", 10)
                                )),
                                #///////////////////////////////////////////////////////
                                frow2 <- fluidRow(
                                  column(8,align ='center', h1('Attribution'),
                                         dataTableOutput("mytable1", width="100%"), theme = shinytheme("cyborg"))  ##Table we're trying to display##
                                ),
                                ### Below lines are used to create tabs within a page
                                
                                #tabBox(
                                #   title = "Data Viewer"
                                #   ,width = 9
                                #   ,id = "dataTabBox"
                                #   ,tabPanel(
                                #     title = "Sales by Model",
                                #     width = 4,
                                #     dataTableOutput("mytable1")  ##Table we're trying to display##
                                #   )
                                #,tabPanel(
                                #title = "Sales by Quarter"
                                #,dataTableOutput("mytable2")
                                #)
                                # ,tabPanel(
                                #  title = "Prior Year Sales"
                                #  ,dataTableOutput("mytable3")
                                #)
                                
                                
                                
                                #//////Below lines can be used to create navigation bars within a page/////////////////////////////////////////////////////
                                ## navbarPage("Multi-Touch Attribution",
                                ## tabPanel("Summary Dashboard"),
                                ## tabPanel("Attribution Dashboard"),
                                ## tabPanel("Channel Performance")),
                                
                                fluidPage(theme = "bootstrap.css") ###----Themes for the page----###
                                # fluidPage(theme = shinytheme("cyborg")),
                                
                                # dateRangeInput('dateRange',
                                # label = 'Date range - "yyyy-mm-dd"',
                                # start = Sys.Date() - 2, end = Sys.Date() + 2
                                # ),
                                
                                #-----HIstogram------#
                                
                                #fluidRow(
                                #box(title="Histogram",status="primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot1",height=250)),
                                #box(title="Inputs",status = "warning",solidHeader = TRUE,
                                #"Box content here", br(), "More box content",
                                # sliderInput("slider", "Slider input:", 1, 100, 50),
                                # textInput("text", "Text input:")
                                #)
                                #),
                                # fluidRow(
                                #   tabBox(
                                #     title = "First tabBox",
                                #     # The id lets us use input$tabset1 on the server to find the current tab
                                #     id = "tabset1", height = "250px",
                                #     tabPanel("Tab1", "First tab content"),
                                #     tabPanel("Tab2", "Tab content 2")
                                #   )
                                # ),
                                # dataTableOutput('mytable'),
                                
                                # fluidPage(
                                # h2("The mtcars data"),
                                # dataTableOutput("mytable")
                                
                                #fluidPage(downloadButton("report", "Generate report"))
                                #)
                        ),
                        ###############-------END of Attributoin dashboard-------------------------#############################################################################################
                        
                        #--------------------------------------------------Channel report---------------------------------------------------------
                        #------------------------------------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "channelreport",
                                
                                column (width = 3,offset = 9,
                                        dateRangeInput('dateRange',
                                               label = tags$b("Date range :  YYYY-MM-DD"),
                                               start = Sys.Date() - 2, end = Sys.Date() + 2
                                )),
                                
                                fluidRow(
<<<<<<< HEAD
                                  box(title="Histogram",status="primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot1",height=250)),
                                  
                                  box(title="Inputs",status = "warning",solidHeader = TRUE,
                                      "Box content here", br(), "More box content",
                                      sliderInput("slider", "Slider input:", 1, 100, 50),
                                      textInput("text", "Text input:")
                                  )
                                )
=======
                                  column(width = 9, 
                                         box(title="Channel Performance - KPI",status="primary",solidHeader = TRUE,collapsible = FALSE,width = 12,plotOutput("plot1",height=300))
                                         #box(tags$b("Budget"),br(), 250000,width = 2,offset = 9, background = "olive") ,
                                         #box(tags$b("Budget"),br(), 250000,width = 2,offset = 9, background = "olive") 
                                  ),
                                  column(width = 3, br(),
                                         #box(title="Channel Performance - KPI",status="primary",solidHeader = TRUE,collapsible = FALSE,width = 12,plotOutput("plot1",height=300))
                                         box(selectInput("Monthlychannel", label = "Month",
                                                                                choices = c("Awareness", "Engagement", "ROI"))
                                             ,width = 8,offset =2) ,
                                         box(selectInput("quarterlychannel", label = "Quarter",
                                                        choices = c("Awareness", "Engagement", "ROI"))
                                            ,width = 8,offset =2) ,
                                         box(selectInput("yearlychannel", label = "Year",
                                                         choices = c("Awareness", "Engagement", "ROI"))
                                             ,width = 8,offset =2) 
                                         
                                         
                                         )),
                                
                                fluidRow(
                                  column(width = 8, 
                                         box(title="Channel Performance - Trend",status="primary",solidHeader = TRUE,collapsible = FALSE,width = 12,plotOutput("plot6",height=300))
                                  ),
                                column(width = 4, br(),
                                       #box(title="Channel Performance - KPI",status="primary",solidHeader = TRUE,collapsible = FALSE,width = 12,plotOutput("plot1",height=300))
                                       box(selectInput("objective$channel", label = "Objective",
                                                       choices = c("Awareness", "Engagement", "ROI"))
                                           ,width = 8,offset =2) ,
                                       box(selectInput("type$channel", label = "Channel Type",
                                                       choices = c("Awareness", "Engagement", "ROI"))
                                           ,width = 8,offset =2) ,
                                       box(selectInput("kpi$channel", label = "KPI",
                                                       choices = c("Awareness", "Engagement", "ROI"))
                                           ,width = 8,offset =2) )
>>>>>>> 8860b928378c50208fa5d61052d20028895f6eb4
                                #
                                #------------------------------------------------------------------------------------------------------------------------------------
                                #------------------------------------------------------------------------------------------------------------------------------------
                        )
                      )
                    )
     )
)

