#install.packages("shinythemes")
library(shiny)
library(shinydashboard)
#library(shinythemes)
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
                      sidebarUserPanel("www.i2decisions.com",image = "AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png"),
                      
                      
                      
                      img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=40)
                      
                      
                      
                      
                      
                      ##img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=40)
                      
                      
                      ##img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=100)
                      
                      
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
                                  box(plotOutput("plot3", height = 250))
                                  
                                ),
                                fluidRow(
                                  
                                  tabBox(
                                    title = "First tabBox",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", height = "250px",
                                    tabPanel("Tab1", "First tab content"),
                                    tabPanel("Tab2", "Tab content 2")
                                  )
                                )
                                
                        ),
                        ################################################################################################################################################
                        
                        tabItem(tabName = "attrdashboard",
                                
                                #///////////////////////////////////////////////////////
                                frow1 <- fluidRow(
<<<<<<< HEAD
                                  tabBox(
                                    title = "Data Viewer"
                                    ,width = 12
                                    ,id = "dataTabBox"
                                    ,tabPanel(
                                      title = "Sales by Model"
                                      ,dataTableOutput("mytable1")
                                    )
                                    ,tabPanel(
                                      title = "Sales by Quarter"
                                      ,dataTableOutput("mytable2")
                                    )
                                    ,tabPanel(
                                      title = "Prior Year Sales"
                                      ,dataTableOutput("mytable3")
                                    )
                                  )
                                )
=======
                                  column(10,align ='center', h1('Attribution'),
                                  dataTableOutput("mytable1", width="100%"))  ##Table we're trying to display##
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
                                  
                                
>>>>>>> e8012d414a7b7f975225766a8591e5566781de75
                                
                                #///////////////////////////////////////////////////////////
                                ## navbarPage("Multi-Touch Attribution",
                                ## tabPanel("Summary Dashboard"),
                                ## tabPanel("Attribution Dashboard"),
                                ## tabPanel("Channel Performance")),
                                
                                # fluidPage(theme = "bootstrap.css"),
                                #  fluidPage(theme = shinytheme("cyborg")),
                                
                                # dateRangeInput('dateRange',
                                # label = 'Date range - "yyyy-mm-dd"',
                                # start = Sys.Date() - 2, end = Sys.Date() + 2
                                # ),
                                
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
                        ################################################################################################################################################
                        
                        #--------------------------------------------------Channel report---------------------------------------------------------
                        #------------------------------------------------------------------------------------------------------------------------------------
                        tabItem(tabName = "channelreport",
                                
                                dateRangeInput('dateRange',
                                               label = 'Date range input: yyyy-mm-dd',
                                               start = Sys.Date() - 2, end = Sys.Date() + 2
                                ),
                                
                                fluidRow(
                                  box(title="Histogram",status="primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot1",height=250)),
                                  
                                  box(title="Inputs",status = "warning",solidHeader = TRUE,
                                      "Box content here", br(), "More box content",
                                      sliderInput("slider", "Slider input:", 1, 100, 50),
                                      textInput("text", "Text input:")
                                  )
                                )
                                #
                                #------------------------------------------------------------------------------------------------------------------------------------
                                #------------------------------------------------------------------------------------------------------------------------------------
                        )
                      )
                    )
)
