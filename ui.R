
#install.packages("shinythemes")
library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
#library(zoo)


buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")
attribution<-read.csv("Attribution.csv")

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Multi-Touch Attribution"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Summary Dashboard", tabName = "sumdashboard", icon = icon("bar-chart")),
                        menuItem("Attribution Dashboard", tabName = "attrdashboard", icon = icon("list-alt")),
                        menuItem("Channel Performance", tabName = "channelreport", icon = icon("line-chart")),
                        #menuItem("Campaign Performance", tabName = "campaignreport", icon = icon("pie-chart")),
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
                      ###########################Summary Dashboard#####################################################################################################################
                      tabItems(
                        # First tab content
                        tabItem(tabName = "sumdashboard",
                                fluidRow(
                                  tabBox(
                                    title = "Summary Dashboard",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", height = "650px",width = "500px",
                                    ### Sumary dashboard - Month tab
                                    tabPanel("Month",                                 
                                             fluidRow(
                                      # A static infoBox
                                      
                                      box(tags$b("Budget Allocated ($)"),br(), sum(buget$marketing.budget),width = 2, background = "olive"),
                                      box(tags$b("Budget Used ($)"),br(), sum(buget$marketing.budget)*0.8,width = 2, background = "olive"),
                                      box(tags$b("Total Conversions"),br(),sum(buget$no.of.conversions),width = 2, background = "olive"),
                                      box(tags$b("Cost per Conversion ($)"),br(),round(mean(buget$Cost.per.conversion), digits=2),width = 3, background = "olive"),
                                      box(tags$b("Revenue Generated ($)"),br(), sum(buget$roi),width = 3, background = "olive")
                                    ),
                                    fluidRow(
                                      box(plotlyOutput("summarymonthplot1", height = 250)),
                                      box(plotlyOutput("summarymonthplot2", height = 250)),
                                      box(plotlyOutput("summarymonthplot3", height = 420)),
                                      frow1 <- bootstrapPage(
                                        column(6,
                                               dataTableOutput("summarymonthplot4", width="100%"))  ##Table we're trying to display##
                                      )
                                      
                                    )),
                                    ### Sumary dashboard - Quarter tab
                                    tabPanel("Quarter",  
                                             fluidRow(
                                               # A static infoBox
                                               box(tags$b("Budget Allocated ($)"),br(), sum(buget$marketing.budget),width = 2, background = "olive"),
                                               box(tags$b("Budget Used ($)"),br(), sum(buget$marketing.budget)*0.8,width = 2, background = "olive"),
                                               box(tags$b("Total Conversions"),br(),sum(buget$no.of.conversions),width = 2, background = "olive"),
                                               box(tags$b("Cost per Conversion ($)"),br(),round(mean(buget$Cost.per.conversion), digits=2),width = 3, background = "olive"),
                                               box(tags$b("Revenue Generated ($)"),br(), sum(buget$roi),width = 3, background = "olive")
                                             ),
                                             fluidRow(
                                               box(plotlyOutput("summaryquarterplot1", height = 250)),
                                               box(plotlyOutput("summaryquarterplot2", height = 250)),
                                               box(plotlyOutput("summaryquarterplot3", height = 420)),
                                               frow1 <- bootstrapPage(
                                                 column(6,
                                                        dataTableOutput("summaryquarterplot4", width="100%"))  ##Table we're trying to display##
                                               )
                                               
                                             )),
                                    tabPanel("Year",                                 
                                             fluidRow(
                                               # A static infoBox
                                               
                                               box(tags$b("Budget Allocated ($)"),br(), sum(buget$marketing.budget),width = 2, background = "olive"),
                                               box(tags$b("Budget Used ($)"),br(), sum(buget$marketing.budget)*0.8,width = 2, background = "olive"),
                                               box(tags$b("Total Conversions"),br(),sum(buget$no.of.conversions),width = 2, background = "olive"),
                                               box(tags$b("Cost per Conversion ($)"),br(),round(mean(buget$Cost.per.conversion), digits=2),width = 3, background = "olive"),
                                               box(tags$b("Revenue Generated ($)"),br(), sum(buget$roi),width = 3, background = "olive")
                                             ),
                                             fluidRow(
                                               box(plotlyOutput("summaryyearplot1", height = 250)),
                                               box(plotlyOutput("summaryyearplot2", height = 250)),
                                               box(plotlyOutput("summaryyearplot3", height = 420)),
                                               frow1 <- bootstrapPage(
                                                 column(6,
                                                        dataTableOutput("summaryyearplot4", width="100%"))  ##Table we're trying to display##
                                               )
                                               
                                             ))
                                    ))
                                #,
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
                        #############################################Attribution Dashboard###################################################################################################
                        
                        tabItem(tabName = "attrdashboard",
                                
                                fluidRow(
                                  
                                  sidebarPanel(width=3,
                                               column (width = 12,
                                                       dateRangeInput('dateRange',
                                                                      label = tags$b("Date range :  YYYY-MM-DD"),
                                                                      start = Sys.Date() - 2, end = Sys.Date() + 2
                                                       )),
                                               selectInput("AttributionType", "Choose a type:",
                                                           choices = c("First interaction", "Last interaction", "Multi-touch")),
                                               #numericInput("obs", "Observations:", 10),
                                               sliderInput("Budget", label = h5("Budget"), min = 0, max = 100000, value = 50),
                                               sliderInput("Conversions", label = h5("Conversions"), min = 0, max = 5000, value = 75)#,
                                               #selectInput("region", "Region:", choices=colnames(WorldPhones))
                                               #sliderInput("Conversions", label = h4("Conversions"), min = 0, max = 5000, value = c(25, 75))
                                  ),
                                  mainPanel(
                                    
                                    fluidRow(dataTableOutput("mytable1", width=800)),
                                    br(),
                                    br(),
                                    fluidRow(plotlyOutput("plot11", width=800))
                                  )                                      
                                  
                                )
                                
                        ),
                                
                               # frow <- basicPage(
                                 # column(6,align ='left', h1('Attribution'),
                                           ##Table we're trying to display##
                               # ,
                                #column(6,align ='right', h2('Selection'),
                                    
                                #///////////////////////////////////////////////////////
                                
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
                                #,dataTableOutput("summarymonthplot4")
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
                                
                                #fluidPage(theme = "bootstrap.css") ###----Themes for the page----###
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
                        
                        ###############Channel report#############################################
                        tabItem(tabName = "channelreport",
                                fluidRow(
                                  tabBox(
                                    title = "Channel Performance",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1", height = "650px",width = "500px",
                                    ### Channel dashboard - Month tab
                                    tabPanel("Month",                                 
                                             fluidRow(
                                               box(plotlyOutput("channelmonthplot1", height = 380)),
                                               box(plotlyOutput("channelmonthplot2", height = 380))),
                                            
                                             fluidRow(
                                               splitLayout(cellWidths = c("33%", "33%","33%"),cellArgs = list(style = "padding: 1px"), plotlyOutput("channelmonthplot3"), plotlyOutput("channelmonthplot4"),plotlyOutput("channelmonthplot5"))
                                             )
                                               #fluidPage(
                                              #column(4,plotlyOutput("channelmonthplot3", height = 350,width = 100),
                                              #column(2,plotlyOutput("channelmonthplot4", height = 350,width = 100),
                                              #column(2,plotlyOutput("channelmonthplot5", height = 350,width = 100)
                                               ),
                                    ### Channel dashboard - Quarter tab
                                    tabPanel("Quarter",  
                                             
                                             fluidRow(
                                               box(plotlyOutput("channelquarterplot1", height = 380)),
                                               box(plotlyOutput("channelquarterplot2", height = 380))
                                               ),
                                              fluidRow(
                                                splitLayout(cellWidths=c("33%","33%","33%"),cellArgs = list(style="padding :1px"),plotlyOutput("channelquarterplot3"),plotlyOutput("channelquarterplot4"),plotlyOutput("channelquarterplot5"))
                                              )
                                                #box(plotlyOutput("channelquarterplot3", height = 350)),
                                               #box(plotlyOutput("channelquarterplot4", height = 350))
                                             ),
                                    ### Channel dashboard - Year tab
                                    tabPanel("Year",                                 
                                             fluidRow(
                                               box(plotlyOutput("channelyearplot1", height = 380)),
                                               box(plotlyOutput("channelyearplot2", height = 380))),
                                             fluidRow(
                                               splitLayout(cellWidths=c("33%","33%","33%"),cellArgs = list(style="padding :1px"),plotlyOutput("channelyearplot3"),plotlyOutput("channelyearplot4"),plotlyOutput("channelyearplot5"))
                                               #box(plotlyOutput("channelyearplot3", height = 350)),
                                               #box(plotlyOutput("channelyearplot4", height = 350))
                                             ))
                                  ))
            ),
##########################Path report#####################################################
            tabItem(tabName = "pathreport",
                    
                    fluidRow(
                            title = "Path Report Dashboard",
                        # The id lets us use input$tabset1 on the server to find the current tab
                        id = "tabset1", height = "650px",width = "500px",
                        ### Sumary dashboard - Month tab
                        
                        
                                                      
                                 mainPanel(
                                   fluidRow(
                                   frow1 <- bootstrapPage(
                                            column(9,dataTableOutput("pathreportplot1"))  ##Table we're trying to display##
                                                          ),
                                   
                                            column(3,plotlyOutput("pathreportplot2", width=500,height = 450))
                                          ),
                                   
                                            bootstrapPage(
                                            dataTableOutput("pathreportplot3", width="100%")
                                            )  ##Table we're trying to display##
                                   )
                                   
                                   
                                 )
                        
                      ))
                    
            )
    )
  

