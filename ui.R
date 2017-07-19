#install.packages("shinythemes")
library(shiny)
library(shinydashboard)
#library(shinythemes)
library(DT)


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


<<<<<<< HEAD
    img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=40)
=======
<<<<<<< HEAD

    

    img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=100)

    ##img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=40)
=======
    

    ##img(src="AAEAAQAAAAAAAAjdAAAAJDk0ZjAwNTU5LWVkZmMtNGY4Yy05MzkzLWZmNmMxYWI2YTNlYQ.png",height=40,width=100)

>>>>>>> 6b7690492ac44f443e2aa9fa387f8db5f115e6d8

>>>>>>> ae71f06821a9b26099d0afc294b187bca129f378

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
                box(plotOutput("plot2")),
                box(plotOutput("plot3")),
                box(title="Histogram",status="primary",solidHeader = TRUE,collapsible = TRUE,plotOutput("plot1",height=250)),
                
                box(title="Inputs",status = "warning",solidHeader = TRUE,
                    "Box content here", br(), "More box content",
                    sliderInput("slider", "Slider input:", 1, 100, 50),
                    textInput("text", "Text input:")
                )
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
       
         basicPage(
         h2("The mtcars data"),
         DT::dataTableOutput("mytable")
         
         
              #fluidPage(downloadButton("report", "Generate report"))
      )
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

#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
      )
    )
  )
)
