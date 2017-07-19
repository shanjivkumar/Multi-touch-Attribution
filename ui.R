library(shiny)
library(shinydashboard)



ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Multi-Touch Attribution"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Dashboard", tabName = "sumdashboard", icon = icon("bar-chart")),
      menuItem("Attribution Dashboard", tabName = "attrdashboard", icon = icon("list-alt")),
      menuItem("Channel Performance", tabName = "channelreport", icon = icon("line-chart")),
      menuItem("Campaign Performance", tabName = "campaignreport", icon = icon("pie-chart")),
      menuItem("Path Report", tabName = "pathreport", icon = icon("arrows-alt"))
      
    )
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

      tabItem(tabName = "attrdashboard",
              
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
      )
      ,
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)