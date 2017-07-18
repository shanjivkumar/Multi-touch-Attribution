library(shiny)
library(shinydashboard)



dashboardPage(
  dashboardHeader(title = "Multi-Touch Attribution"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Dashboard", tabName = "sumdashboard", icon = icon("th")),
      menuItem("Attribution Dashboard", tabName = "attrdashboard", icon = icon("th")),
      menuItem("Channel Performance", tabName = "channelreport", icon = icon("dashboard")),
      menuItem("Campaign Performance", tabName = "campaignreport", icon = icon("dashboard")),
      menuItem("Path Report", tabName = "pathreport", icon = icon("dashboard"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "sumdashboard",
              fluidRow(
                # A static infoBox
                box(title = "Budget", 250000,width = 2, background = "maroon"),
                box(title = "Budget Used", 50000,width = 2, background = "maroon"),
                box(title = "Conversion",120000,width = 2, background = "maroon"),
                box(title = "Cost per Conversions",3.6,width = 2, background = "maroon"),
                box(title = "Revenue Generated", paste0(120000),width = 2, background = "maroon"),
                box(title = "Revenue Generated", paste0(120000),width = 2, background = "maroon")
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
