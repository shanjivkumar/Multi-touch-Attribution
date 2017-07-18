library(shiny)
library(shinydashboard)



ui <- dashboardPage(
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
                infoBox("Budget", 250000, icon = icon("list"),color = "purple",width = 2),
                infoBox("Budget Used", 50000, icon = icon("thumbs-up"),color = "yellow",width = 2),
                infoBox("Conversion",120000, icon = icon("credit-card"),width = 2),
                infoBox("Cost per Conversions", paste0(120000), icon = icon("credit-card"),width = 3),
                infoBox("Revenue Generated", paste0(120000), icon = icon("credit-card"),width = 3)
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
