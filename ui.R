library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard MTA"),
  dashboardSidebar(
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
      menuItem("Budget Allocated", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Budget Used", tabName = "widgets", icon = icon("th")),
      menuItem("Conversion", icon = icon("file-code-o"), 
               href = "https://i2decisions.com"),
      menuItem("Cost per Conversions", tabName = "widgets", icon = icon("th")),
      menuItem("Revenue Generated", tabName = "widgets", icon = icon("th"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                # A static infoBox
                infoBox("Budget Allocated", 250000, icon = icon("list"),color = "purple"),
                infoBox("Budget Used", 50000, icon = icon("thumbs-up", lib = "glyphicon"),
                        color = "yellow"),
                infoBox("Conversion", paste0(120000), icon = icon("credit-card")),
                infoBox("Cost per Conversions", paste0(120000), icon = icon("credit-card")),
                infoBox("Revenue Generated", paste0(120000), icon = icon("credit-card"))
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
      
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
      )
    )
  )
)