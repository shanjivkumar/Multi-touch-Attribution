library(shiny)
library(shinydashboard)
library(data.table)
library(datasets)
library(DT)

server <- function(input, output) {
  
  
  output$mytable = DT::renderDataTable({mtcars})
  
  ##output$table <- renderTable(iris)
  ##output$mytable <- renderDataTable({iris})
  
    
}