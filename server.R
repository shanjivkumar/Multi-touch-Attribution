library(shiny)
library(shinydashboard)
<<<<<<< HEAD
library(ggplot2)
buget<-read.csv("D:/satheesh/Multi touch attribution/Rshiny/Multi-touch-Attribution/Budget.csv")

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")},height = 400,width = 400)
  output$plot3 <- renderPlot({
    hist(buget$roi)
  })
=======
library(data.table)
library(datasets)
library(DT)

server <- function(input, output) {
  
  
  output$mytable = DT::renderDataTable({mtcars})
  
  ##output$table <- renderTable(iris)
  ##output$mytable <- renderDataTable({iris})
  
    
>>>>>>> ae71f06821a9b26099d0afc294b187bca129f378
}