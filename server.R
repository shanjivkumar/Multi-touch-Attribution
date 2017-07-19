library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
buget<-read.csv("D:/satheesh/Multi touch attribution/Rshiny/Multi-touch-Attribution/Budget.csv")

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")},)
  output$plot3 <- renderPlot({
    hist(buget$roi)
  output$mytable1 = renderDataTable(buget)
  output$mytable2 = renderDataTable(buget)
  output$mytable3 = renderDataTable(buget)
  
  })
<<<<<<< HEAD
}
=======
}





>>>>>>> d3eba09d5b19db103145d9a46c6e8a785262c9cc
