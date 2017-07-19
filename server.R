library(shiny)
library(shinydashboard)
library(ggplot2)
buget<-read.csv("D:/satheesh/Multi touch attribution/Rshiny/Multi-touch-Attribution/Budget.csv")

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")},height = 400,width = 400)
  output$plot3 <- renderPlot({
    hist(buget$roi)
  })
}