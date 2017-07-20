library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")})
  output$plot3 <- renderPlot({
    hist(buget$roi)
    output$mytable1 = renderDataTable(buget)
    output$mytable2 = renderDataTable(channelpath)
    # output$mytable2 = renderDataTable(buget)
    # output$mytable3 = renderDataTable(iris)
    #
  })
  
}





