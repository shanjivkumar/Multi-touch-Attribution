library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
buget<-read.csv("D:/satheesh/Multi touch attribution/Rshiny/Multi-touch-Attribution/Budget.csv")


server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)


 
  
  output$table <- renderDataTable(iris)

server <- function(input,output){
  output$plot2<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")})
  output$plot3 <- renderPlot({
    hist(buget$roi)
  output$mytable1 = renderDataTable(buget)
  output$mytable2 = renderDataTable(buget)
  output$mytable3 = renderDataTable(buget)

  })

}




}







>>>>>>> 4f80526a066e966bc0b95cb4ea60dce0ff8bdb4e
