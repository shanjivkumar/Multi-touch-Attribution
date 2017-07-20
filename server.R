<<<<<<< HEAD
library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")

server <- function(input,output){
  output$plot2 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  output$plot3 <- renderPlot({

    hist(buget$roi)
  output$plot1<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")})
  output$plot4<-renderPlot({
    ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")})
    output$mytable1 = renderDataTable(buget)
   # output$mytable2 = renderDataTable(buget)
   # output$mytable3 = renderDataTable(iris)
    #

    ggplot(data=buget,aes(x=factor(date),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")

  })

    output$mytable1 = renderDataTable(buget)
    
    output$mytable2 = renderDataTable(channelpath)
    output$plot4<-renderPlot({
      ggplot(data=buget,aes(x=date,y=roi))+geom_bar(stat="identity")})
    
    # output$mytable2 = renderDataTable(buget)
    # output$mytable3 = renderDataTable(iris)
    #
  }


=======
>>>>>>> e52e95622714c7598a03754cb5cfbc4b0c5969a8
