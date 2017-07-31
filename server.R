library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
library(plotly)
library(dplyr)
library(zoo)

buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")

server <- function(input,output){

########### Summary tab #########
########### Month Report #########
  buget$date <- strptime(as.character(buget$date), "%d/%m/%Y")
  buget$month<-as.Date(cut(buget$date,breaks = "month"))
  
  output$summarymonthplot1 <- renderPlot({
    ggplot(data=buget,aes(month,roi))+stat_summary(fun.y=sum,geom="bar")+stat_summary(fun.y=sum,geom="line")
  })
  
  
  output$summarymonthplot2 <- renderPlot({
    ggplot(data=buget,aes(x=(month),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
    
  })
  
  output$summarymonthplot3 <- renderPlot({
    ggplot(data=buget,aes(x=(month),y=no.of.conversions,group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summarymonthplot4 = renderDataTable(channelpath)
  ########### Summary tab #########
  ########### Quarter Report #########
  quarter<-quarter(buget$date,with_year = T)
  buget.new<-data.frame(quarter,buget)
  summaryquarterplot2<-aggregate(buget.new$roi~buget.new$quarter+buget.new$channel, FUN=sum)
  summaryquarterplot2
  names(summaryquarterplot2)<-c("quarter","channel","roi")
  summaryquarterplot3<-aggregate(buget.new$no.of.conversions~buget.new$quarter+buget.new$channel, FUN=sum)
  summaryquarterplot3
  names(summaryquarterplot3)<-c("quarter","channel","no.of.conversions")
  output$summaryquarterplot1 <- renderPlot({
    ggplot(data=buget,aes(quarter,roi))+stat_summary(fun.y=sum,geom="bar")+stat_summary(fun.y=sum,geom="line")
  })
  
  output$summaryquarterplot2 <- renderPlot({
    ggplot(data=summaryquarterplot2,aes(x=factor(quarter),y=roi,fill=channel)) + 
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryquarterplot3 <- renderPlot({
    ggplot(data=summaryquarterplot3,aes(x=factor(quarter),y=(no.of.conversions),group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summaryquarterplot4 = renderDataTable(channelpath)
  ########### Summary tab #########
  ########### Year Report #########
  buget$year<-as.Date(cut(buget$date,breaks = "year"))
  
  output$summaryyearplot1 <- renderPlot({
    ggplot(data=buget,aes(year,roi))+stat_summary(fun.y=sum,geom="bar")+stat_summary(fun.y=sum,geom="line")
  })
  
  output$summaryyearplot2 <- renderPlot({
    ggplot(data=buget,aes(x=(year),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryyearplot3 <- renderPlot({
    ggplot(data=buget,aes(x=(year),y=no.of.conversions,group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summaryyearplot4 = renderDataTable(channelpath)

  
############################################################################
############################################################################
  output$plot10 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  

  ##Sample try
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main=input$region,
            ylab="Number of Telephones",
            xlab="Year")
  })


  
  output$mytable1 = renderDataTable(buget)
  

  

  
  # output$mytable2 = renderDataTable(buget)
  # output$mytable3 = renderDataTable(iris)
  #
}
