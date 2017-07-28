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
  output$summarymonthplot1 <- renderPlot({
    
    ggplot(buget) +
      geom_bar(aes(x = date, weight = (roi))) +
      geom_line(aes(x = as.numeric(date), y = (marketing.budget)))+
      theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))+
      ggtitle("ROI & Marketing budget")
  })
  
  output$summarymonthplot2 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summarymonthplot3 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=no.of.conversions,group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summarymonthplot4 = renderDataTable(channelpath)
  ########### Summary tab #########
  ########### Quarter Report #########
  buget$yearquater <- as.yearqtr(buget$date, format = "%d/%m/%y%y")
  output$summaryquarterplot1 <- renderPlot({
    
    ggplot(buget) +
      geom_bar(aes(x = yearquater, weight = roi)) +
      geom_line(aes(x = as.numeric(yearquater), y = marketing.budget))+
      theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))+
      ggtitle("ROI & Marketing budget")
  })
  
  output$summaryquarterplot2 <- renderPlot({
    ggplot(data=buget,aes(x=factor(yearquater),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryquarterplot3 <- renderPlot({
    ggplot(data=buget,aes(x=factor(yearquater),y=no.of.conversions,group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summaryquarterplot4 = renderDataTable(channelpath)
  ########### Summary tab #########
  ########### Year Report #########
  output$summaryyearplot1 <- renderPlot({
    
    ggplot(buget) +
      geom_bar(aes(x = date, weight = roi)) +
      geom_line(aes(x = as.numeric(date), y = marketing.budget))+
      theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"))+
      ggtitle("ROI & Marketing budget")
  })
  
  output$summaryyearplot2 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryyearplot3 <- renderPlot({
    ggplot(data=buget,aes(x=factor(date),y=no.of.conversions,group=channel)) +
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
