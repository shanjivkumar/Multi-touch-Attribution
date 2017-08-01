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

    ggplot(buget, aes(date)) + 
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")
    
    
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
  quarter<-quarter(buget$date)
  year<-year(buget$date)
  buget.new<-data.frame(quarter,buget)
  summaryquarterplot1<-aggregate(buget.new$marketing.budget~buget.new$quarter, FUN=sum)
  summaryquarterplot1
  names(summaryquarterplot1)<-c("quarter","marketing.budget")
  
  summaryquarterplot1new<-aggregate(buget.new$roi~buget.new$quarter, FUN=sum)
  summaryquarterplot1new
  names(summaryquarterplot1new)<-c("quarter","roi")
  
  finaldataframe<-data.frame(summaryquarterplot1,summaryquarterplot1new$roi)
  finaldataframe
  names(finaldataframe)<-c("quarter","marketing.budget","roi")
  finaldataframe
  
  
  summaryquarterplot2<-aggregate(buget.new$roi~buget.new$quarter+buget.new$channel, FUN=sum)
  summaryquarterplot2
  names(summaryquarterplot2)<-c("quarter","channel","roi")
  summaryquarterplot3<-aggregate(buget.new$no.of.conversions~buget.new$quarter+buget.new$channel, FUN=sum)
  summaryquarterplot3
  names(summaryquarterplot3)<-c("quarter","channel","no.of.conversions")
 

  output$summaryquarterplot1 <- renderPlot({
ggplot(data=finaldataframe,aes(x=factor(quarter)))+
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")
    
    
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
  buget$year<-year(buget$date)
  yeardate<-year(buget$date)
  buget.new<-data.frame(yeardate,buget)
  summaryyearplot2<-aggregate(buget.new$roi~buget.new$yeardate+buget.new$channel, FUN=sum)
  names(summaryyearplot2)<-c("yeardate","channel","roi")
  
  summaryyearplot3<-aggregate(buget.new$no.of.conversions~buget.new$yeardate+buget.new$channel, FUN=sum)
  summaryyearplot3
  names(summaryyearplot3)<-c("yeardate","channel","no.of.conversions")
  
  output$summaryyearplot1 <- renderPlot({
    ggplot(buget, aes(year)) + 
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")
    
  })
  
  output$summaryyearplot2 <- renderPlot({
    ggplot(data=summaryyearplot2,aes(x=(yeardate),y=roi,fill=channel)) +  
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryyearplot3 <- renderPlot({
    ggplot(data=summaryyearplot3,aes(x=(yeardate),y=no.of.conversions,group=channel)) +
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
