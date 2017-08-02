library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
library(plotly)
library(dplyr)
#library(zoo)

buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")
attribution<-read.csv("Attribution.csv")

server <- function(input,output){

########### Summary tab #########
########### Month Report #########
  buget$date <- strptime(as.character(buget$date), "%d/%m/%Y")
  buget$month<-as.Date(cut(buget$date,breaks = "month"))
  
  
  
  month<-month(buget$date)
  year<-year(buget$date)
  month1<-paste(year,month)
  buget.new<-data.frame(month1,buget)
  summarymonthplot1<-aggregate(buget.new$marketing.budget~buget.new$month1, FUN=sum)
  summarymonthplot1
  names(summarymonthplot1)<-c("month1","marketing.budget")
  
  summarymonthplot1new<-aggregate(buget.new$roi~buget.new$month1, FUN=sum)
  summarymonthplot1new
  names(summarymonthplot1new)<-c("month1","roi")
  
  finaldataframemonth<-data.frame(summarymonthplot1,summarymonthplot1new$roi)
  finaldataframemonth
  names(finaldataframemonth)<-c("month1","marketing.budget","roi")
  finaldataframemonth
  
  
  
  
  output$summarymonthplot1 <- renderPlot({

    ggplot(data=finaldataframemonth,aes(x=factor(month1)))+
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.position="bottom",legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")+xlab("Date")+ylab("Value") 
    
    
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
  quarter1<-paste(year," Q",quarter,sep="")
  buget.new<-data.frame(quarter1,buget)
  summaryquarterplot1<-aggregate(buget.new$marketing.budget~buget.new$quarter1, FUN=sum)
  summaryquarterplot1
  names(summaryquarterplot1)<-c("quarter1","marketing.budget")
  
  summaryquarterplot1new<-aggregate(buget.new$roi~buget.new$quarter1, FUN=sum)
  summaryquarterplot1new
  names(summaryquarterplot1new)<-c("quarter1","roi")
  
  finaldataframe<-data.frame(summaryquarterplot1,summaryquarterplot1new$roi)
  finaldataframe
  names(finaldataframe)<-c("quarter1","marketing.budget","roi")
  finaldataframe
  
  
  summaryquarterplot2<-aggregate(buget.new$roi~buget.new$quarter1+buget.new$channel, FUN=sum)
  summaryquarterplot2
  names(summaryquarterplot2)<-c("quarter1","channel","roi")
  summaryquarterplot3<-aggregate(buget.new$no.of.conversions~buget.new$quarter1+buget.new$channel, FUN=sum)
  summaryquarterplot3
  names(summaryquarterplot3)<-c("quarter1","channel","no.of.conversions")
 

  output$summaryquarterplot1 <- renderPlot({
ggplot(data=finaldataframe,aes(x=factor(quarter1)))+
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.position="bottom",legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")+xlab("Date")+ylab("Value")     
  })
  
  output$summaryquarterplot2 <- renderPlot({
    ggplot(data=summaryquarterplot2,aes(x=factor(quarter1),y=roi,fill=channel)) + 
      geom_bar(position = "dodge", stat="identity") + ylab("ROI On Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Top 5 Channels")
  })
  
  output$summaryquarterplot3 <- renderPlot({
    ggplot(data=summaryquarterplot3,aes(x=factor(quarter1),y=(no.of.conversions),group=channel)) +
      geom_line(aes(color=channel))+geom_point(aes(color=channel))+ ylab("# Of Conversions") + 
      xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("No of conversions vs channels")
  })
  
  output$summaryquarterplot4 = renderDataTable(channelpath)
  ########### Summary tab #########
  ########### Year Report #########
  buget$year<-year(buget$date)
  yeardate<-as.integer(buget$year)
  buget.new<-data.frame(yeardate,buget)
  
  
  summaryyearplot1<-aggregate(buget.new$marketing.budget~buget.new$yeardate, FUN=sum)
  summaryyearplot1
  names(summaryyearplot1)<-c("yeardate","marketing.budget")
  
  summaryyearplot1new<-aggregate(buget.new$roi~buget.new$yeardate, FUN=sum)
  summaryyearplot1new
  names(summaryyearplot1new)<-c("yeardate","roi")
  
  finaldataframeyear<-data.frame(summaryyearplot1,summaryyearplot1new$roi)
  finaldataframeyear
  names(finaldataframeyear)<-c("yeardate","marketing.budget","roi")
  finaldataframeyear
  
  
  summaryyearplot2<-aggregate(buget.new$roi~buget.new$yeardate+buget.new$channel, FUN=sum)
  names(summaryyearplot2)<-c("yeardate","channel","roi")
  
  summaryyearplot3<-aggregate(buget.new$no.of.conversions~buget.new$yeardate+buget.new$channel, FUN=sum)
  summaryyearplot3
  names(summaryyearplot3)<-c("yeardate","channel","no.of.conversions")
  
  output$summaryyearplot1 <- renderPlot({
    ggplot(finaldataframeyear, aes(yeardate)) + 
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.position="bottom",legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("ROI & Marketing Budget")+xlab("Date")+ylab("Value")    
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


  
  output$mytable1 = renderDataTable(attribution)
  

  

  
  # output$mytable2 = renderDataTable(buget)
  # output$mytable3 = renderDataTable(iris)
  #
}
