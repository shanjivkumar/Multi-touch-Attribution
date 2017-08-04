library(shiny)
library(shinydashboard)
library(ggplot2)
library(data.table)
library(datasets)
library(DT)
library(plotly)
library(dplyr)
#library(scales)
#library(zoo)

buget<-read.csv("Budget.csv")
channelpath<-read.csv("Channel path.csv")
attribution<-read.csv("Attribution.csv")
attribution_type<-read.csv("Attribution-types.csv")
pathlength<-read.csv("Path Length.csv")

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
  
  output$summarymonthplot4 = renderDataTable(channelpath,options=list(dom="t"))
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
  
  output$summaryquarterplot4 = renderDataTable(channelpath,options=list(dom="t"))
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
  
  output$summaryyearplot4 = renderDataTable(channelpath,options=list(dom="t"))
  
  
  ############################################################################
  ############################################################################
  output$plot10 <- renderPlot({
    ggplot(data=Attribution-types,aes(x=(Channel),y=(Percentage_conversion))) +  
      geom_bar(position = "dodge", stat="identity") + ylab("% Of Conversions") + 
      xlab("Channel") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Attribution summary")
  })
  
  
  
  ##Sample try
  
  #attribution$RevenueP <- round((attribution$Revenue/sum(attribution$Revenue))*100)
  #new_data <-attribution[,input$AttributionType]
  new_data<-melt(attribution,id.vars = c("Channel","AttributionType"),measure.vars=c("Percentage_conversion","Percentage_revenue"))
  
  output$plot11 <- renderPlot({
    ggplot(new_data ,aes(x=Channel,y=value,fill=factor(variable)))+  
      geom_bar(data = subset(new_data,AttributionType==input$AttributionType),stat="identity",position="dodge")+ theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold"), legend.title=element_blank())+
      xlab("Channel")+ylab("Percentage")+ggtitle(input$AttributionType)+theme(plot.title=element_text(hjust = 0.5))
  })
  
  output$mytable1 = DT::renderDataTable(DT::datatable({ 
    attribution [attribution$AttributionType== input$AttributionType,c("Channel","Conversions","Percentage_conversion","Revenue","Total_Cost","Cost_per_conversion")]}),options = list(dom = 't'))
  #output$mytable1 = renderDataTable({attribution[,c("Channel","Percentage.Conversion","Revenue","Cost.Conversion","No.of.Conversions")]})
  
  ########################################################################
  ########################################################################
  ######################################################################## 
  
  month<-month(buget$date)
  year<-year(buget$date)
  month1<-paste(year,month)
  buget.new<-data.frame(month1,buget)
  
  
  output$channelmonthplot1 <- renderPlot({
    
    ggplot(data=buget.new,aes(x=factor(channel)))+
      geom_bar(aes(y = roi,fill="roi"), stat="identity") +
      geom_line(aes(y = marketing.budget, group = 1, color = "marketing.budget")) +
      scale_colour_manual(" ", values=c("marketing.budget" = "blue", "roi" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.position="bottom",legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Channel Performance - KPI comparision")+xlab("Date")+ylab("Value") 
    
    
  })
  
  
  output$channelmonthplot2 <- renderPlot({
    
    ggplot(data=buget.new,aes(x=factor(channel)))+
      geom_bar(aes(y = Visits,fill="Visits"), stat="identity") +
      geom_line(aes(y = no.of.conversions, group = 1, color = "no.of.conversions")) +
      scale_colour_manual(" ", values=c("no.of.conversions" = "blue", "Visits" = "red"))+
      scale_fill_manual("",values="red")+
      theme(legend.position="bottom",legend.key=element_blank(),
            legend.title=element_blank(),
            legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Channel Performance - KPI comparision")+xlab("Date")+ylab("Value") 
    
    
  })
  
  #############################Path Report###################
  output$summarymonthplot42 = renderDataTable(pathlength,options = list(dom = 't'))
  pathlength1<-pathlength
  pathlength1$Convesion.percentage<-(pathlength1$Conversions/sum(pathlength1$Conversions))*100
  positions <- c("+10","9","8","7","6","5","4","3","2","1")
  pathlength1$Path.Length.in.Interactions <- factor(pathlength1$Path.Length.in.Interactions, levels = c("10+","9","8","7","6","5","4","3","2","1"))
  
  output$summarymonthplot12 <- renderPlotly({
    plot_ly(pathlength1, y = ~Path.Length.in.Interactions, x = ~Convesion.percentage,type = 'bar', orientation = 'h')%>%
      layout(title="Percentage of Total Conversion")
  })
  
  #ggplotly(ggplot(data=pathlength1,aes(x=(Path.Length.in.Interactions),y=Convesion.percentage)) +scale_x_discrete(limits = positions)+  
  #           geom_bar(position = "dodge", stat="identity") + ylab("Conversions Percentage") + 
  #           xlab("Path Length in Interactions") + theme(legend.position="center" ,plot.title = element_text(size=15, face="bold")) + 
  #           ggtitle("                                   
  #                    Path Length Report")+coord_flip()
  #             )
  
  
  
  
  # output$mytable2 = renderDataTable(buget)
  # output$mytable3 = renderDataTable(iris)
  #
}

