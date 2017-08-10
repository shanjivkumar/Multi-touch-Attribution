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
Channelpath<-read.csv("Channel path.csv")
attribution<-read.csv("Attribution.csv")
attribution_type<-read.csv("Attribution-types.csv")
pathlength<-read.csv("Path Length.csv")

server <- function(input,output){
  
  ########### Summary tab - Month Report #########
  buget$date <- as.POSIXct(strptime(as.character(buget$date), "%d/%m/%Y"))
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
  names(finaldataframemonth)<-c("month1","Marketing_Budget","Revenue")
  finaldataframemonth
  
  
  
  
  output$summarymonthplot1 <- renderPlotly({
    plot_ly(finaldataframemonth)%>%
      add_trace(x=~month1,y=~Revenue,type="bar",name="Revenue")%>%
      add_trace(x=~month1,y=~Marketing_Budget,type="scatter",mode="lines",name="Marketing Budget", fill = 'tozeroy')%>%#,yaxis="y2") %>%
      layout(title = 'Revenue & Marketing Budget',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Month"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
    
    #Below codeis to display the above chart using ggplot    
    
  #  ggplot(data=finaldataframemonth,aes(x=factor(month1)))+
   #   geom_bar(aes(y = Revenue,fill="Revenue"), stat="identity") +
    #  geom_line(aes(y = Marketing_Budget, group = 1, color = "Marketing_Budget")) +
     # scale_colour_manual(" ", values=c("Marketing_Budget" = "blue", "Revenue" = "red"))+
      #scale_fill_manual("",values="red")+
      #theme(legend.position="bottom",legend.key=element_blank(),
       #     legend.title=element_blank(),
        #    legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Revenue & Marketing Budget")+xlab("Date")+ylab("Value") 
    
    
  })
  
  

  output$summarymonthplot2 <- renderPlotly({
    plot_ly(buget, x = ~month, y = ~roi, color = ~Channel,type = 'bar')%>%
        layout(title = 'Top 5 Channels',xaxis=list(title="Month"),yaxis=list(title="roi"))
#Below codeis to display the above chart using ggplot        
    
    
     #   ggplot(data=buget,aes(x=(month),y=roi,fill=Channel)) +  
  #    geom_bar(position = "dodge", stat="identity") + ylab("Revenue On Conversions") + 
   #   xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
    #  ggtitle("Top 5 Channels")
    
  })
  
  output$summarymonthplot3 <- renderPlotly({
    
  plot_ly(buget, x = ~month, y = ~no.of.conversions, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Month"),yaxis=list(title="# of Conversions"))
#Below codeis to display the above chart using ggplot        
    
    
 #   ggplot(data=buget,aes(x=(month),y=no.of.conversions,group=Channel)) +
  #    geom_line(aes(color=Channel))+geom_point(aes(color=Channel))+ ylab("# Of Conversions") + 
   #   xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
    #  ggtitle("No of Conversions vs Channels")
  })
  
  output$summarymonthplot4 = renderDataTable(Channelpath,options=list(dom="t"))
  ###########Summary tab -  Quarter Report #########
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
  names(finaldataframe)<-c("quarter1","Marketing_Budget","Revenue")
  finaldataframe
  
  
  summaryquarterplot2<-aggregate(buget.new$roi~buget.new$quarter1+buget.new$Channel, FUN=sum)
  summaryquarterplot2
  names(summaryquarterplot2)<-c("quarter1","Channel","roi")
  summaryquarterplot3<-aggregate(buget.new$no.of.conversions~buget.new$quarter1+buget.new$Channel, FUN=sum)
  summaryquarterplot3
  names(summaryquarterplot3)<-c("quarter1","Channel","no.of.conversions")
  
  
  output$summaryquarterplot1 <- renderPlotly({
     plot_ly(finaldataframe)%>%
      add_trace(x=~quarter1,y=~Revenue,type="bar",name="Revenue")%>%
      add_trace(x=~quarter1,y=~Marketing_Budget,type="scatter",mode="lines",name="Marketing Budget", fill = 'tozeroy')%>%#,yaxis="y2") %>%
    layout(title = 'Revenue & Marketing Budget',
           xaxis = list(title = ""),
           yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h"))
#Below code commented is to display dual axis

        
      #layout(title = 'Revenue & Marketing Budget',
      #       xaxis = list(title = "Quarter"),
      #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
      #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))

#Below codeis to display the above chart using ggplot    
     #ggplot(data=finaldataframe,aes(x=factor(quarter1)))+
     # geom_bar(aes(y = Revenue,fill="Revenue"), stat="identity") +
      #geom_line(aes(y = Marketing_Budget, group = 1, color = "Marketing_Budget")) +
      #scale_colour_manual(" ", values=c("Marketing_Budget" = "blue", "Revenue" = "red"))+
      #scale_fill_manual("",values="red")+
      #theme(legend.position="bottom",legend.key=element_blank(),
      #      legend.title=element_blank(),
      #      legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Revenue & Marketing Budget")+xlab("Date")+ylab("Value")     
  })
  
  output$summaryquarterplot2 <- renderPlotly({
    plot_ly(buget, x = ~quarter1, y = ~roi, color = ~Channel,type = 'bar')%>%
      layout(title = 'Top 5 Channels',xaxis=list(title="Quarter"),yaxis=list(title="roi"))
#Below codeis to display the above chart using ggplot        
    
    
    #ggplot(data=summaryquarterplot2,aes(x=factor(quarter1),y=roi,fill=Channel)) + 
     # geom_bar(position = "dodge", stat="identity") + ylab("Revenue On Conversions") + 
      #xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      #ggtitle("Top 5 Channels")
  })
  
  output$summaryquarterplot3 <- renderPlotly({
    
    plot_ly(summaryquarterplot3, x = ~quarter1, y = ~no.of.conversions, color = ~Channel, type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Quarter"),yaxis=list(title="# of Conversions"))
#Below codeis to display the above chart using ggplot        
    
    
    #ggplot(data=summaryquarterplot3,aes(x=factor(quarter1),y=(no.of.conversions),group=Channel)) +
    # geom_line(aes(color=Channel))+geom_point(aes(color=Channel))+ ylab("# Of Conversions") + 
    # xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
    # ggtitle("No of Conversions vs Channels")
  })
  
  output$summaryquarterplot4 = renderDataTable(Channelpath,options=list(dom="t"))

  ###########Summary tab -  Year Report #########
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
  names(finaldataframeyear)<-c("yeardate","Marketing_Budget","Revenue")
  finaldataframeyear
  
  
  summaryyearplot2<-aggregate(buget.new$roi~buget.new$yeardate+buget.new$Channel, FUN=sum)
  names(summaryyearplot2)<-c("yeardate","Channel","roi")
  
  summaryyearplot3<-aggregate(buget.new$no.of.conversions~buget.new$yeardate+buget.new$Channel, FUN=sum)
  summaryyearplot3
  names(summaryyearplot3)<-c("yeardate","Channel","no.of.conversions")
  
  output$summaryyearplot1 <- renderPlotly({
    plot_ly(finaldataframeyear)%>%
      add_trace(x=~yeardate,y=~Revenue,type="bar",name="Revenue")%>%
      add_trace(x=~yeardate,y=~Marketing_Budget,type="scatter",mode="lines",name="Marketing Budget", fill = 'tozeroy')%>%#,yaxis="y2") %>%
      layout(title = 'Revenue & Marketing Budget',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h"))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Month"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
    
    #Below codeis to display the above chart using ggplot    
    
    
  #     ggplot(finaldataframeyear, aes(yeardate)) + 
   #   geom_bar(aes(y = Revenue,fill="Revenue"), stat="identity") +
    #  geom_line(aes(y = Marketing_Budget, group = 1, color = "Marketing_Budget")) +
     # scale_colour_manual(" ", values=c("Marketing_Budget" = "blue", "Revenue" = "red"))+
      #scale_fill_manual("",values="red")+
      #theme(legend.position="bottom",legend.key=element_blank(),
       #     legend.title=element_blank(),
        #    legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Revenue & Marketing Budget")+xlab("Date")+ylab("Value")    
  })
  
  output$summaryyearplot2 <- renderPlotly({
    plot_ly(buget, x = ~yeardate, y = ~roi, color = ~Channel,type = 'bar')%>%
      layout(title = 'Top 5 Channels',xaxis=list(title="Year"),yaxis=list(title="roi"))
#Below codeis to display the above chart using ggplot        
    
    
    
    #ggplot(data=summaryyearplot2,aes(x=(yeardate),y=roi,fill=Channel)) +  
     # geom_bar(position = "dodge", stat="identity") + ylab("Revenue On Conversions") + 
      #xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      #ggtitle("Top 5 Channels")
  })
  
  output$summaryyearplot3 <- renderPlotly({
    plot_ly(summaryyearplot3, x = ~yeardate, y = ~no.of.conversions, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Year"),yaxis=list(title="# of Conversions"))
#Below codeis to display the above chart using ggplot        
    
    
    
 #   ggplot(data=summaryyearplot3,aes(x=(yeardate),y=no.of.conversions,group=Channel)) +
 #   geom_line(aes(color=Channel))+geom_point(aes(color=Channel))+ ylab("# Of Conversions") + 
 #     xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
 #    ggtitle("No of Conversions vs Channels")
  })
  
  output$summaryyearplot4 = renderDataTable(Channelpath,options=list(dom="t"))
  
  
  ############################Attribution################################################

  output$plot10 <- renderPlot({
    ggplot(data=Attribution-types,aes(x=(Channel),y=(Percentage_conversion))) +  
      geom_bar(position = "dodge", stat="identity") + ylab("% Of Conversions") + 
      xlab("Channel") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Attribution summary")
  })
  
  
  
  ##Sample try
  
  #attribution$RevenueP <- round((attribution$Revenue/sum(attribution$Revenue))*100)
  #new_data <-attribution[,input$AttributionType]
  #new_data<-melt(attribution,id.vars = c("Channel","AttributionType"),measure.vars=c("Percentage_conversion","Percentage_revenue"))
  
  output$plot11 <- renderPlotly({
    plot_ly(data = subset(attribution,AttributionType==input$AttributionType), x = ~Channel, y = ~Percentage_revenue, type = 'bar',name = "Revenue Percent") %>%
      add_trace(y = ~Percentage_conversion, name = 'Conversion Percent') %>%
      layout(title = subset(attribution,AttributionType==input$AttributionType)[1,"AttributionType"],yaxis = list(title = 'Percentage'), barmode = 'group', legend = list(orientation = "h", anchor = "center", x = 0.56) )
  })
  
  output$mytable1 = DT::renderDataTable(DT::datatable({ 
    attribution [attribution$AttributionType== input$AttributionType,c("Channel","Conversions","Percentage_conversion","Revenue","Total_Cost","Cost_per_conversion")]},rownames= FALSE,options = list(dom = 't')))
  #output$mytable1 = renderDataTable({attribution[,c("Channel","Percentage.Conversion","Revenue","Cost.Conversion","No.of.Conversions")]})
  
  #########################Channel tab - Month Report###############################################
 

  
  output$channelmonthplot1 <- renderPlotly({
    plot_ly(buget)%>%
      add_trace(x=~month1,y=~roi,type="bar",name="Revenue",color = ~Channel)%>%
      #add_trace(x=~month1,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Revenue',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Month"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
    
    #Below codeis to display the above chart using ggplot    
    
    #  ggplot(data=finaldataframemonth,aes(x=factor(month1)))+
    #   geom_bar(aes(y = Revenue,fill="Revenue"), stat="identity") +
    #  geom_line(aes(y = Marketing_Budget, group = 1, color = "Marketing_Budget")) +
    # scale_colour_manual(" ", values=c("Marketing_Budget" = "blue", "Revenue" = "red"))+
    #scale_fill_manual("",values="red")+
    #theme(legend.position="bottom",legend.key=element_blank(),
    #     legend.title=element_blank(),
    #    legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Revenue & Marketing Budget")+xlab("Date")+ylab("Value") 
    
    
  })
  
  
  
  output$channelmonthplot2 <- renderPlotly({
    plot_ly(buget)%>%
      add_trace(x=~month1,y=~marketing.budget,type="bar",name="Marketing Budget",color = ~Channel)%>%
      #add_trace(x=~month1,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Marketing Budget',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Month"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
    
    #Below codeis to display the above chart using ggplot    
    
    #  ggplot(data=finaldataframemonth,aes(x=factor(month1)))+
    #   geom_bar(aes(y = Revenue,fill="Revenue"), stat="identity") +
    #  geom_line(aes(y = Marketing_Budget, group = 1, color = "Marketing_Budget")) +
    # scale_colour_manual(" ", values=c("Marketing_Budget" = "blue", "Revenue" = "red"))+
    #scale_fill_manual("",values="red")+
    #theme(legend.position="bottom",legend.key=element_blank(),
    #     legend.title=element_blank(),
    #    legend.box="horizontal",plot.title = element_text(size=15, face="bold"))+ggtitle("Revenue & Marketing Budget")+xlab("Date")+ylab("Value") 
    
    
  })
  output$channelmonthplot3 <- renderPlotly({
    
    plot_ly(buget, x = ~month, y = ~Visits, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'Visits trend',xaxis=list(title="Month"),yaxis=list(title="# of Visits"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below codeis to display the above chart using ggplot        
    
    
    #   ggplot(data=buget,aes(x=(month),y=no.of.conversions,group=Channel)) +
    #    geom_line(aes(color=Channel))+geom_point(aes(color=Channel))+ ylab("# Of Conversions") + 
    #   xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
    #  ggtitle("No of Conversions vs Channels")
  })
  
  output$channelmonthplot4 <- renderPlotly({
    
    plot_ly(buget, x = ~month, y = ~no.of.conversions, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Month"),yaxis=list(title="# of Conversions"))%>%
      layout(legend=list(orientation="h",y=-0.3))
    #Below codeis to display the above chart using ggplot        
    
    
    #   ggplot(data=buget,aes(x=(month),y=no.of.conversions,group=Channel)) +
    #    geom_line(aes(color=Channel))+geom_point(aes(color=Channel))+ ylab("# Of Conversions") + 
    #   xlab("Date") + theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
    #  ggtitle("No of Conversions vs Channels")
  })
  output$channelmonthplot5 <- renderPlotly({
    
    plot_ly(buget, x = ~month, y = ~Cost.per.conversion, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'Cost Per Conversion VS Channels',xaxis=list(title="Month"),yaxis=list(title="Cost Per Conversion"))%>%
      layout(legend=list(orientation="h",y=-0.3))
   
  })
  
  #########################Channel tab - Quarter Report###############################################
  quarter<-quarter(buget$date)
  year<-year(buget$date)
  quarter1<-paste(year," Q",quarter,sep="")
  buget.new<-data.frame(quarter1,buget)
  
  quarterbudget<-aggregate(buget.new$marketing.budget~buget.new$quarter+buget.new$Channel, FUN=sum)
  quarterbudget
  names(quarterbudget)<-c("quarter1","Channel","marketing.budget")
  
  quarterroi<-aggregate(buget.new$roi~buget.new$quarter+buget.new$Channel, FUN=sum)
  quarterroi
  names(quarterroi)<-c("quarter1","Channel","roi")
  
  quarternoofconversion<-aggregate(buget.new$no.of.conversions~buget.new$quarter+buget.new$Channel,FUN=sum)
  quarternoofconversion
  names(quarternoofconversion)<-c("quarter1","Channel","no of conversion")
  
  quartercostperconversion<-aggregate(buget.new$Cost.per.conversion~buget.new$quarter+buget.new$Channel,FUN=sum)
  quartercostperconversion
  names(quartercostperconversion)<-c("quarter1","Channel","cost per conversion")
  
  quartervisit<-aggregate(buget.new$Visits~buget.new$quarter+buget.new$Channel,FUN=sum)
  quartervisit
  names(quartervisit)<-c("quarter1","Channel","Visit")
  
  finalquarterdataframe<-data.frame(quarterbudget,quarterroi$roi,quarternoofconversion$`no of conversion`,quartercostperconversion$`cost per conversion`,quartervisit$Visit)
  finalquarterdataframe
  names(finalquarterdataframe)<-c("quarter1","Channel","marketing.budget","roi","no.of.conversions","cost.per.conversion","Visits")
  finalquarterdataframe
  
  
  

  output$channelquarterplot1 <- renderPlotly({
    plot_ly(finalquarterdataframe)%>%
      add_trace(x=~quarter1,y=~roi,type="bar",name="Revenue",color = ~Channel)%>%
      #add_trace(x=~quarter1,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Revenue',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Quarter"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
  })

  
  output$channelquarterplot2 <- renderPlotly({
    plot_ly(finalquarterdataframe)%>%
      add_trace(x=~quarter1,y=~marketing.budget,type="bar",name="Marketing Budget",color = ~Channel)%>%
      #add_trace(x=~quarter1,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Marketing Budget',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))

  })
  
    
  output$channelquarterplot3 <- renderPlotly({
    
    plot_ly(finalquarterdataframe, x = ~quarter1, y = ~Visits, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'Visits trend',xaxis=list(title="Quarter"),yaxis=list(title="# of Visits"))%>%
      layout(legend=list(orientation="h",y=-0.3))

  })
  output$channelquarterplot4 <- renderPlotly({
    
    plot_ly(finalquarterdataframe, x = ~quarter1, y = ~no.of.conversions, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Quarter"),yaxis=list(title="# of Conversions"))%>%
      layout(legend=list(orientation="h",y=-0.3))
    
  })
  output$channelquarterplot5<-renderPlotly({
    plot_ly(finalquarterdataframe,x=~quarter1,y=~cost.per.conversion,color=~Channel,type="scatter",mode="lines+markers")%>%
      layout(title="Cost Per Conversion VS Channels",xaxis=list(title="Quarter"),yaxis=list(title="Cost Per Conversion"))%>%
      layout(legend=list(orientation="h",y=-0.3))
  })
  
  #########################Channel tab - Year Report###############################################
  buget$year<-year(buget$date)
  yeardate<-as.integer(buget$year)
  buget.new<-data.frame(yeardate,buget)
  
  yearbudget<-aggregate(buget.new$marketing.budget~buget.new$yeardate+buget.new$Channel, FUN=sum)
  yearbudget
  names(yearbudget)<-c("yeardate","Channel","marketing.budget")
  
  yearroi<-aggregate(buget.new$roi~buget.new$yeardate+buget.new$Channel, FUN=sum)
  yearroi
  names(yearroi)<-c("yeardate","Channel","roi")
  
  yearnoofconversion<-aggregate(buget.new$no.of.conversions~buget.new$yeardate+buget.new$Channel,FUN=sum)
  yearnoofconversion
  names(yearnoofconversion)<-c("yeardate","Channel","no of conversion")
  
  yearcostperconversion<-aggregate(buget.new$Cost.per.conversion~buget.new$yeardate+buget.new$Channel,FUN=sum)
  yearcostperconversion
  names(yearcostperconversion)<-c("yeardate","Channel","cost per conversion")
  
  yearvisit<-aggregate(buget.new$Visits~buget.new$yeardate+buget.new$Channel,FUN=sum)
  yearvisit
  names(yearvisit)<-c("yeardate","Channel","Visit")
  
  finalyeardataframe<-data.frame(yearbudget,yearroi$roi,yearnoofconversion$`no of conversion`,yearcostperconversion$`cost per conversion`,yearvisit$Visit)
  finalyeardataframe
  names(finalyeardataframe)<-c("yeardate","Channel","marketing.budget","roi","no.of.conversions","cost.per.conversion","Visits")
  finalyeardataframe
  
  
  
  output$channelyearplot1 <- renderPlotly({
    plot_ly(finalyeardataframe)%>%
      add_trace(x=~yeardate,y=~roi,type="bar",name="Revenue",color = ~Channel)%>%
      #add_trace(x=~yeardate,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Revenue',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
    #Below code commented is to display dual axis
    
    
    #layout(title = 'Revenue & Marketing Budget',
    #       xaxis = list(title = "Quarter"),
    #       yaxis = list(side = 'left', title = 'Revenue', showgrid = FALSE, zeroline = FALSE),
    #       yaxis2 = list(side = 'right', overlaying = "y", title = 'Marketing Budget', showgrid = FALSE, zeroline = FALSE))
  })
  
  output$channelyearplot2 <- renderPlotly({
    plot_ly(finalyeardataframe)%>%
      add_trace(x=~yeardate,y=~marketing.budget,type="bar",name="Marketing Budget",color = ~Channel)%>%
      #add_trace(x=~yeardate,y=~marketing.budget,type="scatter",mode = 'lines+markers',name="Marketing Budget")%>%#,yaxis="y2") %>%
      layout(title = 'Marketing Budget',
             xaxis = list(title = ""),
             yaxis=list(title="Value"))%>%
      layout(legend=list(orientation="h", y = -0.3))
  })
  
  output$channelyearplot3 <- renderPlotly({
    plot_ly(finalyeardataframe, x = ~yeardate, y = ~Visits, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'Visits trend',xaxis=list(title="Year"),yaxis=list(title="# of Visits"))%>%
      layout(legend=list(orientation="h",y=-0.3))
  })
  
  output$channelyearplot4 <- renderPlotly({
    plot_ly(finalyeardataframe, x = ~yeardate, y = ~no.of.conversions, color = ~Channel,type = 'scatter', mode = 'lines+markers')%>%
      layout(title = 'No of Conversions VS Channels',xaxis=list(title="Year"),yaxis=list(title="# of Conversions"))%>%
      layout(legend=list(orientation="h",y=-0.3))
  })
 output$channelyearplot5<- renderPlotly({
   plot_ly(finalyeardataframe,x=~yeardate,y=~cost.per.conversion,color = ~Channel,type = "scatter",mode="lines+markers")%>%
     layout(title="Cost Per Conversion VS Channels",xaxis=list(title="Year"),yaxis=list(title="Cost Per Conversion"))%>%
     layout(legend=list(orientation="h",y=-0.3))
 })   
  #############################Path Report###################
  output$pathreportplot1 = renderDataTable(pathlength,options = list(dom = 't'))
  pathlength1<-pathlength
  pathlength1$Conversion.percentage<-(pathlength1$Conversions/sum(pathlength1$Conversions))*100
  positions <- c("+10","9","8","7","6","5","4","3","2","1")
  pathlength1$Path.Length.in.Interactions <- factor(pathlength1$Path.Length.in.Interactions, levels = c("10+","9","8","7","6","5","4","3","2","1"))
  
  output$pathreportplot2 <- renderPlotly({
    plot_ly(pathlength1, y = ~Path.Length.in.Interactions, x = ~Conversion.percentage,type = 'bar', orientation = 'h')%>%
      layout(title="Percentage of Total Conversion")
    
    #ggplotly(ggplot(data=pathlength1,aes(x=(Path.Length.in.Interactions),y=Conversion.percentage)) +scale_x_discrete(limits = positions)+  
    #           geom_bar(position = "dodge", stat="identity") + ylab("Conversions Percentage") + 
    #           xlab("Path Length in Interactions") + theme(legend.position="center" ,plot.title = element_text(size=15, face="bold")) + 
    #           ggtitle("                                   
    #                    Path Length Report")+coord_flip()
    #             )
})
  
  output$pathreportplot3 = renderDataTable(Channelpath,options=list(dom="t"))
  
  
  
  # output$mytable2 = renderDataTable(buget)
  # output$mytable3 = renderDataTable(iris)
  #
}

