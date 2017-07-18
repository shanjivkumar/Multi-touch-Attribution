ui <-  dashboardPage(
  
  dashboardHeader(title = strong("Funnel Chart")),        
  
  dashboardSidebar( 
    sidebarMenu(
      menuItem(h4(strong("Funnel Chart")),tabName="Funnel"))),
  
  dashboardBody(
    tabItem(tabName="Funnel",box(title = strong("Funnel Chart"), width = 12, solidHeader = TRUE, collapsible  = TRUE, status="info", showOutput("myChart", "highcharts"))))
)

server <- function(input,output)
{
  output$myChart <- renderChart3({
    h1 <- Highcharts$new()
    h1$series(data = list(list("x",10),list('y',5)),type="funnel")
    tags$head(tags$script(src = "http://code.highcharts.com/modules/funnel.js"))
    return(h1)})
}
shinyApp(ui=ui,server=server)