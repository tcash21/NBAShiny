library(shiny)
library(rCharts)
 
shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
 
  headerPanel(title=""),
  sidebarPanel(
     #includeCSS('app.css'),
     dateInput("date", "Select a date:", value=format(Sys.Date(), "%m/%d/%Y"), format="mm/dd/yyyy")
    ),
  
  
  
  mainPanel(
     #includeCSS('app.css'),   
     showOutput('results', 'datatables'),
     tableOutput('last10'),
     tableOutput('acc'),
     tableOutput('acc2')
    )
    
))
