library(shiny)

hashProxy <- function(inputoutputID) {
  div(id=inputoutputID,class=inputoutputID,tag("div",""));
}

# Define UI for shiny d3 chatter application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Persisting Input in URL",
              "Demo of how to persist inputs in browser URL"),
  
  sidebarPanel(
    tags$p("This widget is a demonstration of how to preserve input state across sessions, using the URL hash."),
    selectInput("beverage", "Choose a beverage:", 
                choices = c("Tea", "Coffee", "Cocoa")),
    checkboxInput("milk", "Milk"),
    sliderInput("sugarLumps", "Sugar Lumps:", 
                min=0, max=10, value=3),
    textInput("customer", "Your Name:")
  ),
  
  mainPanel(
    includeHTML("URL.js"),
    h3(textOutput("order")),
    hashProxy("hash")
  )
))