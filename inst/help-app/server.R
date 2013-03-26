library(shiny)

url_fields_to_sync <- c("beverage","milk","sugarLumps","customer");

# Define server logic required to respond to d3 requests
shinyServer(function(input, output) {
  
  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$order <- reactiveText(function() {
    paste(input$beverage, 
          if(input$milk) "with milk" else ", black",
          "and", 
          if (input$sugarLumps == 0) "no" else input$sugarLumps,
          "sugar lumps",
          "for",
          if (input$customer == "") "next customer" else input$customer)
  })
  
  firstTime <- TRUE
  
  output$hash <- reactiveText(function() {
    
    newHash = paste(collapse=",",
                    Map(function(field) {
                          paste(sep="=",
                                field,
                                input[[field]])
                        },
                        url_fields_to_sync))
    
    # the VERY FIRST time we pass the input hash up.
    return(
      if (!firstTime) {
        newHash
      } else {
        if (is.null(input$hash)) {
          NULL
        } else {
          firstTime<<-F;
          isolate(input$hash)
        }
      }
    )
  })
})