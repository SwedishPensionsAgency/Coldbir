# Connect to database
db <- cdb(.help_args$path)

shinyServer(function(input, output) {
    output$histogram <- renderPlot({
        hist(db[input$variable], main = input$variable, labels = TRUE, col = "lightblue")
    })
})
