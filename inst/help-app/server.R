shinyServer(function(input, output) {
    output$show <- renderText({
        paste(.help_args$path, .help_args$search)  # global variable set by the cdb-class
    })
})
