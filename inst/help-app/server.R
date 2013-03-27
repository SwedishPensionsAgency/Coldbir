require(markdown)
require(hwriter)
require(data.table)

# Connect to database
db <- cdb(.help_args$path, type = "f")

shinyServer(function(input, output) {
    output$plot <- renderPlot({
        x <- db[input$variable]
        
        if (is.factor(x)) {
            x <- sort(table(x))
            barplot(x, las = 1, horiz = TRUE, col = "lightblue")
        } else {
            x <- table(x)
            barplot(x, las = 1, horiz = FALSE, col = "lightblue")
        }
    })
    
    output$docs <- renderText({
        markdown::markdownToHTML(text = list_to_md(get_doc(db, input$variable)), fragment.only = TRUE)
    })
    
    output$summary <- renderText({
        x <- db[input$variable]
        if (is.factor(x)) {
            paste("<b>Levels:</b> ", paste(levels(x), collapse = ", "))
        } else {
            s <- summary(x)
            m <- matrix(s, nr = 1)
            colnames(m) <- names(s)
            hwrite(m, border = 0, cellpadding = 5)
        }
    })
})
