# Connect to database
db <- cdb(.help_args$path, type = "f")

# Fetch all variables
list_vars <- vars(db)
list_vars <- setNames(as.list(list_vars), list_vars)

# Interface
shinyUI(pageWithSidebar(
    
    headerPanel(""),
    
    sidebarPanel(
        radioButtons(inputId = "variable",
            label = "",
            choices = list_vars)
    ),
    
    mainPanel(
        htmlOutput("docs"),
        plotOutput("plot"),
        htmlOutput("summary")
    )
))