# Connect to database
db <- cdb(.help_args$path)

# Fetch all variables
list_vars <- vars(db)
list_vars <- setNames(as.list(list_vars), list_vars)

# Interface
shinyUI(bootstrapPage(
     selectInput(inputId = "variable",
                 label = "Variables:",
                 choices = list_vars),
    plotOutput("histogram")
))