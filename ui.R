library(shiny)
library(DT)
library(ggthemes)
library("shinyjs")
source("getGerman.R") 
library("shinyjs")
library("rpart.plot") 

# Define UI - one panel, 4 tabs
shinyUI(fluidPage(
  # Application title
  titlePanel("Interactive Sandbox for a Classification Tree Model"),
  # Sidebar with all inputs selected and nice buttom 
  sidebarLayout(
    sidebarPanel(
      h3("Variables selected"),
      verbatimTextOutput('out7'),
        useShinyjs(),
        disabled(actionButton(
        inputId = "submit_loc",
        label = "Submit"))
        ),
    
    # Main Panel and the 5 tabs - 
    
    mainPanel(
      tabsetPanel(
        # tab 01 - Instructions
        tabPanel("Instructions",
                 includeHTML("static.html") # use html to make ui.r cleaner.
                ),
        
        # tab 02 - Binary variables - uses datatable 
        tabPanel("Binary",
                 h3("Click on the lines that will be part of the model"),
                 p("This dinamic table allows sorting, search and pagination. Select variables by clicking on the lines - start by selecting the survey question 'No checking account with bank'"),
                 DT::dataTableOutput('mytable1')
        ),
        
        
        # tab 03 - Categorical variables - uses selectInput to show tables 
                tabPanel("Categorical",
                 h3("Select the variable you want to see the contingency table and the variables you want to be considered in the model"),
                 p("This page displays a contigency table based on the user selection"),
                 selectInput('in21', 'Contigency Table',categovars$longNames, multiple=FALSE, selectize=TRUE),
                 selectInput('in22', 'Selection for Model',categovars$longNames, multiple=TRUE, selectize=TRUE),
                 DT::dataTableOutput('mytable5')
        ),
        
        
        # tab 04 - Cont. variables - uses selection to show box plot 
                tabPanel("Continuous",
                 h3("Select the variable you want to see the box plot table and the variables you want to be considered in the model"),
                 p("This page displays a box plot graph as a function of the credit classification"),
                 selectInput('in31', 'Graph', continvars$longNames, multiple=FALSE, selectize=TRUE),
                 selectInput('in32', 'Selection for Model', continvars$longNames, multiple=TRUE, selectize=TRUE),
                 plotOutput('plot1')

        ),  
        
        # tab 05 - Results 
        tabPanel("Results",
                 h4("Summary of the classification tree"),
                 h5(textOutput('model8')),
                 h5(textOutput('model9')),
                 h5(textOutput('model10')),
                 h5("Example of a decision tree:"),
                 plotOutput('model13'),
                 h5("Confusion Matrix"),
                 verbatimTextOutput('model15'),
                 h5("Additonal info about the model:"),
                 verbatimTextOutput('model11')
                 
               
        )
        
      )
    )
  ))
)