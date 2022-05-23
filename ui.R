suppressWarnings(library(shiny))

shinyUI(fluidPage(
  #Aplication title
  titlePanel("Benchmark management system for irace"),
  
  fluidPage(tabsetPanel(
    tabPanel("Scenario", 
             titlePanel("Scenario"),
             hr(),
             DT::dataTableOutput("DataScenarios", width = 12)
            ),
    tabPanel("Instances",
             titlePanel("Instances"),
             hr(),
             DT::dataTableOutput("DataInstances", width = 5)
            ),
    tabPanel("Parameters", 
             titlePanel("Parameters"),
             hr(),
             DT::dataTableOutput("DataParameters", width = 5)
            ),
    tabPanel("Target", 
             titlePanel("Target"),
             hr(),
             DT::dataTableOutput("DataTarget", width = 5)
            ),
    tabPanel("Experiments", 
             titlePanel("Experiments"),
             hr(),
             DT::dataTableOutput("DataExperiment", width = 5)
            ),
    tabPanel("Iterations",
             titlePanel("Iterations"),
             hr(),
             DT::dataTableOutput("DataIterations", width = 5)
            ),
    tabPanel("Version", 
             titlePanel("Versions"),
             hr(),
             DT::dataTableOutput("DataVersion", width = 5)
             )))
))
