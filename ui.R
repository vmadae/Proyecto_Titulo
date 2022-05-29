suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(gt))

shinyUI(fluidPage(
  theme = shinytheme("united"),
  #Aplication title
  titlePanel(h1("Benchmark management system for irace", align = "center")),
  title = "Benchmark management system for irace",
  hr(),
  
  fluidPage(tabsetPanel(
    tabPanel("Scenario", 
             titlePanel(h2("Scenario")),
             hr(),
             fluidRow(
               column(1,  actionButton("more", label = "See more")
               ),
               column(11,
                      DT::dataTableOutput("DataScenarios", width = 1)
               )
             )
            ),
    tabPanel("Instances",
             titlePanel(h2("Instances")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataInstances", width = 5)
               )
             )
            ),
    tabPanel("Parameters", 
             titlePanel(h2("Parameters")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataParameters", width = 5)
               )
             )
            ),
    tabPanel("Target", 
             titlePanel(h2("Target")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataTarget", width = 5)
               )
             )
            ),
    tabPanel("Experiments", 
             titlePanel(h2("Experiments")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataExperiment", width = 5)
               )
             )
            ),
    tabPanel("Iterations",
             titlePanel(h2("Iterations")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataIterations", width = 5)
               )
             )
            ),
    tabPanel("Version", 
             titlePanel(h2("Versions")),
             hr(),
             fluidRow(
               column(1,
               ),
               column(11,
                      DT::dataTableOutput("DataVersion", width = 5)
              )
            )
            ),
    tabPanel("More info", 
             titlePanel(h2("More info")),
             hr(),
             h3("HECHO POR")
    )
    ))
))
