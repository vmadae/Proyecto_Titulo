suppressWarnings(library(shiny))

shinyServer(function(input,output){
  output$DataScenarios <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Scenario.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataInstances <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Instances.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataParameters <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Parameters.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataTarget <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Target.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataExperiment <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Experiment.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataIterations <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Iteration.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
  output$DataVersion <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Version.txt", header = TRUE, sep = ",", dec = ".")
    })
  )
})