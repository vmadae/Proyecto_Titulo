suppressWarnings(library(shiny))
suppressWarnings(library(knitr))
suppressWarnings(library(gt))

shinyServer(function(input,output){
  output$value <- renderPrint({input$more})
  output$DataScenarios <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = ("./FileSystem/Scenario.txt"), header = TRUE, sep = ",", dec = ".")
      fileData[c(1,3,6,7,8)]
    })
  )
  
  output$DataInstances <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Instances.txt", header = TRUE, sep = ",", dec = ".")
      fileData[c(1,9,10)]
    })
  )
  output$DataParameters <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Parameters.txt", header = TRUE, sep = ",", dec = ".")
      fileData[c(1,3,9,10)]
    })
  )
  output$DataTarget <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Target.txt", header = TRUE, sep = ",", dec = ".")
      fileData[c(1,5,6)]
    })
  )
  output$DataExperiment <- DT::renderDataTable(
    DT::datatable({
      fileData <- read.delim(file = "./FileSystem/Experiment.txt", header = TRUE, sep = ",", dec = ".")
      fileData[c(1,3,4,8,10,11)]
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