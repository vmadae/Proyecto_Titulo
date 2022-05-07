library(argparser, quietly=TRUE)
library(readr)
library("fs")  # File manipulations
library(tidyverse)  # Data manipulation

# Create a parser
p <- arg_parser("Help")

# Add command line arguments

#Arguments to list

p <- add_argument(p, short = "--ls", "--list_scenario", help="List all scenarios in the system", flag=TRUE)
p <- add_argument(p, short = "--lt", "--list_target", help="List all target algorithms", type="string", flag=TRUE)
p <- add_argument(p, short = "--lp", "--list_parameters", help="List all parameter sets", type="string", flag=TRUE)
p <- add_argument(p, short = "--li", "--list_instances", help="List all instance sets", type="string", flag=TRUE)
p <- add_argument(p, short = "--lv", "--list_versions", help="List all versions of irace", type="string", flag=TRUE)
p <- add_argument(p, short = "--lb", "--list_iteration", help="List all registered tests", type="string", flag=TRUE)

#Arguments to display
p <- add_argument(p, short = "--vs", "--view_scenario", help="Show details of a scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "--vt", "--view_target", help="Show the detail of a target algorithm", type="string", flag=TRUE)
p <- add_argument(p, short = "--vp", "--view_parameters", help="Show the detail of a set of parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "--vi", "--view_instances", help="Show the detail of a set of instances", type="string", flag=TRUE)
p <- add_argument(p, short = "--vv", "--view_version", help="Show version details", type="string", flag=TRUE)
p <- add_argument(p, short = "--vb", "--view_test", help="Show test details", type="string", flag=TRUE)

#Argument to see results
p <- add_argument(p, short = "--r", "--results", help="Show results (experiments) of a test", type="string", flag=TRUE)

#Arguments to test
p <- add_argument(p, short = "--ab", "--add_test", help="Create a test, add: 1.Name 2.Description 3.Irace version 4.Scenarios and their repetitions", type="string", flag=TRUE)
p <- add_argument(p, short = "--xb", "--execute_test", help="Run a test", type="string", flag=TRUE)

#Arguments to add
p <- add_argument(p, short = "--at", "--add_target", help="Add target", type="string", flag=TRUE)
p <- add_argument(p, short = "--ap", "--add_parameters", help="Add parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "--ai", "--add_instances", help="Add instances", type="string", flag=TRUE)
p <- add_argument(p, short = "--as", "--add_scenario", help="Add scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "--av", "--add_version", help="Add new version", type="string", flag=TRUE)

#Argument to modificate
p <- add_argument(p, short = "--mt", "--modify_target", help="Modify target", type="string", flag=TRUE)
p <- add_argument(p, short = "--mp", "--modify_parameters", help="Modify parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "--mi", "--modify_instances", help="Modify instances", type="string", flag=TRUE)
p <- add_argument(p, short = "--ms", "--modify_scenario", help="Modify scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "--mv", "--modify_version", help="Modify version", type="string", flag=TRUE)

#Arguments to create website
p <- add_argument(p, "--web", help="Generate website in shiny", type="string", flag=TRUE)

# Parse the command line arguments
args <- parse_args(p)

######################################################################################
#Functionality of argument

#Argument to add
if(args$add_target){
  #Request data from the user
  targetName <- readline("Enter the name of the target algorithm to add: " )
  targetDescription <- readline("Enter a description corresponding to the target to enter: ")
  routeTargetRunner <- readline("Enter the path where the target's runner file is hosted: ")
  executablePathTarget <- readline("Enter the path of the target executable: ")
  
  #Add data to the file system
  
  targetData <- list(targetName, targetDescription, routeTargetRunner, executablePathTarget)
  
  setwd("../Proyecto_titulo/FileSystem")
  write.table(targetData, file = "Target.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
  
  
  #Add files to the file system
}

#Argument to list
if(args$list_scenario){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Scenario.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

if(args$list_target){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Target.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

if(args$list_parameters){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Parameters.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

if(args$list_instances){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Instances.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

if(args$list_versions){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Version.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

if(args$list_iteration){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Iteration.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#Argument to view

