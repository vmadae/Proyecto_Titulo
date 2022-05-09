library(argparser, quietly=TRUE)
library(readr)


###############################################################################################################
# Add command line arguments
###############################################################################################################

# Create a parser
p <- arg_parser("Help")

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

###############################################################################################################
#Functionality of argument
###############################################################################################################
#Argument to add
###############################################################################################################

#add target
if(args$add_target){
  #Request data from the user
  targetName <- readline("Enter the name of the target algorithm to add: " )
  targetDescription <- readline("Enter a description corresponding to the target to enter: ")
  routeTargetRunner <- readline("Enter the path where the target's runner file is hosted: ")
  nameTargetRunner <- readline("Enter the target runner filename with file type: ")
  executablePathTarget <- readline("Enter the path of the target executable: ")
  nameExecutableTarget <- readline("Enter the target executable filename with file type: ")
  
  #Add files to the file system
  #Create folder
  setwd("../Proyecto_titulo/FileSystem/Files/Target")
  dir.create(targetName)
  
  
  fileRoot <- getwd()
  route <- paste(fileRoot, targetName, sep = "/") 
  setwd(route)
  
  #add to target runner
  file.copy(routeTargetRunner, route)
  routeTargetRunner <- paste(route, nameTargetRunner, sep = "/") 
  #add to path target
  route <- paste(fileRoot, targetName, sep = "/")
  file.copy(executablePathTarget, route) 
  executablePathTarget <- paste(route, nameExecutableTarget, sep = "/") 
  
  setwd("../../../..")
  
  #Add data to the file system
  targetData <- list(targetName, targetDescription, routeTargetRunner, executablePathTarget)
  setwd("../Proyecto_titulo/FileSystem")
  write.table(targetData, file = "Target.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
}

#add parameters
if(args$add_parameters){
  #Request data from the user
  parametersName <- readline("Enter the name of the parameter to add: ")
  targetAlgorithm <- readline("Enter the target algorithm to use: ")
  parameters <- readline("Archivo de texto que lista parametros que irace debe configurar .txt: ")
  nameParameters <- readline("Ingresar nombre del archivo de parametro con el .txt: ")
  typeParameters <- readline("Enter the type of parameter: ")
  forbidden <- readline("Archivo de texto que lista combinaciones prohibidas de valores de parametros .txt (opcional): ")
  nameForbidden <- readline("Ingrese el nombre del archivo forbiden incluyendo .txt:" )
  initial <- readline("Archivo de texto que provee configuraciones para iniciar la busqueda en irace (opcional) .txt: ")
  nameInitial <- readline("Ingrese el nombre de initial con el .txt")
  
  #Add files to the file system
  #Create folder
  setwd("../Proyecto_titulo/FileSystem/Files/Parameters")
  dir.create(parametersName)
  
  fileRoot <- getwd()
  route <- paste(fileRoot, parametersName, sep = "/") 
  setwd(route)
  
  #add to parameters file
  file.copy(parameters, route)
  parameters <- paste(route, nameParameters, sep = "/")
  #add to forbidden file
  file.copy(forbidden, route)
  forbidden <- paste(route, nameForbidden, sep = "/")
  #add to initial file
  file.copy(initial, route)
  initial <- paste(route, nameInitial, sep = "/")
  
  setwd("../../../..")
  
  routeFile <- route
  
  #Add data to the file system
  parameterData <- list(parametersName, targetAlgorithm, parameters, typeParameters, forbidden, initial, routeFile)
  setwd("../Proyecto_titulo/FileSystem")
  write.table(parameterData, file = "Parameters.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
}

#add instances
if(args$add_instances){
  #Request data from the user
  instanceName <- readline("Enter the instance name to add: " )
  instanceDescription <- readline("Enter the instance description to add: " )
  instanceTraining <- readline("Enter the instance training to add: " )
  instanceNumberTraining <- readline("Enter the instance number of the training to add: " )
  instanceTesting <- readline("Enter the instance testing to add: " )
  instanceNumberTesting <- readline("Enter the number of the testing to add: " )
  instanceRouteTraining <- readline("Enter the route of the instance training to add: " )
  instanceRouteTrainingName <- readline("Enter the route of the instance training name to add: ")
  instanceRouteTesting <- readline("Enter the route of the instance testing to add: " )
  instanceRouteTestingName <-readline("Enter the route of the instance testing name to add: ")
  
  #Add files to the file system
  #Create folder
  setwd("../Proyecto_titulo/FileSystem/Files/Instances")
  dir.create(instanceName) 
  
  fileRoot <- getwd()
  route <- paste(fileRoot, instanceName, sep = "/") 
  setwd(route)
  
  #add to instance training 
  file.copy(instanceRouteTraining, route)
  instanceRouteTraining <- paste(route, instanceRouteTrainingName, sep = "/")
  #add to instance testing
  file.copy(instanceRouteTesting, route)
  instanceRouteTesting <- paste(route, instanceRouteTestingName, sep = "/")
  
  setwd("../../../..")
  
  #Add data to the file system
  instanceData <- list(instanceName, instanceDescription, instanceTraining, instanceNumberTraining, instanceTesting, instanceNumberTesting, instanceRouteTraining, instanceRouteTesting)
  setwd("../Proyecto_titulo/FileSystem")
  write.table(instanceData, file = "Instances.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
}

#add scenario
if(args$add_scenario){
  #Request data from the user
  scenarioName <- readline("Enter the name of scenario to add: ")
  parameterSpace <- readline("Enter the space parameter for the scenario: ")
  setInstances <- readline("Enter the set of instances for the scenario: ")
  optionsRoute <- readline("Enter the options rout for the scenario (archivo .txt): ")
  nameOptionFile <- readline("Ingresar el nombre del archivo con junto a la terminacion .txt")
  scenarioType <- readline("Enter the type of the scenario: ")
  
  #Add files to the file system
  #Create folder
  setwd("../Proyecto_titulo/FileSystem/Files/Scenario")
  dir.create(scenarioName)
  
  fileRoot <- getwd()
  route <- paste(fileRoot, scenarioName, sep = "/") 
  setwd(route)
  
  #add to option route 
  file.copy(optionsRoute, route)
  optionsRoute <- paste(route, nameOptionFile, sep = "/")
  
  setwd("../../../..")
  
  #Add data to the file system
  scenarioData <- list(scenarioName, parameterSpace, setInstances, optionsRoute, scenarioType)
  setwd("../Proyecto_titulo/FileSystem")
  write.table(scenarioData, file = "Scenario.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
}

#add version
if(args$add_version){
  #Request data from the user
  versionNumber <- readline("Enter the version number of irace to add: " )
  versionDescription <- readline("Enter a description corresponding to the version of irace to enter: ")
  versionRoute <- readline("Enter the path where the irace version is located: ")
  nameVersion <- readline("Enter the name with the file type of the version to add:: ")
  
  #Add files to the file system
  #Create folder
  setwd("../Proyecto_titulo/FileSystem/Files/Version")
  dir.create(versionNumber)
  
  fileRoot <- getwd()
  route <- paste(fileRoot, versionNumber, sep = "/") 
  setwd(route)
  
  #add to version 
  file.copy(versionRoute, route)
  versionRoute <- paste(route, nameVersion, sep = "/")
  
  setwd("../../../..")
  
  #Add data to the file system
  versionData <- list(versionNumber, versionDescription, versionRoute)
  setwd("../Proyecto_titulo/FileSystem")
  write.table(versionData, file = "Version.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  setwd("..")
}

###############################################################################################################
#Argument to list
###############################################################################################################

#list scenario
if(args$list_scenario){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Scenario.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#list target
if(args$list_target){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Target.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#list parameters
if(args$list_parameters){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Parameters.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#list instances
if(args$list_instances){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Instances.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#list versions
if(args$list_versions){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Version.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

#list iteration
if(args$list_iteration){
  fileRoot <- getwd()
  subDir <- "/FileSystem/Iteration.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData)
}

###############################################################################################################
#Argument to view
###############################################################################################################
