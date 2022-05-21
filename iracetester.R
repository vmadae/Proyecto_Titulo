suppressWarnings(library(argparser, quietly=TRUE))
suppressWarnings(library(readr))
suppressWarnings(library(knitr))

###############################################################################################################
#FUNCTIONS
###############################################################################################################

#Function to add target
addTarget <- function(){
  #Request data from the user
  repeat{
    #The name of the target to be added is requested
    cat('Enter the name of the target algorithm to add: ')
    cat('\n')
    targetName <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again.')
    cat('\n')
  }
  
  #The description of the target to be added is requested
  cat('Enter a description corresponding to the target to enter: ')
  cat('\n')
  targetDescription <- readLines("stdin", n = 1)
  
  #The runner file of the target to be added is requested
  repeat{
    cat('Enter the path where the target runner file is hosted: ')
    cat('\n')
    routeTargetRunner <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(routeTargetRunner)){
      break
    }
    cat("The file you entered does not exist, please try again. ")
    cat('\n')
  }
  
  #The executable file of the target to be added is requested
  repeat{
    cat('Enter the path of the target executable: ')
    cat('\n')
    executablePathTarget <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(executablePathTarget)){
      break
    }
    cat('The file you entered does not exist, please try again. ')
    cat('\n')
  }
  
  #Add files to the file system
  #The route of the target with which you work is saved
  route <- ("./FileSystem/Files/Target")
  routeFile <- paste(route, targetName, sep = "/")
  
  #Create folder
  dir.create(routeFile, recursive = T)
  
  #The target runner is saved in the system
  finalRouteTargetRunner <- paste(routeFile, targetName, sep = "/")
  finalRouteTargetRunner <- paste(finalRouteTargetRunner, "runner", sep = "_")
  file.copy(routeTargetRunner, finalRouteTargetRunner)
  routeTargetRunner <- finalRouteTargetRunner
  
  #The executable is saved in the system
  finalRouteExecutableTarget <- paste(routeFile, targetName, sep = "/")
  finalRouteExecutableTarget <- paste(finalRouteExecutableTarget, "executable", sep = "_") ##VERIFICAR .C
  file.copy(routeTargetRunner, finalRouteExecutableTarget)
  executablePathTarget <- finalRouteExecutableTarget
  
  #Add data to the file system
  targetData <- list(targetName, 
                     targetDescription, 
                     routeTargetRunner, 
                     executablePathTarget)
  write.table(targetData, file = "./FileSystem/Target.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
}

#Function to add parameter
addParameter <- function(){
  #Request data from the user
  repeat{
    #The name of the parameter to be added is requested
    cat(('Enter the name of the parameter to add: '))
    cat('\n')
    parametersName <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Parameters", parametersName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again.')
    cat('\n')
  }
  
  #The description of the parameter to be added is requested
  cat(('Enter a description of the parameter you want to add: '))
  cat('\n')
  parametersDescription <- readLines("stdin", n = 1)
  
  #The target algorithm that will be used for the new parameter is requested
  repeat{
    cat(('Enter the target algorithm to use: '))
    cat('\n')
    targetAlgorithm <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Target", targetAlgorithm, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the target algorithm entered by the user is not found in the system, the user can add it or choose another
    repeat{
      cat('The entered target algorithm does not exist in the database, want to add it?')
      cat('\n')
      cat('Enter "Y" to add or "N" to try again.')
      cat('\n')
      opt <- scan(quiet = T,'stdin', character(), n=1)
      
      #Entering the "Y" option performs the add target function
      if(opt == "Y" | "y"){
        addTarget()
        break
      }
      #Entering option "N" again prompts you to enter a target
      if(opt == "N" | "n"){
        break
      }
      #If the user enters another undefined letter, it is indicated that the option entered is not valid and it is requested to enter the option again.
      if(opt != "Y" | opt != "N" | opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again.')
        cat('\n')
      }
      Sys.sleep(0.5)
    }
  }

  #The text file is requested that lists the parameters that irace must configure, which will be used for the new parameter.
  repeat{
    cat(('Ingrese la ruta del archivo donde se listan los parámetros que debe configurar irace (por favor ingrese .txt) : '))
    cat('\n')
    parameters <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(parameters)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  #The type of the parameter to add is requested
  cat('Enter the type of the parameter: ')
  cat('\n')
  typeParameters <- scan(quiet = T,'stdin', character(), n=1)
  
  #
  repeat{
    cat('Archivo de texto que lista combinaciones prohibidas de valores de parametros .txt (opcional): ')
    cat('\n')
    forbidden <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(forbidden)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  repeat{
    cat('Archivo de texto que provee configuraciones para iniciar la busqueda en irace (opcional) .txt: ')
    cat('\n')
    initial <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(initial)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Parameters")
  routeFile <- paste(route, parametersName, sep = "/")
  
  dir.create(routeFile, recursive = T)
  
  #add to parameters file
  finalRouteParameters <- paste(routeFile, parametersName, sep = "/")
  finalRouteParameters <- paste(finalRouteParameters, "parameters.txt", sep = "_")
  
  file.copy(parameters, finalRouteParameters)
  parameters <- finalRouteParameters
  
  #add to forbidden file
  finalRouteForbidden <- paste(routeFile, parametersName, sep = "/")
  finalRouteForbidden <- paste(finalRouteForbidden, "forbidden.txt", sep = "_")
  
  file.copy(forbidden, finalRouteForbidden)
  forbidden <- finalRouteForbidden
  
  #add to initial file
  finalRouteInitial <- paste(routeFile, parametersName, sep = "/")
  finalRouteInitial <- paste(finalRouteInitial, "initial.txt", sep = "_")
  
  file.copy(initial, finalRouteInitial)
  initial <- finalRouteInitial
  
  #Add data to the file system
  parameterData <- list(parametersName, 
                        parametersDescription, 
                        targetAlgorithm, 
                        parameters, 
                        typeParameters, 
                        forbidden, 
                        initial, 
                        routeFile)
  write.table(parameterData, file = "./FileSystem/Parameters.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
}

#Function to add instance
addInstance <- function(){
  #Request data from the user
  repeat{
    cat(('Enter the instance name to add: '))
    cat('\n')
    instanceName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again.')
    cat('\n')
  }
  
  cat(('Enter the instance description to add: '))
  cat('\n')
  instanceDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat(('Enter the instance training to add: '))
    cat('\n')
    instanceTraining <- scan(quiet = T,'stdin', character(), n=1)
    
    if(file.exists(instanceTraining)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  repeat{
    cat(('Enter the instance number of the training to add: '))
    cat('\n')
    instanceNumberTraining <- scan(quiet = T,'stdin', character(), n=1)
    
    if(file.exists(instanceNumberTraining)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  repeat{
    cat(('Enter the instance testing to add: '))
    cat('\n')
    instanceTesting <- scan(quiet = T,'stdin', character(), n=1)
    
    if(file.exists(instanceTesting)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  repeat{
    cat(('Enter the number of the testing to add: '))
    cat('\n')
    instanceNumberTesting <- scan(quiet = T,'stdin', character(), n=1)
    
    if(file.exists(instanceNumberTesting)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Instances")
  routeFile <- paste(route, instanceName, sep = "/")
  
  dir.create(routeFile, recursive = T) 
  
  #add to instance training 
  routeFile <- paste(routeFile, instanceName, sep = "/")
  instanceRouteTraining <- paste(routeFile, "training", sep = "_")
  dir.create(instanceRouteTraining, recursive = T)
  
  #add to set training
  finalRouteSetTraining <- paste(instanceRouteTraining, instanceName, sep = "/")
  finalRouteSetTraining <- paste(finalRouteSetTraining, "training.txt", sep = "_")
  
  file.copy(instanceTraining, finalRouteSetTraining)
  
  instanceTraining <- finalRouteSetTraining
  
  #add to set #training
  finalRouteNumTraining <- paste(instanceRouteTraining, instanceName, sep = "/")
  finalRouteNumTraining <- paste(finalRouteNumTraining, "numtraining.txt", sep = "_")
  
  file.copy(instanceNumberTraining, finalRouteNumTraining)
  
  instanceNumberTraining <- finalRouteNumTraining
  
  #add to instance testing
  instanceRouteTesting <- paste(routeFile, "testing", sep = "_")
  
  dir.create(instanceRouteTesting, recursive = T)
  
  #add to set testing
  finalRouteSetTesting <- paste(instanceRouteTesting, instanceName, sep = "/")
  finalRouteSetTesting <- paste(finalRouteSetTesting, "testing.txt", sep = "_")
  
  file.copy(instanceTesting, finalRouteSetTesting)
  
  instanceTesting <- finalRouteSetTesting
  
  #add to set #testing
  finalRouteNumTesting <- paste(instanceRouteTesting, instanceName, sep = "/")
  finalRouteNumTesting <- paste(finalRouteNumTesting, "numtesting.txt", sep = "_")
  
  file.copy(instanceNumberTesting, finalRouteNumTesting)
  
  instanceNumberTesting <- finalRouteNumTesting
  
  #Add data to the file system
  instanceData <- list(instanceName, 
                       instanceDescription, 
                       instanceTraining, 
                       instanceNumberTraining, 
                       instanceTesting, 
                       instanceNumberTesting, 
                       instanceRouteTraining, 
                       instanceRouteTesting)
  write.table(instanceData, file = "./FileSystem/Instances.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
}

#Function to add scenario
addScenario <- function(){
  #Request data from the user
  repeat{
    cat('Enter the name of scenario to add: ')
    cat('\n')
    scenarioName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    cat('The file name you want to input already exists, please try again.')
    cat('\n')
  }
  
  cat('Enter the scenario description: ')
  cat('\n')
  scenarioDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the space parameter for the scenario: ')
    cat('\n')
    parameterSpace <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Parameters", parameterSpace, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    repeat{
      cat('The entered parameter does not exist in the database, want to add it?')
      cat('\n')
      cat('Enter "Y" to add or "N" to try again.')
      cat('\n')
      opt <- scan(quiet = T,'stdin', character(), n=1)
      
      if(opt == "Y" | opt == "y"){
        addParameter()
        break
      }
      if(opt == "N" | opt == "n"){
        break
      }
      if(opt != "Y" | opt != "N" | opt == "y"| opt == "n"){
        cat('The option entered is not valid, please try again.')
        cat('\n')
      }
      Sys.sleep(0.5)
    }
  }
  
  repeat{
    cat('Enter the set of instances for the scenario: ')
    cat('\n')
    setInstances<- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Instances", setInstances, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    repeat{
      cat('The entered instances does not exist in the database, want to add it?')
      cat('\n')
      cat('Enter "Y" to add or "N" to try again.')
      cat('\n')
      opt <- scan(quiet = T,'stdin', character(), n=1)
      
      if(opt == "Y" | opt == "y"){
        addTarget()
        break
      }
      if(opt == "N" | opt == "n"){
        break
      }
      if(opt != "Y" | opt != "N" | opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again.')
        cat('\n')
      }
      Sys.sleep(0.5)
    }
  }
  
  repeat{
    cat('Enter the option route for the scenario (.txt ): ')
    cat('\n')
    optionsRoute <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(optionsRoute)){
      break
    }
    cat('The entered option route does not exist in the file system, please try again.')
    cat('\n')
  }
  
  cat('Enter the type of the scenario: ')
  cat('\n')
  scenarioType <- readLines("stdin", n = 1)
  
  #Add files to the file system
  #Create folder
  route <- './FileSystem/Files/Scenario'
  routeFile <- paste(route, scenarioName, sep = "/")
  
  dir.create(routeFile,recursive = T)
  
  #add to option route 
  finalOptionRoute <- paste(routeFile,scenarioName,sep = "/")
  finalOptionRoute <- paste(finalOptionRoute,'options.txt',sep = "_")
  
  file.copy(optionsRoute, finalOptionRoute)
  
  optionsRoute <- finalOptionRoute
  
  #Add data to the file system
  scenarioData <- list(scenarioName,
                       scenarioDescription,
                       parameterSpace, 
                       setInstances, 
                       optionsRoute, 
                       scenarioType)
  write.table(scenarioData, file = "./FileSystem/Scenario.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
}

###############################################################################################################
#ADD COMMAND LINE ARGUMENTS
###############################################################################################################

# Create a parser
p <- arg_parser("Help")

#Arguments to list

p <- add_argument(p, short = "-ls", "--list_scenario", help="List all scenarios in the system", flag=TRUE)
p <- add_argument(p, short = "-lt", "--list_target", help="List all target algorithms", type="string", flag=TRUE)
p <- add_argument(p, short = "-lp", "--list_parameters", help="List all parameter sets", type="string", flag=TRUE)
p <- add_argument(p, short = "-li", "--list_instances", help="List all instance sets", type="string", flag=TRUE)
p <- add_argument(p, short = "-lv", "--list_versions", help="List all versions of irace", type="string", flag=TRUE)
p <- add_argument(p, short = "-le", "--list_experiment", help="List all registered experiments", type="string", flag=TRUE)

#Arguments to show
p <- add_argument(p, short = "-ss", "--show_scenario", help="Show details of a scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-st", "--show_target", help="Show the detail of a target algorithm", type="string", flag=TRUE)
p <- add_argument(p, short = "-sp", "--show_parameter", help="Show the detail of a set of parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-si", "--show_instance", help="Show the detail of a set of instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-sv", "--show_version", help="Show version details", type="string", flag=TRUE)
p <- add_argument(p, short = "-se", "--show_experiment", help="Show experiment details", type="string", flag=TRUE)

#Argument to see results
p <- add_argument(p, short = "-r", "--results", help="Show results (experiments) of a test", type="string", flag=TRUE)

#Arguments to test
p <- add_argument(p, short = "-ab", "--add_test", help="Create a test, add: 1.Name 2.Description 3.Irace version 4.Scenarios and their repetitions", type="string", flag=TRUE)
p <- add_argument(p, short = "-xb", "--execute_test", help="Run a test", type="string", flag=TRUE)

#Arguments to add
p <- add_argument(p, short = "-at", "--add_target", help="Add target", type="string", flag=TRUE)
p <- add_argument(p, short = "-ap", "--add_parameter", help="Add parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-ai", "--add_instances", help="Add instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-as", "--add_scenario", help="Add scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-av", "--add_version", help="Add new version", type="string", flag=TRUE)
p <- add_argument(p, short = "-ae", "--add_experiment", help= "Add experiment", type="string", flag=TRUE)

#Argument to modificate
p <- add_argument(p, short = "-mt", "--modify_target", help="Modify target", type="string", flag=TRUE)
p <- add_argument(p, short = "-mp", "--modify_parameter", help="Modify parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-mi", "--modify_instance", help="Modify instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-ms", "--modify_scenario", help="Modify scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-mv", "--modify_version", help="Modify version", type="string", flag=TRUE)

#Arguments to create website
p <- add_argument(p, "--web", help="Generate website in shiny", type="string", flag=TRUE)

# Parse the command line arguments
args <- parse_args(p)

###############################################################################################################
#FUNCTIONALITIES OF THE ARGUMENT
###############################################################################################################
#ARGUMENTS TO ADD
###############################################################################################################

#add target
if(args$add_target){
  addTarget()
  cat('The target has been entered successfully.')
  cat('\n')
}

#add parameters
if(args$add_parameter){
  addParameter()
  cat('The parameter has been added successfully.')
  cat('\n')
}

#add instances
if(args$add_instances){
  addInstance()
  cat('The instance has been entered successfully.')
  cat('\n')
}

#add scenario
if(args$add_scenario){
  addScenario()
  cat('The stage was added successfully.')
  cat('\n')
}

#add version
if(args$add_version){
  #Request data from the user
  repeat{
    cat('Enter the version number of irace to add: ')
    cat('\n')
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    cat('The version you want to enter already exists, please try again.')
    cat('\n')
  }
  
  cat('Enter a description corresponding to the version of irace to enter: ')
  cat('\n')
  versionDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the path where the irace version is located: ')
    cat('\n')
    versionRoute <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(versionRoute)){
      break
    }
    cat('The file you entered does not exist, please try again.')
    cat('\n')
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Version")
  routeFile <- paste(route, versionNumber, sep = "/")
  
  dir.create(routeFile, recursive = T)
  
  #add to version 
  finalRouteVersion <- paste(routeFile, versionNumber, sep = "/")
  
  file.copy(versionRoute, finalRouteVersion)
  
  versionRoute <- finalRouteVersion
  
  #Add data to the file system
  versionData <- list(versionNumber, 
                      versionDescription, 
                      versionRoute)
  write.table(versionData, file = "./FileSystem/Version.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  cat('The version has been added successfully.')
  cat('\n')
}

#add experiment
if(args$add_experiment){
  #Request data from the user
  repeat{
    cat('Enter the name of the experiment to add: ')
    cat('\n')
    experimentName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again.')
    cat('\n')
  }
  
  cat('Enter a description corresponding to the experiment to enter: ')
  cat('\n')
  experimentDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the scenario to use in the experiment: ')
    cat('\n')
    scenarioName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    #Check if the file exists
    if(file.exists(checkFile)){
      break
    }
    
    repeat{
      cat('The entered scenario does not exist in the database, want to add it?')
      cat('\n')
      cat('Enter "Y" to add or "N" to try again.')
      cat('\n')
      opt <- scan(quiet = T,'stdin', character(), n=1)
      
      if(opt == "Y" | opt == "y"){
        addTarget()
        break
      }
      if(opt == "N" | opt == "n"){
        break
      }
      if(opt != "Y" | opt != "N" | opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again.')
        cat('\n')
      }
      Sys.sleep(0.5)
    }
  }
  
  numRepeticiones <- 0
  
  resultados <- ""
  
  settings <- ""
  
  statusExperiment <- "created"
  
  experimentPath <- ""
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Experiment")
  routeFile <- paste(route, experimentName, sep = "/")
  
  dir.create(routeFile, recursive = T)
  
  #Add data to the file system
  targetData <- list(experimentName, 
                     experimentDescription, 
                     scenarioName, 
                     numRepeticiones, 
                     resultados, 
                     settings, 
                     statusExperiment, 
                     experimentPath)
  write.table(targetData, file = "./FileSystem/Experiment.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
}

###############################################################################################################
#ARGUMENTS TO LIST
###############################################################################################################

#list scenario
if(args$list_scenario){
  subDir <- "./FileSystem/Scenario.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[, c(1,6)])
}

#list target
if(args$list_target){
  subDir <- "./FileSystem/Target.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[1])
}

#list parameters
if(args$list_parameters){
  subDir <- "./FileSystem/Parameters.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[,c(1,3)])
}

#list instances
if(args$list_instances){
  subDir <- ("./FileSystem/Instances.txt")
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[1])
}

#list versions
if(args$list_versions){
  subDir <- "./FileSystem/Version.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[1])
}

#list experiment
if(args$list_experiment){
  subDir <- "./FileSystem/Experiment.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  kable(fileData[1])
}

###############################################################################################################
#ARGUMENTS TO SHOW
###############################################################################################################

#show scenario
if(args$show_scenario){
  repeat{
    cat('Enter the scenario to display: ')
    cat('\n')
    scenarioName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered scenario does not exist, please try another.')
    cat('\n')
  }
  
  subDir <- "./FileSystem/Scenario.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == scenarioName)
  
  cat('\n')
  cat(crayon::bold('Name:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Parameter space:'), x[,3])
  cat('\n')
  cat(crayon::bold('Set of instances:'), x[,4])
  cat('\n')
  cat(crayon::bold('Options route:'), x[,5])
  cat('\n')
  cat(crayon::bold('Type:'), x[,6])
  cat('\n')
}

#show target
if(args$show_target){
  repeat{
    cat('Enter the target algorithm to display: ')
    cat('\n')
    targetName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered target does not exist, please try another.')
    cat('\n')
  }
  
  subDir <- "./FileSystem/Target.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == targetName)
  
  cat('\n')
  cat(crayon::bold('Name:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Target rounner route:'), x[,3])
  cat('\n')
  cat(crayon::bold('Executable path:'), x[,4])
  cat('\n')
}

#show parameter
if(args$show_parameter){
  repeat{
    cat('Enter the parameter to display: ')
    parameterName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered parameter does not exist, please try another.')
    cat('\n')
  }
  
  subDir <- "./FileSystem/Parameters.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == parameterName)
  
  cat('\n')
  cat(crayon::bold('Name:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Target algorithm:'), x[,3])
  cat('\n')
  cat(crayon::bold('#Parameters:'), x[,4])
  cat('\n')
  cat(crayon::bold('Type:'), x[,5])
  cat('\n')
  cat(crayon::bold('Forbidden:'), x[,6])
  cat('\n')
  cat(crayon::bold('Initial:'), x[,7])
  cat('\n')
  cat(crayon::bold('File-path:'), x[,8])
  cat('\n')
}

#show instance
if(args$show_instance){
  repeat{
    cat('Enter the instance to display: ')
    cat('\n')
    instanceName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered instance does not exist, please try another.')
    cat('\n')
  }
  
  subDir <- "./FileSystem/Instances.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == instanceName)
  
  cat('\n')
  cat(crayon::bold('Name:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Training:'), x[,3])
  cat('\n')
  cat(crayon::bold('#training:'), x[,4])
  cat('\n')
  cat(crayon::bold('Testing:'), x[,5])
  cat('\n')
  cat(crayon::bold('#testing:'), x[,6])
  cat('\n')
  cat(crayon::bold('RouteTraining:'), x[,7])
  cat('\n')
  cat(crayon::bold('RouteTesting:'), x[,8])
  cat('\n')
}

#show version
if(args$show_version){
  repeat{
    cat('Enter the version to display: ')
    cat('\n')
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered version does not exist, please try another.')
    cat('\n')
  }

  subDir <- "./FileSystem/Version.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Version_Number == versionNumber)
  
  cat('\n')
  cat(crayon::bold('Version Number:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Route:'), x[,3])
  cat('\n')
}

#show experiment
if(args$show_experiment){
  repeat{
    cat('Enter the experiment to display: ')
    cat('\n')
    experimentName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered experiment does not exist, please try another.')
    cat('\n')
  }
  
  subDir <- "./FileSystem/Experiment.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Test == experimentName)
  
  cat('\n')
  cat(crayon::bold('Name:'), x[,1])
  cat('\n')
  cat(crayon::bold('Description:'), x[,2])
  cat('\n')
  cat(crayon::bold('Scenario:'), x[,3])
  cat('\n')
  cat(crayon::bold('#Repetitions:'), x[,4])
  cat('\n')
  cat(crayon::bold('Results:'), x[,5])
  cat('\n')
  cat(crayon::bold('Settings:'), x[,6])
  cat('\n')
  cat(crayon::bold('Status:'), x[,7])
  cat('\n')
  cat(crayon::bold('Experiment-path:'), x[,8])
  cat('\n')
}

###############################################################################################################
#ARGUMENTS TO MODIFY
###############################################################################################################

#modify scenario
if(args$modify_scenario){
  repeat{
    cat('Enter the scenario to modify: ')
    cat('\n')
    scenarioName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered scenario does not exist, please try another.')
    cat('\n')
  }
  
}

#modify target
if(args$modify_target){
  repeat{
    cat('Enter the target algorithm to modify: ')
    cat('\n')
    targetName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    #check if the entered target exists
    if(file.exists(checkFile)){
      break
    }
    cat('The entered target does not exist, please try another.')
    cat('\n')
  }
  
  #Select option to modify
  repeat{
    cat('Enter the option corresponding to the element you want to modify.')
    cat('\n')
    cat('1. Name')
    cat('\n')
    cat('2. Description')
    cat('\n')
    cat('3. Target runner')
    cat('\n')
    cat('4. Executable')
    cat('\n')
    
    opt <- scan(quiet = T,'stdin', integer(), n=1)
    
    if(opt == 1){
      cat('Enter the new name for the target: ')
      cat('\n')
      newNameTarget <- scan(quiet = T,'stdin', character(), n=1)
      
      subDir <- "./FileSystem/Target.txt"
      fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
      
      fileData$Name[fileData$Name == targetName] <- newNameTarget
      
      file.remove("./FileSystem/Target.txt")
      
      fromPath <- paste("./FileSystem/Files/Target", targetName, sep = "/")
      toPath <- paste("./FileSystem/Files/Target", newNameTarget, sep = "/")
      
      file.rename(fromPath, toPath)
      
      write.table(fileData, file = "./FileSystem/Target.txt", sep = "," ,row.names = FALSE, col.names = TRUE, append = TRUE)
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Target", 
                         Sys.Date(), 
                         "Name", 
                         targetName,
                         newNameTarget
                         )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
      
      
      break
    }
    if(opt == 2){
      cat('Se ingreso la opcion 2, se modifica la descripcion')
      cat('\n')
      break
    }
    if(opt == 3){
      cat('Se ingreso la opcion 3, se modifica el target runner')
      cat('\n')
      break
    }
    if(opt == 4){
      cat('Se ingreso la opcion 4, se medifica el ejecutable')
      cat('\n')
      break
    }
    if(opt != 1 | opt != 2 | opt != 3 | opt != 4 ){
      cat('The option entered is not correct, please try again.')
      cat('\n')
    }
    
    ############################################################
    #Ask if you want to continue modifying the entered algorithm
    cat('Do you want to modify another element of the target?')
    cat('\n')
    cat('Enter "Y" to modify another item or "N" to finish.')
    cat('\n')
    
    opt1 <- scan(quiet = T,'stdin', character(), n=1)
    
    if(opt1 == "N"){
      break
    }
    #############################################################
    Sys.sleep(0.5)
  }
  cat('The target has been successfully modified.')
}

#modify parameter
if(args$modify_parameter){
  repeat{
    cat('Enter the parameter to modify: ')
    cat('\n')
    parameterName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered parameter does not exist, please try another.')
    cat('\n')
  }
  
}

#modify instance
if(args$modify_instance){
  repeat{
    cat('Enter the instance to modify: ')
    cat('\n')
    instanceName <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered instance does not exist, please try another.')
    cat('\n')
  }
  
}

#modify version
if(args$modify_version){
  repeat{
    cat('Enter the version to modify: ')
    cat('\n')
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat('The entered version does not exist, please try another.')
    cat('\n')
  }
}
