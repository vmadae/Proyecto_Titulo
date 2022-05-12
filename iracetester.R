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
p <- add_argument(p, short = "--ae", "--add_experiment", help= "Add experiment", type="string", flag=TRUE)

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
  repeat{
    cat('Enter the name of the target algorithm to add: ')
    targetName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The file you want to input already exists, please try again.")
  }
  
  cat('Enter a description corresponding to the target to enter: ')
  targetDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the path where the target runner file is hosted: ')
    routeTargetRunner <- scan('stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(routeTargetRunner)){
     break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  repeat{
    cat('Enter the path of the target executable: ')
    executablePathTarget <- scan('stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(executablePathTarget)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Target")
  routeFile <- paste(route, targetName, sep = "/")
  
  dir.create(routeFile)
  
  #add to target runner
  finalRouteTargetRunner <- paste(routeFile, targetName, sep = "/")
  finalRouteTargetRunner <- paste(finalRouteTargetRunner, "runner", sep = "_")
  file.copy(routeTargetRunner, finalRouteTargetRunner)
  routeTargetRunner <- finalRouteTargetRunner
  
  #add to executable path target
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

  print("The target has been entered successfully.")
  }

#add parameters
if(args$add_parameters){
  #Request data from the user
  repeat{
    cat(('Enter the name of the parameter to add: '))
    parametersName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Parameters", parametersName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The file you want to input already exists, please try again.")
  }
  
  cat(('Enter a description of the parameter you want to add: '))
  parametersDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat(('Enter the target algorithm to use: '))
    targetAlgorithm <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Target", targetAlgorithm, sep = "/")
    
    #Check if the file exists
    if(file.exists(checkFile)){
      break
    }
    
    print("The entered target algorithm does not exist in the database, please enter or choose another.")
  }
  
  repeat{
    cat(('Archivo de texto que lista parametros que irace debe configurar .txt: '))
    parameters <- scan('stdin', character(), n=1)
    
    if(file.exists(parameters)){
      break
    }
    
    print("The file you entered does not exist, please try again.")
  }
  
  cat(('Enter the type of the parameter: '))
  typeParameters <- scan('stdin', character(), n=1)
  
  repeat{
    cat(('Archivo de texto que lista combinaciones prohibidas de valores de parametros .txt (opcional): '))
    forbidden <- scan('stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(forbidden)){
      break
    }
    
    print("The file you entered does not exist, please try again.")
  }
  
  repeat{
    cat(('Archivo de texto que provee configuraciones para iniciar la busqueda en irace (opcional) .txt: '))
    initial <- scan('stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(initial)){
      break
    }
    
    print("The file you entered does not exist, please try again.")
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Parameters")
  routeFile <- paste(route, parametersName, sep = "/")
  
  dir.create(routeFile)
  
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
  
  print("The parameter has been added successfully.")
}

#add instances
if(args$add_instances){
  #Request data from the user
  repeat{
    cat(('Enter the instance name to add: '))
    instanceName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The file you want to input already exists, please try again.")
  }
  
  cat(('Enter the instance description to add: '))
  instanceDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat(('Enter the instance training to add: '))
    instanceTraining <- scan('stdin', character(), n=1)
    
    if(file.exists(instanceTraining)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  repeat{
    cat(('Enter the instance number of the training to add: '))
    instanceNumberTraining <- scan('stdin', character(), n=1)
    
    if(file.exists(instanceNumberTraining)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  repeat{
    cat(('Enter the instance testing to add: '))
    instanceTesting <- scan('stdin', character(), n=1)
    
    if(file.exists(instanceTesting)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  repeat{
    cat(('Enter the number of the testing to add: '))
    instanceNumberTesting <- scan('stdin', character(), n=1)
    
    if(file.exists(instanceNumberTesting)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Instances")
  routeFile <- paste(route, instanceName, sep = "/")
  
  dir.create(routeFile) 
  
  #add to instance training 
  instanceRouteTraining <- paste(routeFile, "training", sep = "_")
  
  dir.create(instanceRouteTraining)
  
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
  
  dir.create(instanceRouteTesting)
  
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
  
  print("The instance has been entered successfully.")
}

#add scenario
if(args$add_scenario){
  #Request data from the user
  repeat{
    cat('Enter the name of scenario to add: ')
    scenarioName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The file name you want to input already exists, please try again.")
  }
  
  cat('Enter the scenario description: ')
  optionsRoute <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the space parameter for the scenario: ')
    parameterSpace <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Parameters", parameterSpace, sep = "/")
    
    #Check if the file exists
    if(file.exists(checkFile)){
      print(file.exists(checkFile))
      break
    }
    
    print("The entered space parameter does not exist in the database, please enter or choose another.")
  }
  
  repeat{
    cat('Enter the set of instances for the scenario: ')
    setInstances<- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Instances", setInstances, sep = "/")
    
    #Check if the file exists
    if(file.exists(checkFile)){
      print(file.exists(checkFile))
      break
    }
    
    print("The entered set of instances does not exist in the database, please enter or choose another.")
  }
  
  repeat{
    cat('Enter the option route for the scenario (.txt ): ')
    optionsRoute <- scan('stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(optionsRoute)){
      print(file.exists(optionsRoute))
      break
    }
    
    print("The entered option route does not exist in the file system, please try again.")
  }
  
  cat('Enter the type of the scenario: ')
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
  scenarioData <- list(scenarioName, parameterSpace, setInstances, optionsRoute, scenarioType)
  write.table(scenarioData, file = "./FileSystem/Scenario.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  print("The stage was added successfully.")
}

#add version
if(args$add_version){
  #Request data from the user
  repeat{
    cat('Enter the version number of irace to add: ')
    versionNumber <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The version you want to enter already exists, please try again")
  }
  
  cat('Enter a description corresponding to the version of irace to enter: ')
  versionDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the path where the irace version is located: ')
    versionRoute <- scan('stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(versionRoute)){
      break
    }
    
    print("The file you entered does not exist, please try again")
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Version")
  routeFile <- paste(route, versionNumber, sep = "/")
  
  dir.create(routeFile)
  
  #add to version 
  finalRouteVersion <- paste(routeFile, versionNumber, sep = "/")
  
  file.copy(versionRoute, finalRouteVersion)
  
  versionRoute <- finalRouteVersion
  
  #Add data to the file system
  versionData <- list(versionNumber, 
                      versionDescription, 
                      versionRoute)
  write.table(versionData, file = "./FileSystem/Version.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  print("La versión se ha introducido correctamente.")
}

#add experiment
if(args$add_experiment){
  #Request data from the user
  repeat{
    cat('Enter the name of the experiment to add: ')
    experimentName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
    
    #Check if the file exists
    if(!file.exists(checkFile)){
      break
    }
    
    print("The file you want to input already exists, please try again.")
  }
  
  cat('Enter a description corresponding to the experiment to enter: ')
  experimentDescription <- readLines("stdin", n = 1)
  
  repeat{
    cat('Enter the scenario to use in the experiment: ')
    scenarioName <- scan('stdin', character(), n=1)
    
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    #Check if the file exists
    if(file.exists(checkFile)){
      break
    }
    
    print("The scenario entered does not exist, try entering another")
  }
  
  numRepeticiones <- 0
  
  resultados <- ""
  
  settings <- ""
  
  statusExperiment <- "created"
  
  experimentPath <- ""
  
  #Add data to the file system
  targetData <- list(experimentName, 
                     experimentDescription, 
                     scenarioName, 
                     numRepeticiones, 
                     resultados, 
                     settings, 
                     statusExperiment, 
                     experimentPath)
  write.table(targetData, file = "./FileSystem/Scenario.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
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
  subDir <- "./FileSystem/Target.txt"
  route <- paste(fileRoot, subDir, sep = "")
  fileData <- read.delim(file = route, header = TRUE, sep = ",", dec = ".")
  print(fileData[, 1:2])
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
#Argument to show (CAMBIAR ARGUMENTOS DE VIEW A SHOW)
###############################################################################################################
