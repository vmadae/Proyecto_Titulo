###############################################################################################################
#VALIDATE PACKAGE
###############################################################################################################
#A list is generated with the packages used by the system
packages <- c("argparser","readr","knitr","shiny","shinythemes","gt","irace","crayon")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages], repos = "http://cran.us.r-project.org")
}

#package warnings are suppressed for a cleaner terminal
suppressWarnings(library(argparser, quietly=TRUE))
suppressWarnings(library(readr))
suppressWarnings(library(knitr))
suppressWarnings(library(shiny))
suppressWarnings(library(shinythemes))
suppressWarnings(library(gt))
suppressWarnings(library(irace))
suppressWarnings(library(crayon))

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

###############################################################################################################
#FUNCTIONS
###############################################################################################################

#Function to add target
addTarget <- function(flagTarget){
  #Request data from the user
  repeat{
    #The name of the target to be added is requested
    cat(blue$bold('Enter the name of the target algorithm to add: \n'))
    targetName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #check if the file exists
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat(magenta('The file you want to input already exists, please try again. \n'))
  }
  
  #The description of the target to be added is requested
  cat(blue$bold('Enter a description corresponding to the target to enter: \n'))
  targetDescription <- tolower(readLines("stdin", n = 1))
  
  #The runner file of the target to be added is requested
  repeat{
    cat(blue$bold('Enter the path where the target runner file is hosted: \n'))
    routeTargetRunner <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(routeTargetRunner)){
      break
    }
    cat(magenta("The file you entered does not exist, please try again. \n"))
  }
  
  #The executable file of the target to be added is requested
  repeat{
    cat(blue$bold('Enter the path of the target executable: \n'))
    executablePathTarget <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(executablePathTarget)){
      break
    }
    cat(magenta('The file you entered does not exist, please try again. \n'))
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
  finalRouteExecutableTarget <- paste(finalRouteExecutableTarget, "executable.zip", sep = "_")
  file.copy(executablePathTarget, finalRouteExecutableTarget)
  executablePathTarget <- finalRouteExecutableTarget
  
  #Add data to the file system
  targetData <- list(targetName, 
                     targetDescription, 
                     routeTargetRunner, 
                     executablePathTarget,
                     Sys.Date(),
                     "-")
  write.table(targetData, file = "./FileSystem/Target.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #Add to file for the update to github
  dataForGitHub <- list("Target",
                        targetName,
                        checkFile)
  write.table(dataForGitHub, file = "./FileSystem/SubmitGitHub.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #The user is informed that the target was added successfully.
  cat("\n")
  cat(blue$bold('The target has been entered successfully. \n'))
  cat("\n")
  
  #If it was requested to create the target from another configuration element, the name is returned.
  if(flagTarget == TRUE){
    return(targetName)
  }
}

#Function to add parameter
addParameter <- function(flagParameter){
  #Request data from the user
  repeat{
    #The name of the parameter to be added is requested
    cat(blue$bold('Enter the name of the parameter to add: \n'))
    parametersName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Parameters", parametersName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat(magenta('The file you want to input already exists, please try again. \n'))
  }
  
  #The description of the parameter to be added is requested
  cat(blue$bold('Enter a description of the parameter you want to add: \n'))
  parametersDescription <- tolower(readLines("stdin", n = 1))
  
  #The target algorithm that will be used for the new parameter is requested
  repeat{
    cat(blue$bold('Enter the target algorithm to use: \n'))
    targetAlgorithm <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Target", targetAlgorithm, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the target algorithm entered by the user is not found in the system, the user can add it or choose another
    repeat{
      cat(blue$bold('The entered target algorithm does not exist in the database, want to add it? \n'))
      cat(blue$bold('Enter "Y" to add or "N" to try again. \n'))
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      #Entering the "Y" option performs the add target function
      if(opt == "y"){
        flagTarget = TRUE
        targetAlgorithm <- addTarget(flagTarget)
        break
      }
      #Entering option "N" again prompts you to enter a target
      if(opt == "n"){
        break
      }
      #If the user enters another undefined letter, it is indicated that the option entered is not valid and it is requested to enter the option again.
      if(opt != "y" | opt != "n"){
        cat(magenta('The option entered is not valid, please try again. \n'))
      }
      Sys.sleep(0.5)
    }
    
    if(opt=="y"){
      break
    }
  }

  #The text file is requested that lists the parameters that irace must configure, which will be used for the new parameter.
  repeat{
    cat(blue$bold('Enter the path of the file (.txt) that contains the list of parameters to be configured by irace: \n'))
    parameters <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(parameters)){
      break
    }
    cat(magenta('The file you entered does not exist, please try again. \n'))
  }
  
  #The type of the parameter to add is requested
  cat(blue$bold('Enter the type of the parameter: \n'))
  typeParameters <- scan(quiet = T,'stdin', character(), n=1)
  
  #The user is asked if he wants to add the file that lists prohibited combinations of parameters.
  repeat{
    cat(blue$bold('Do you want to enter the list of prohibited combinations of parameter values? \n'))
    cat(blue$bold('Enter "Y" if you want to add and "N" if you dont want to add it. \n'))
    optForbidden <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #If the user wants to add it, they are prompted to enter the file path.
    if(optForbidden == 'y'){
      repeat{
        cat(blue$bold('Enter the path of the file (.txt) containing the list of prohibited combinations of parameter values: \n'))
        forbidden <- scan(quiet = T,'stdin', character(), n=1)
        
        #Check if the file exists
        if(file.exists(forbidden)){
          break
        }
        cat(magenta('The file you entered does not exist, please try again. \n'))
      }
      break
    }
    
    #If the user does not want to add it, it is given the value -1 by default.
    if(optForbidden == 'n'){
      forbidden <- "-1"
      break
    }
    
    #In case the user makes a mistake when entering the option, he must enter it again.
    if(optForbidden != 'y' | optForbidden != 'n'){
      cat(magenta("The option entered is incorrect, please try again. \n"))
    }
  }
  
  #The user is asked if he wants to add the file that provides settings to start the search in irace.
  repeat{
    cat(blue$bold('Do you want to add the file that provides the configuration to start the search in irace? \n'))
    cat(blue$bold('Enter "Y" if you want to add and "N" if you dont want to add it. \n'))
    optInitial <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #If the user wants to add it, they are prompted to enter the file path.
    if(optInitial == 'y'){
      repeat{
        cat(blue$bold('Enter the path of the file that provides the configuration to start the search in irace. \n'))
        initial <- scan(quiet = T,'stdin', character(), n=1)
        
        #Check if the file exists
        if(file.exists(initial)){
          break
        }
        cat(magenta('The file you entered does not exist, please try again. \n'))
      }
      break
    }
    
    #If the user does not want to add it, it is given the value -1 by default.
    if(optInitial == 'n'){
      initial <- "-1"
      break
    }
    
    #In case the user makes a mistake when entering the option, he must enter it again.
    if(optInitial != 'y' | optInitial != 'n'){
      cat(magenta("The option entered is incorrect, please try again. \n"))
    }
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
  if(forbidden != "-1"){
    finalRouteForbidden <- paste(routeFile, parametersName, sep = "/")
    finalRouteForbidden <- paste(finalRouteForbidden, "forbidden.txt", sep = "_")
    
    file.copy(forbidden, finalRouteForbidden)
    forbidden <- finalRouteForbidden
  }
  
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
                        routeFile,
                        Sys.Date(),
                        "-")
  write.table(parameterData, file = "./FileSystem/Parameters.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  #Add to file for the update to github
  dataForGitHub <- list("Parameter",
                        parametersName,
                        checkFile)
  write.table(dataForGitHub, file = "./FileSystem/SubmitGitHub.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  
  cat("\n")
  cat(blue$bold('The parameter has been added successfully. \n'))
  cat("\n")
  
  if(flagParameter == TRUE){
    return(parametersName)
  }
}

#Function to add instance
addInstance <- function(flagInstance){
  #Request data from the user
  repeat{
    cat('Enter the instance name to add: \n')
    instanceName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again. \n')
  }
  
  #The description of the add instance is requested.
  cat(('Enter the instance description to add: \n'))
  instanceDescription <- tolower(readLines("stdin", n = 1))
  
  #The user is prompted to enter the path of the file that lists the instances.
  repeat{
    cat(('Enter the instance training to add: \n'))
    instanceTraining <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- instanceTraining 
    
    if(file.exists(checkFile)){
      break
    }
    cat('The file you want to input already no exists, please try again. \n')
  }
  
  #Enter the number of instances the training set contains.
  cat(('Enter the instance number of the training to add: \n'))
  instanceNumberTraining <- scan(quiet = T,'stdin', integer(), n=1)
  
  #The user is asked if they want to add the instance files.
  repeat{
    cat('Do you want to add the file that instances? \n')
    cat('Enter "Y" if you want to add and "N" if you dont want to add it. \n')
    optRouteTraining <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #If the user wants to add it, they are prompted to enter the file path.
    if(optRouteTraining == 'y'){
      repeat{
        cat('Enter the path where the file containing the instances for the training set is located. \n')
        instanceRouteTraining <- scan(quiet = T,'stdin', character(), n=1)
        
        #Check if the file exists
        if(file.exists(instanceRouteTraining)){
          break
        }
        cat('The file you entered does not exist, please try again. \n')
      }
      break
    }
    
    #If the user does not want to add it, it is given the value -1 by default.
    if(optRouteTraining == 'n'){
      instanceRouteTraining <- "-1"
      break
    }
    
    #In case the user makes a mistake when entering the option, he must enter it again.
    if(optRouteTraining != 'y' | optRouteTraining != 'n'){
      cat("The option entered is incorrect, please try again. \n")
    }
  }
  
  #The user is prompted to enter the file that lists instances for the test set.
  repeat{
    cat(('Enter the path to the text file that lists the instances for the test set.: \n'))
    instanceTesting <- scan(quiet = T,'stdin', character(), n=1)
    
    if(file.exists(instanceTesting)){
      break
    }
    cat('The file you entered does not exist, please try again. \n')
  }
  
  #the user is prompted to enter the number of instances for the test set.
  cat(('Enter the number of instances for the test set: \n'))
  instanceNumberTesting <- scan(quiet = T,'stdin', integer(), n=1)
  
  #The user is asked if they want to add the instance files.
  repeat{
    cat('Do you want to add the instance files? \n')
    cat('Enter "Y" if you want to add and "N" if you dont want to add it. \n')
    optRouteTesting <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #If the user wants to add it, they are prompted to enter the file path.
    if(optRouteTesting == 'y'){
      repeat{
        cat('Enter the path where the test set instance files are located. \n')
        instanceRouteTesting <- scan(quiet = T,'stdin', character(), n=1)
        
        #Check if the file exists
        if(file.exists(instanceRouteTesting)){
          break
        }
        cat('The file you entered does not exist, please try again. \n')
      }
      break
    }
    
    #If the user does not want to add it, it is given the value -1 by default.
    if(optRouteTesting == 'n'){
      instanceRouteTesting <- "-1"
      break
    }
    
    #In case the user makes a mistake when entering the option, he must enter it again.
    if(optRouteTesting != 'y' | optRouteTesting != 'n'){
      cat("The option entered is incorrect, please try again. \n")
    }
  }
  
  #Add files to the file system
  #Create folder
  route <- ("./FileSystem/Files/Instances")
  routeFile <- paste(route, instanceName, sep = "/")
  
  dir.create(routeFile, recursive = T) 
  
  #add instance training
  finalRouteInstanceTraining <- paste(routeFile,instanceName,sep = "/")
  finalRouteInstanceTraining <- paste(finalRouteInstanceTraining,"training",sep = "_")
  
  dir.create(finalRouteInstanceTraining,recursive = T)
  
  finalInstanceTraining <- paste(finalRouteInstanceTraining,instanceName,sep = "/")
  finalInstanceTraining <- paste(finalInstanceTraining,"list_instances",sep = "_")
  
  file.copy(instanceTraining ,finalInstanceTraining)
  
  instanceTraining <- finalInstanceTraining
  
  #add instance route training
  if(instanceRouteTraining != "-1"){
    finalinstanceRouteTraining <- paste(finalRouteInstanceTraining,instanceName,sep = "/")
    finalinstanceRouteTraining <- paste(finalinstanceRouteTraining,"file_instances",sep = "_")
    
    file.copy(instanceRouteTraining, finalinstanceRouteTraining)
    
    instanceRouteTraining <- finalinstanceRouteTraining
  }
  
  #add instance testing
  finalRouteInstanceTesting <- paste(routeFile,instanceName,sep = "/")
  finalRouteInstanceTesting <- paste(finalRouteInstanceTesting,"testing",sep = "_")
  
  dir.create(finalRouteInstanceTesting,recursive = T)
  
  finalInstanceTesting <- paste(finalRouteInstanceTesting,instanceName,sep = "/")
  finalInstanceTesting <- paste(finalInstanceTesting,"list_instances",sep = "_")
  
  file.copy(instanceTesting,finalInstanceTesting)
  
  instanceTesting <- finalInstanceTesting
  
  #add instance route testing
  if(instanceRouteTesting != "-1"){
    finalInstanceRouteTesting <- paste(finalRouteInstanceTesting,instanceName,sep = "/")
    finalInstanceRouteTesting <- paste(finalInstanceRouteTesting,"file_instances",sep = "_")
    
    file.copy(instanceRouteTesting,finalInstanceRouteTesting)
    
    instanceRouteTesting <- finalInstanceRouteTesting
  }
  
  #Add data to the file system
  instanceData <- list(instanceName, 
                       instanceDescription, 
                       instanceTraining, 
                       instanceNumberTraining, 
                       instanceTesting, 
                       instanceNumberTesting, 
                       instanceRouteTraining, 
                       instanceRouteTesting,
                       Sys.Date(),
                       "-")
  write.table(instanceData, file = "./FileSystem/Instances.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  if(flagInstance == TRUE){
    return(instanceName)
  }
}

#Function to add scenario
addScenario <- function(flagScenario){
  #Request data from the user
  repeat{
    cat('Enter the name of scenario to add: \n')
    scenarioName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The file name you want to input already exists, please try again. \n')
  }
  
  #The user is asked to enter the description of the scenario to add.
  cat('Enter the scenario description: \n')
  scenarioDescription <- tolower(readLines("stdin", n = 1))
  
  #The user is prompted to enter a parameter.
  repeat{
    cat('Enter the space parameter for the scenario: \n')
    parameterSpace <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Parameters", parameterSpace, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the entered parameter does not exist, the user is asked if he wants to add it or not.
    repeat{
      cat('The entered parameter does not exist in the database, want to add it? \n')
      cat('Enter "Y" to add or "N" to try again. \n')
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      #If you want to add, call the function.
      if(opt == "y"){
        flagParameter <- TRUE
        parameterSpace <- addParameter(flagParameter)
        break
      }
      #If you do not want to add it, you will be prompted to enter it again.
      if(opt == "n"){
        break
      }
      #If the option entered is incorrect, the user is informed.
      if(opt == "y"| opt == "n"){
        cat('The option entered is not valid, please try again. \n')
      }
      Sys.sleep(0.5)
    }
    
    #If the user added a parameter, the iteration is exited.
    if(opt == "y"){
      break
    }
  }
  
  #The user is prompted to enter the set of parameters to be used in the scenario.
  repeat{
    cat('Enter the set of instances for the scenario: \n')
    setInstances<- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Instances", setInstances, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the set of instances is not found the user can enter a new one.
    repeat{
      cat('The entered instances does not exist in the database, want to add it? \n')
      cat('Enter "Y" to add or "N" to try again. \n')
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      if(opt == "y"){
        flagInstance <- TRUE
        setInstances <- addInstance(flagInstance)
        break
      }
      if(opt == "n"){
        break
      }
      if(opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again. \n')
      }
      Sys.sleep(0.5)
    }
    
    if(opt == "y"){
      break
    }
  }
  
  #The user is prompted to enter options for irace.
  repeat{
    cat('Enter the option route for the scenario (.txt ): \n')
    optionsRoute <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    if(file.exists(optionsRoute)){
      break
    }
    cat('The entered option route does not exist in the file system, please try again. \n')
  }
  
  cat('Enter the type of the scenario: \n')
  scenarioType <- tolower(readLines("stdin", n = 1))
  
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
                       scenarioType,
                       Sys.Date(),
                       "-")
  write.table(scenarioData, file = "./FileSystem/Scenario.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  if(flagScenario == TRUE){
    return(scenarioName)
  }
}

#Function to add version
addVersion <- function(flagVersion){
  #Request data from the user
  repeat{
    cat('Enter the version number of irace to add: \n')
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The version you want to enter already exists, please try again. \n')
  }
  
  #The user is prompted to enter the description of the version to enter.
  cat('Enter a description corresponding to the version of irace to enter: \n')
  versionDescription <- tolower(readLines("stdin", n = 1))
  
  #The user is prompted to enter the path where the version is located.
  repeat{
    cat('Enter the path where the irace version is located: \n')
    versionRoute <- scan(quiet = T,'stdin', character(), n=1)
    
    #check if the file exists
    if(file.exists(versionRoute)){
      break
    }
    cat('The file you entered does not exist, please try again. \n')
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
                      versionRoute,
                      Sys.Date(),
                      "-")
  write.table(versionData, file = "./FileSystem/Version.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  if(flagVersion == TRUE){
    return(versionNumber)
  }
}

#Function to add experiment
addExperiment <- function(flagExperiment){
  #Request data from the user
  repeat{
    cat('Enter the name of the experiment to add: \n')
    experimentName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
    
    if(!file.exists(checkFile)){
      break
    }
    cat('The file you want to input already exists, please try again. \n')
  }
  
  #The user is prompted to enter the description pertaining to the experiment.
  cat('Enter a description corresponding to the experiment to enter: \n')
  experimentDescription <- tolower(readLines("stdin", n = 1))
  
  #The user is prompted to enter the experiment to use.
  repeat{
    cat('Enter the scenario to use in the experiment: \n')
    scenarioName <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the entered scenario is not found, the option is given to create a new one or enter another one.
    repeat{
      cat('The entered scenario does not exist in the database, want to add it? \n')
      cat('Enter "Y" to add or "N" to try again. \n')
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      if(opt == "y"){
        flagScenario <- TRUE
        scenarioName <- addScenario(flagScenario)
        break
      }
      if(opt == "n"){
        break
      }
      if(opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again. \n')
      }
      Sys.sleep(0.5)
    }
    
    #If the user entered a new scenario, the iteration is exited.
    if(opt == "y"){
      break
    }
  }
  
  #The user is prompted to enter the version of irace to use.
  repeat{
    cat('Enter the version of irace to use: \n')
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    #Check if the file exists
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #The user is prompted to enter the version of irace to use
    repeat{
      cat('The entered version is not in the database, do you want to add another? \n')
      cat('Enter "Y" to add or "N" to try again. \n')
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      if(opt == "y"){
        flagVersion <- TRUE
        versionNumber <- addVersion(flagVersion)
        break
      }
      if(opt == "n"){
        break
      }
      if(opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again. \n')
      }
      Sys.sleep(0.5)
    }
    
    #If the user entered a new version, the iteration is exited.
    if(opt == "y"){
      break
    }
  }
  
  #The user is prompted for the number of repetitions performed by the experiment.
  cat('Enter the number of repetitions to perform:: \n')
  numRepeticiones <- scan(quiet = T,'stdin', integer(), n=1)
  
  #The results will be entered after the experiment is performed.
  resultados <- ""
  
  #The best configuration found is obtained after the completion of the experiment.
  settings <- ""
  
  #The state of the configuration changes as the experiment progresses.
  statusExperiment <- "created"
  
  #The run path is saved after the run is ready.
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
                     versionNumber,
                     numRepeticiones, 
                     resultados, 
                     settings, 
                     statusExperiment, 
                     experimentPath,
                     Sys.Date(),
                     "-")
  write.table(targetData, file = "./FileSystem/Experiment.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
  
  if(flagExperiment == TRUE){
    return(experimentName)
  }
}

#Function run the experiment
runExperiment <- function(scenarioName, nameExperiment, nRepetitions, cores, 
                          scenarioFile, parameterFile, targetRunner, 
                          trainInstancesFile, testInstancesFile, 
                          configurationsFile, forbiddenFile){
  
  # create directory for repetitions
  path_to_test <- paste("./FileSystem/Files/Iterations", scenarioName, sep= "/")
  dir.create(file.path(path_to_test), showWarnings = FALSE)
  
  # create scenario object
  scenario_files <- list()
  scenario_files$parameterFile <- parameterFile
  scenario_files$forbiddenFile <- forbiddenFile
  scenario_files$configurationsFile <- configurationsFile
  scenario_files$targetRunner <- targetRunner
  scenario_files$trainInstancesFile <- trainInstancesFile
  print(scenario_files$trainInstancesFile)
  scenario_files$testInstancesFile <- testInstancesFile
  
  scenario_files$trainInstancesDir <- ""
  scenario_files$testInstancesDir <- ""
  
  scenario <- irace::defaultScenario(scenario_files)
  print(scenario$trainInstancesFile)
  scenario <- irace::readScenario(filename=scenarioFile, scenario=scenario)
  print(scenario$trainInstancesFile)
  
  # set parallel cores
  if (!is.null(cores)) {
    cat("here core", cores)
    scenario$parallel <- cores
  }
  
  # read parameters information
  parameters <- irace::readParameters(file=parameterFile)
  
  seed = 1234567
  for (i in 1:nRepetitions) {
    # generate repetition folder name test-num
    num <- i
    if (i<9) num <- paste0("0",i)
    path_exe <- paste0(path_to_test, "/test-",num)
    # create directory to run irace
    dir.create(file.path(path_exe), showWarnings = FALSE)
    # add execution path to scenario
    scenario$execDir <- path_exe
    # add execution seed
    scenario$seed <- seed
    # run irace
    cat("# Running repetition 1\n")
    irace::irace(scenario=scenario, parameters=parameters)
    # next seed
    seed = seed + 1
  }
}


#Function ...
is_repetition_completed <- function(scenarioName, nrep) {
  num <- nrep
  if (num < 9) num <- paste0("0", nrep)
  logFile <- paste0(tests_folder, "/", scenarioName, "/test-", num, "/irace.Rdata")
  load(logFile)
  return (iraceResults$state$completed$flag)
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

#Argument to display by entering the name
p <- add_argument(p, short = "-ss", "--show_scenario", help="Show details of a scenario", type="string", default ="NA")
p <- add_argument(p, short = "-st", "--show_target", help="Show the detail of a target algorithm", type="string", default ="NA")
p <- add_argument(p, short = "-sp", "--show_parameter", help="Show the detail of a set of parameters", type="string", default ="NA")
p <- add_argument(p, short = "-si", "--show_instance", help="Show the detail of a set of instances", type="string", default ="NA")
p <- add_argument(p, short = "-sv", "--show_version", help="Show version details", type="string", default ="NA")
p <- add_argument(p, short = "-se", "--show_experiment", help="Show experiment details", type="string", default ="NA")

#Arguments to show 
p <- add_argument(p, short = "-sws", "--showScenario", help="Show details of a scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-swt", "--showTarget", help="Show the detail of a target algorithm", type="string", flag=TRUE)
p <- add_argument(p, short = "-swp", "--showParameter", help="Show the detail of a set of parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-swi", "--showInstance", help="Show the detail of a set of instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-swv", "--showVersion", help="Show version details", type="string", flag=TRUE)
p <- add_argument(p, short = "-swe", "--showExperiment", help="Show experiment details", type="string", flag=TRUE)

#Argument to see results #FALTA
p <- add_argument(p, short = "-r", "--results", help="Show results (experiments) of a test", type="string", flag=TRUE)

#Arguments to test #FALTA
p <- add_argument(p, short = "-ee", "--execute_experiment", help="Run a test", type="string", flag=TRUE)

#Arguments to add
p <- add_argument(p, short = "-at", "--add_target", help="Add target", type="string", flag=TRUE)
p <- add_argument(p, short = "-ap", "--add_parameter", help="Add parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-ai", "--add_instances", help="Add instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-as", "--add_scenario", help="Add scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-av", "--add_version", help="Add new version", type="string", flag=TRUE)
p <- add_argument(p, short = "-ae", "--add_experiment", help= "Add experiment", type="string", flag=TRUE)

#Argument to modificate #FALTA
p <- add_argument(p, short = "-mt", "--modify_target", help="Modify target", type="string", flag=TRUE)
p <- add_argument(p, short = "-mp", "--modify_parameter", help="Modify parameters", type="string", flag=TRUE)
p <- add_argument(p, short = "-mi", "--modify_instance", help="Modify instances", type="string", flag=TRUE)
p <- add_argument(p, short = "-ms", "--modify_scenario", help="Modify scenario", type="string", flag=TRUE)
p <- add_argument(p, short = "-mv", "--modify_version", help="Modify version", type="string", flag=TRUE)

#Arguments to create website #FALTA TERMINAR
p <- add_argument(p, "--web", help="Generate website in shiny", type="string", flag=TRUE)

#Argument for uploading to github #FALTA

# Parse the command line arguments
args <- parse_args(p)

###############################################################################################################
#FUNCTIONALITIES OF THE ARGUMENT
###############################################################################################################
#ARGUMENTS TO ADD
###############################################################################################################

#add target
if(args$add_target){
  flagTarget = FALSE
  addTarget(flagTarget)
}

#add parameters
if(args$add_parameter){
  flagParameter = FALSE
  addParameter(flagParameter)
}

#add instances
if(args$add_instances){
  flagInstance = FALSE
  addInstance(flagInstance)
  cat('The instance has been entered successfully. \n')
}

#add scenario
if(args$add_scenario){
  flagScenario <- FALSE
  addScenario(flagScenario)
  cat('The stage was added successfully. \n')
}

#add version
if(args$add_version){
  flagVersion <- FALSE
  addVersion(flagVersion)
  cat('The version has been added successfully. \n')
}

#add experiment
if(args$add_experiment){
  flagExperiment <- FALSE
  addExperiment(flagExperiment)
}

###############################################################################################################
#ARGUMENTS TO LIST
###############################################################################################################

#list scenario
if(args$list_scenario){
  #The item's .txt file is read
  subDir <- "./FileSystem/Scenario.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[, c(1,6,7,8)])
}

#list target
if(args$list_target){
  #The item's .txt file is read
  subDir <- "./FileSystem/Target.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[, c(1,5,6)])
}

#list parameters
if(args$list_parameters){
  #The item's .txt file is read
  subDir <- "./FileSystem/Parameters.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[,c(1,3,9,10)])
}

#list instances
if(args$list_instances){
  #The item's .txt file is read
  subDir <- ("./FileSystem/Instances.txt")
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[,c(1,9,10)])
}

#list versions
if(args$list_versions){
  #The item's .txt file is read
  subDir <- "./FileSystem/Version.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[, c(1,4,5)])
}

#list experiment
if(args$list_experiment){
  #The item's .txt file is read
  subDir <- "./FileSystem/Experiment.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  #The relevant information of each element is displayed
  kable(fileData[,c(1,3,4,8,10,11)])
}

###############################################################################################################
#ARGUMENTS TO SHOW
###############################################################################################################

#show scenario by entering the name
if(args$show_scenario != "NA"){
  #The name entered by the user is assigned to a variable
  scenarioName <- tolower(args$show_scenario)
  
  ##It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
  
  #If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one
  if(!file.exists(checkFile)){
    cat(magenta('The entered scenario does not exist, please try another.'))
    cat('\n')
    repeat{
      cat(blue$bold('Enter the scenario to display: \n'))
      scenarioName <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered scenario does not exist, please try another.'))
      cat('\n')
    }
  }
  
  ##The information of the scenario entered within the file system is searched
  subDir <- "./FileSystem/Scenario.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == scenarioName)
  
  ##Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Parameter space:'), x[,3], '\n')
  cat(blue$underline$bold('Set of instances:'), x[,4], '\n')
  cat(blue$underline$bold('Options route:'), x[,5], '\n')
  cat(blue$underline$bold('Type:'), x[,6], '\n')
  cat(blue$underline$bold('Date added:'), x[,7], '\n')
  cat(blue$underline$bold('Date modified:'), x[,8], '\n')
}

#show target by entering the name
if(args$show_target != "NA"){
  #The name entered by the user is assigned to a variable
  targetName <- tolower(args$show_target)
  
  #It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
  
  #If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one.
  if(!file.exists(checkFile)){
    cat(magenta('The entered target does not exist, please try another. \n'))
    
    repeat{
      cat(blue$bold('Enter the target algorithm to display: \n'))
      targetName <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered target does not exist, please try another. \n'))
    }
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Target.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == targetName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Target rounner route:'), x[,3], '\n')
  cat(blue$underline$bold('Executable route:'), x[,4], '\n')
  cat(blue$underline$bold('Date added:'), x[,5], '\n')
  cat(blue$underline$bold('Date modified:'), x[,6], '\n')
}

#show parameter by entering the name
if(args$show_parameter != "NA"){
  #The name entered by the user is assigned to a variable
  parameterName <- tolower(args$show_parameter)
  
  #It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
  
  #If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one.
  if(!file.exists(checkFile)){
    cat(magenta('The entered parameter does not exist, please try another. \n'))
    
    repeat{
      cat(blue$bold('Enter the parameter to display: \n'))
      parameterName <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      checkFile <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered parameter does not exist, please try another. \n'))
    }
  }
  
  ##The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Parameters.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == parameterName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Target algorithm:'), x[,3], '\n')
  cat(blue$underline$bold('#Parameters:'), x[,4], '\n')
  cat(blue$underline$bold('Type:'), x[,5], '\n')
  cat(blue$underline$bold('Forbidden:'), x[,6], '\n')
  cat(blue$underline$bold('Initial:'), x[,7], '\n')
  cat(blue$underline$bold('File-path:'), x[,8], '\n')
  cat(blue$underline$bold('Date added:'), x[,9], '\n')
  cat(blue$underline$bold('Date modified:'), x[,10], '\n')
}

#show instance by entering the name
if(args$show_instance != "NA"){
  #The name entered by the user is assigned to a variable
  instanceName <- tolower(args$show_instance)
  
  ##It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
  
  ##If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one.
  if(!file.exists(checkFile)){
    cat(magenta('The entered instance does not exist, please try another. \n'))
    
    repeat{
      cat(blue$bold('Enter the instance to display: \n'))
      instanceName <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered instance does not exist, please try another. \n'))
    }
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Instances.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == instanceName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Training:'), x[,3], '\n')
  cat(blue$underline$bold('#training:'), x[,4], '\n')
  cat(blue$underline$bold('Testing:'), x[,5], '\n')
  cat(blue$underline$bold('#testing:'), x[,6], '\n')
  cat(blue$underline$bold('RouteTraining:'), x[,7], '\n')
  cat(blue$underline$bold('RouteTesting:'), x[,8], '\n')
  cat(blue$underline$bold('Date added:'), x[,9], '\n')
  cat(blue$underline$bold('Date modified:'), x[,10], '\n')
}

#show version by entering the name
if(args$show_version != "NA"){
  #The name entered by the user is assigned to a variable
  versionNumber <- tolower(args$show_version)
  
  #It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
  
  #If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one.
  if(!file.exists(checkFile)){
    cat(magenta('The entered version does not exist, please try another. \n'))
    
    repeat{
      cat(blue$bold('Enter the version to display: \n'))
      versionNumber <- scan(quiet = T,'stdin', character(), n=1)
      
      checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered version does not exist, please try another. \n'))
    }
  }

  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Version.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Version.Number == versionNumber)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Version Number:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Route:'), x[,3], '\n')
  cat(blue$underline$bold('Date added:'), x[,4], '\n')
  cat(blue$underline$bold('Date modified:'), x[,5], '\n')
}

#show experiment by entering the name
if(args$show_experiment != "NA"){
  #The name entered by the user is assigned to a variable
  experimentName <- tolower(args$show_experiment)
  
  #It checks if the entered name exists
  checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
  
  #If the name entered does not exist, it will be indicated to the user and they will be able to enter another valid one.
  if(!file.exists(checkFile)){
    cat(magenta('The entered experiment does not exist, please try another. \n'))
    
    repeat{
      cat(blue$bold('Enter the experiment to display: \n'))
      experimentName <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
      
      if(file.exists(checkFile)){
        break
      }
      cat(magenta('The entered experiment does not exist, please try another. \n'))
    }
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Experiment.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Experiment == experimentName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Version:'), x[,3], '\n')
  cat(blue$underline$bold('Scenario:'), x[,4], '\n')
  cat(blue$underline$bold('#Repetitions:'), x[,5], '\n')
  cat(blue$underline$bold('Results:'), x[,6], '\n')
  cat(blue$underline$bold('Settings:'), x[,7], '\n')
  cat(blue$underline$bold('Status:'), x[,8], '\n')
  cat(blue$underline$bold('Experiment-path:'), x[,9], '\n')
  cat(blue$underline$bold('Date added:'), x[,10], '\n')
  cat(blue$underline$bold('Date modified:'), x[,11], '\n')
}

#show scenario
if(args$showScenario){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the scenario to display: \n'))
    scenarioName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Scenario", scenarioName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered scenario does not exist, please try another.'))
    cat('\n')
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Scenario.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == scenarioName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Parameter space:'), x[,3], '\n')
  cat(blue$underline$bold('Set of instances:'), x[,4], '\n')
  cat(blue$underline$bold('Options route:'), x[,5], '\n')
  cat(blue$underline$bold('Type:'), x[,6], '\n')
  cat(blue$underline$bold('Date added:'), x[,7], '\n')
  cat(blue$underline$bold('Date modified:'), x[,8], '\n')
}

#show target
if(args$showTarget){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the target algorithm to display: \n'))
    targetName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Target", targetName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered target does not exist, please try another. \n'))
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Target.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == targetName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Target rounner route:'), x[,3], '\n')
  cat(blue$underline$bold('Executable route:'), x[,4], '\n')
  cat(blue$underline$bold('Date added:'), x[,5], '\n')
  cat(blue$underline$bold('Date modified:'), x[,6], '\n')
}

#show parameter
if(args$showParameter){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the parameter to display: \n'))
    parameterName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered parameter does not exist, please try another. \n'))
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Parameters.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == parameterName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Target algorithm:'), x[,3], '\n')
  cat(blue$underline$bold('#Parameters:'), x[,4], '\n')
  cat(blue$underline$bold('Type:'), x[,5], '\n')
  cat(blue$underline$bold('Forbidden:'), x[,6], '\n')
  cat(blue$underline$bold('Initial:'), x[,7], '\n')
  cat(blue$underline$bold('File-path:'), x[,8], '\n')
  cat(blue$underline$bold('Date added:'), x[,9], '\n')
  cat(blue$underline$bold('Date modified:'), x[,10], '\n')
}

#show instance
if(args$showInstance){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the instance to display: \n'))
    instanceName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Instances", instanceName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered instance does not exist, please try another. \n'))
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Instances.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Name == instanceName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Training:'), x[,3], '\n')
  cat(blue$underline$bold('#training:'), x[,4], '\n')
  cat(blue$underline$bold('Testing:'), x[,5], '\n')
  cat(blue$underline$bold('#testing:'), x[,6], '\n')
  cat(blue$underline$bold('RouteTraining:'), x[,7], '\n')
  cat(blue$underline$bold('RouteTesting:'), x[,8], '\n')
  cat(blue$underline$bold('Date added:'), x[,9], '\n')
  cat(blue$underline$bold('Date modified:'), x[,10], '\n')
}

#show version
if(args$showVersion){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the version to display: \n'))
    versionNumber <- scan(quiet = T,'stdin', character(), n=1)
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Version", versionNumber, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered version does not exist, please try another. \n'))
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Version.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Version.Number == versionNumber)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Version Number:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Route:'), x[,3], '\n')
  cat(blue$underline$bold('Date added:'), x[,4], '\n')
  cat(blue$underline$bold('Date modified:'), x[,5], '\n')
}

#show experiment
if(args$showExperiment){
  repeat{
    #The user is asked to enter the target name to display
    cat(blue$bold('Enter the experiment to display: \n'))
    experimentName <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    #It checks if the entered name exists
    checkFile <- paste("./FileSystem/Files/Experiment", experimentName, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    cat(magenta('The entered experiment does not exist, please try another. \n'))
  }
  
  #The information of the target entered within the file system is searched
  subDir <- "./FileSystem/Experiment.txt"
  
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  x <- subset(fileData, Experiment == experimentName)
  
  #Information is displayed to the user
  cat('\n')
  cat(blue$underline$bold('Name:'), x[,1], '\n')
  cat(blue$underline$bold('Description:'), x[,2], '\n')
  cat(blue$underline$bold('Version:'), x[,3], '\n')
  cat(blue$underline$bold('Scenario:'), x[,4], '\n')
  cat(blue$underline$bold('#Repetitions:'), x[,5], '\n')
  cat(blue$underline$bold('Results:'), x[,6], '\n')
  cat(blue$underline$bold('Settings:'), x[,7], '\n')
  cat(blue$underline$bold('Status:'), x[,8], '\n')
  cat(blue$underline$bold('Experiment-path:'), x[,9], '\n')
  cat(blue$underline$bold('Date added:'), x[,10], '\n')
  cat(blue$underline$bold('Date modified:'), x[,11], '\n')
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
      
      write.table(fileData, file = "./FileSystem/Target.txt", sep = "," ,row.names = FALSE, col.names = TRUE)
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Target", 
                               Sys.Date(),
                               targetName,
                               "Name", 
                               targetName,
                               newNameTarget
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
      
      
    }
    if(opt == 2){
      cat('Enter the new description for the target: ')
      cat('\n')
      newDescTarget <- tolower(readLines("stdin", n = 1))
      
      subDir <- "./FileSystem/Target.txt"
      fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
      
      oldDescription <- fileData$Description[fileData$Name == targetName]
      
      fileData$Description[fileData$Name == targetName] <- newDescTarget
      
      
      
      file.remove("./FileSystem/Target.txt")
      
      fromPath <- paste("./FileSystem/Files/Target", targetName, sep = "/")
      toPath <- paste("./FileSystem/Files/Target", newDescTarget, sep = "/")
      
      file.rename(fromPath, toPath)
      
      write.table(fileData, file = "./FileSystem/Target.txt", sep = "," ,row.names = FALSE, col.names = TRUE)
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Target", 
                               Sys.Date(),
                               targetName, 
                               "Description", 
                               oldDescription,
                               newDescTarget
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
      
      
    }
    if(opt == 3){
      cat('Se ingreso la opcion 3, se modifica el target runner')
      cat('\n')
      #Loop that helps entering a path so the system doesnt try to add a fake file.
      repeat{
        cat('Enter the new target runner path for the target: ')
        cat('\n')
        newTargetRunnerRoute <- scan(quiet = T,'stdin', character(), n=1)
        if(file.exists(newTargetRunnerRoute)){
          break
        } else {
          cat('File path doesnt found.')
        }
      }
      
      actualTargetRoute <- paste("./FileSystem/Files/Target/",targetName,sep='')
      actualTargetRoute <- paste(actualTargetRoute,paste(targetName,'_runner',sep = ''),sep = '/')
      file.remove(actualTargetRoute)
      file.copy(newTargetRunnerRoute,actualTargetRoute)
      
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Target", 
                               Sys.Date(),
                               targetName,
                               "Target runner path", 
                               targetName,
                               newTargetRunnerRoute
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
    }
    if(opt == 4){
      cat('Se ingreso la opcion 4, se medifica el ejecutable')
      cat('\n')
    }
    if(opt != 1 & opt != 2 & opt != 3 & opt != 4 ){
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
  cat('The target has been successfully modified.\n')
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
  
  #Select option to modify
  repeat{
    cat('Enter the option corresponding to the element you want to modify.')
    cat('\n')
    cat('1. Name')
    cat('\n')
    cat('2. Description')
    cat('\n')
    cat('3. Target algorithm')
    cat('\n')
    cat('4. Type of parameter')
    cat('\n')
    
    opt <- scan(quiet = T,'stdin', integer(), n=1)
    
    if(opt == 1){
      cat('Enter the new name for the parameter: ')
      cat('\n')
      newParamName <- scan(quiet = T,'stdin', character(), n=1)
      
      subDir <- "./FileSystem/Parameters.txt"
      fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
      
      fileData$Date.modified[fileData$Name == parameterName] <- Sys.Date()
      fileData$Name[fileData$Name == parameterName] <- newParamName
      
      file.remove("./FileSystem/Parameters.txt")
      
      fromPath <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
      toPath <- paste("./FileSystem/Files/Parameters", newParamName, sep = "/")
      
      file.rename(fromPath, toPath)
      
      write.table(fileData, file = "./FileSystem/Parameters.txt", sep = "," ,row.names = FALSE, col.names = TRUE)
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Parameter", 
                               Sys.Date(),
                               parameterName,
                               "Name", 
                               parameterName,
                               newParamName
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
      
      
    }
    if(opt == 2){
      cat('Enter the new description for the parameter: ')
      cat('\n')
      newParamDesc <- scan(quiet = T,'stdin', character(), n=1)
      
      subDir <- "./FileSystem/Parameters.txt"
      fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
      
      oldDescription <- fileData$Description[fileData$Name == parameterName]
      
      fileData$Date.modified[fileData$Name == parameterName] <- Sys.Date()
      fileData$Description[fileData$Name == parameterName] <- newParamDesc
      
      
      
      file.remove("./FileSystem/Parameters.txt")
      
      fromPath <- paste("./FileSystem/Files/Parameters", parameterName, sep = "/")
      toPath <- paste("./FileSystem/Files/Parameters", newParamDesc, sep = "/")
      
      file.rename(fromPath, toPath)
      
      write.table(fileData, file = "./FileSystem/Parameters.txt", sep = "," ,row.names = FALSE, col.names = TRUE)
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Parameters", 
                               Sys.Date(),
                               parameterName, 
                               "Description", 
                               oldDescription,
                               newParamDesc
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
      
      
    }
    if(opt == 3){
      cat('Se ingreso la opcion 3, se modifica el target runner')
      cat('\n')
      #Loop that helps entering a path so the system doesnt try to add a fake file.
      repeat{
        cat('Enter the new target runner path for the target: ')
        cat('\n')
        newTargetRunnerRoute <- scan(quiet = T,'stdin', character(), n=1)
        if(file.exists(newTargetRunnerRoute)){
          break
        } else {
          cat('File path doesnt found.')
        }
      }
      
      actualTargetRoute <- paste("./FileSystem/Files/Target/",targetName,sep='')
      actualTargetRoute <- paste(actualTargetRoute,paste(targetName,'_runner',sep = ''),sep = '/')
      file.remove(actualTargetRoute)
      file.copy(newTargetRunnerRoute,actualTargetRoute)
      
      
      #Registrar modificación
      #Add data to the file modification
      modificationData <- list("Target", 
                               Sys.Date(),
                               targetName,
                               "Target runner path", 
                               targetName,
                               newTargetRunnerRoute
      )
      write.table(modificationData, file = "./FileSystem/ChangeLog.txt", sep = "," ,row.names = FALSE, col.names = FALSE, append = TRUE)
      
    }
    if(opt == 4){
      cat('Se ingreso la opcion 4, se medifica el ejecutable')
      cat('\n')
    }
    if(opt != 1 & opt != 2 & opt != 3 & opt != 4 ){
      cat('The option entered is not correct, please try again.')
      cat('\n')
    }
    
    #############################################################
    
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

###############################################################################################################
#WEBSITE CREATION
###############################################################################################################
if(args$web){
  shiny::runApp()
}

###############################################################################################################
#RUN THE EXPERIMENT
###############################################################################################################
if(args$execute_experiment){
  repeat{
    cat('Enter the experiment you want to run: \n')
    nameExperiment <- tolower(scan(quiet = T,'stdin', character(), n=1))
    
    checkFile <- paste("./FileSystem/Files/Experiment", nameExperiment, sep = "/")
    
    if(file.exists(checkFile)){
      break
    }
    
    #If the entered scenario is not found, the option is given to create a new one or enter another one.
    repeat{
      cat('The entered experiment does not exist in the database, want to add it? \n')
      cat('Enter "Y" to add or "N" to try again. \n')
      opt <- tolower(scan(quiet = T,'stdin', character(), n=1))
      
      if(opt == "y"){
        flagExperiment <- TRUE
        nameExperiment <- addExperiment(flagExperiment)
        break
      }
      if(opt == "n"){
        break
      }
      if(opt != "y" | opt != "n"){
        cat('The option entered is not valid, please try again. \n')
      }
      Sys.sleep(0.5)
    }
    
    if(flagExperiment == TRUE){
      break
    }
  }
  
  #information is read from the database
  subDir <- "./FileSystem/Experiment.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  #The information of the experiment entered by the user is obtained
  xExperiment <- subset(fileData, Experiment == nameExperiment)
  
  scenarioName <- xExperiment[,3]
  nRepetitions <- xExperiment[,5]
  
  #It must be modified according to whether you work on a computer or with servers
  #He is currently working on a computer
  cores = 1
  
  #The scenario indicated in the experiment is entered to obtain information.
  subDir <- "./FileSystem/Scenario.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  xScenario <- subset(fileData, Name == scenarioName)
  
  scenarioFile <- xScenario[,5]
  nameParameter <- xScenario[,3]
  nameInstance <- xScenario[,4]
  
  #The parameter indicated in the escenario is entered to obtain information.
  subDir <- "./FileSystem/Parameters.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  xParameter <- subset(fileData, Name == nameParameter)
  
  parameterFile <- xParameter[,4]
  nameTarget <- xParameter[,3]
  forbiddenFile <- xParameter[,6]
  
  if(forbiddenFile != -1){
    forbiddenFile <- NULL
  }
  
  #The target indicated in the parameter is entered to obtain information.
  subDir <- "./FileSystem/Target.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  xTarget <- subset(fileData, Name == nameTarget)
  
  targetRunner <- xTarget[,3]
  
  #The instance indicated in the scenario is entered to obtain information.
  subDir <- "./FileSystem/Instances.txt"
  fileData <- read.delim(file = subDir, header = TRUE, sep = ",", dec = ".")
  
  xInstance <- subset(fileData, Name == nameInstance)
  
  trainInstancesFile <- xInstance[,3]
  print(trainInstancesFile)
  testInstancesFile <- xInstance[,5]
  print(testInstancesFile)
  
  targetRunner="./FileSystem/Files/Target/trunv.p.1/trunv.p.1_runner.exe"
  
  runExperiment(scenarioName, nameExperiment, nRepetitions, cores, 
                scenarioFile, parameterFile, targetRunner, 
                trainInstancesFile, testInstancesFile = NULL, 
                configurationsFile = NULL, forbiddenFile = NULL)
  
}