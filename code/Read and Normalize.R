# Read and Normalize.R
# Reads and normalizes data
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

readData <- function(tasks, filePath="data/"){
  # Reads assigned files from a folder
  
  # Set variables
  files     <- list.files(path=filePath, pattern="*.csv", full.names=TRUE)
  files     <- files[tasks]
  fileNames <- gsub("data/|.csv", "", files)
  numTasks  <- length(tasks)
  
  # Read and store data
  data <- list()
  for(i in 1:numTasks){
    data[[i]]               <- list()
    data[[i]]$rawData       <- read_csv(file=files[i])
    data[[i]]$rawData$color <- as.numeric(data[[i]]$rawData$color)+1
    names(data)[i]          <- fileNames[i]
  }
  
  # Return assigned data
  return(data)
}

normalizeData <- function(data){
  # Normalizes raw data
  
  # Initialize variables
  numTasks <- length(data)
  
  # Normalize data and save realTags
  for(i in 1:numTasks){
    aux                     <- data[[i]]$rawData
    data[[i]]$processedData <- as_tibble(scale(select(aux,x,y)))
    data[[i]]$realTags      <- as.numeric(aux$color)
  }
  
  # Return normalized data
  return(data)
}