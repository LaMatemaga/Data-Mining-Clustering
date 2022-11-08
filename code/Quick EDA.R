# Quick EDA.R
# Export quick EDAs for all datasets in data folder
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

exportEDA <- function(tasks=1:30, filePath="export/EDA/"){
  # Makes graphs of raw data
  
  # Set variables
  data      <- readData(tasks)
  files     <- list.files(path="data/", pattern="*.csv", full.names=TRUE)
  files     <- files[tasks]
  fileNames <- gsub("data/|.csv", "", files)
  numTasks  <- length(tasks)
  
  # Make graphs
  for(i in 1:numTasks){
    png(file=paste(filePath,"plot_",fileNames[i],".png",sep=""),width=600, height=600)
    plot(data[[i]]$rawData, col=as.numeric(data[[i]]$rawData$color+1))
    dev.off()
  }
}