# Toolkit.R
# Auxiliary functions for clusterization project
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

get.confusionMatrix <- function(observations,predictions){
  # Gets True Positives, True Negatives, False Positives, and False Negatives
  
  # Initialize variables
  numData <- length(observations)
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  
  # Get the entries of the confusion matrix
  for(i in 1:(numData-1)){
    for(j in (i+1):numData){
      # True Positives
      if(observations[i]==observations[j] & predictions[i]==predictions[j]){
        TP<-TP+1
      }
      
      # True Negatives
      if(observations[i]!=observations[j] & predictions[i]!=predictions[j]){
        TN<-TN+1
      }
      
      # False Negatives
      if(observations[i]==observations[j] & predictions[i]!=predictions[j]){
        FN<-FN+1
      }
      
      # False Positives
      if(observations[i]!=observations[j] & predictions[i]==predictions[j]){
        FP<-FP+1
      }
    }
  }
  
  # Store values in confusion matrix and put names in rows and columns
  confusionMatrix           <- matrix(c(TP,FP,FN,TN),ncol=2, nrow=2)
  colnames(confusionMatrix) <- c("PP","PN")
  rownames(confusionMatrix) <- c("P", "N")
  
  # Return confusionMatrix as output
  return(confusionMatrix)
}

get.hclustering <- function(data,kClust,methods=c("complete","single","average")){
  # Gets hierarchical clustering
  
  # Initialize variables
  numTasks    <- length(data)
  numMethods  <- length(methods)
  nameMethods <- paste("hclust.", methods, sep="")
  
  # Hierarchical clustering for all data sets with different methods
  for(i in 1:numTasks){
    # Get euclidean distances
    dataEuclidean <- dist(data[[i]]$processedData, method = "euclidean")
    
    # Iterate for each method
    for(j in 1:numMethods){
      # Get predicted tags using hierarchical clustering
      HC     <- hclust(dataEuclidean, method=methods[j])
      tags   <- as.numeric(cutree(HC, k=kClust[i]))
      matrix <- get.confusionMatrix(data[[i]]$realTags, tags)
      
      # Save tags and confusion matrix in results list
      tags   -> data[[i]]$results[[nameMethods[j]]]$tags
      matrix -> data[[i]]$results[[nameMethods[j]]]$confusionMatrix
    }
  }
  
  # Return results
  return(data)
}

get.kmeans <- function(data,kClust,iterations=c(5,10,20,30)){
  # Gets kmeans
  
  # Initialize variables
  numTasks           <- length(data)
  numIterations      <- length(iterations)
  namesResults       <- paste("kmeans.",iterations,sep="")
  
  # Gets clusters for all data sets with different hyperparameters
  for(i in 1:numTasks){
    for(j in 1:numIterations){
      # Get predicted tags using kmeans
      tags.kmeans <- kmeans(data[[i]]$processedData,
                            centers=kClust[i],
                            iter.max=iterations[j],
                            nstart=1)$cluster
      
      # Get confusion matrix for this instance
      matrix.kmeans <- get.confusionMatrix(data[[i]]$realTags,tags.kmeans)
      
      # Save tags and confusion matrix in results list
      tags.kmeans   -> data[[i]]$results[[namesResults[j]]]$tags
      matrix.kmeans -> data[[i]]$results[[namesResults[j]]]$confusionMatrix
    }
  }
  
  # Return results
  return(data)
}

get.indices <- function(data){
  # Gets the evaluation of the binary classifiers
  
  # Initialize variables
  numTasks      <- length(data)
  numMethods    <- length(data[[1]]$results)
  namesMethods  <- names(data[[1]]$results)
  
  # For each data set and each method
  for(i in 1:numTasks){
    for(j in 1:numMethods){
      # Get individual values
      TP <- data[[i]]$results[[j]]$confusionMatrix[["P","PP"]]
      FN <- data[[i]]$results[[j]]$confusionMatrix[["P","PN"]]
      FP <- data[[i]]$results[[j]]$confusionMatrix[["N","PP"]]
      TN <- data[[i]]$results[[j]]$confusionMatrix[["N","PN"]]
      
      # Evaluation indices
      accuracy <- (TP+TN)/(TP+FN+FP+TN)
      precision <- TP/(TP+FP)
      recall <- TP/(TP+FN)
      specificity <- TN/(TN+FP) 
      jaccard <- TP/(TP+FN+FP)
      fowlkes.mallows <- sqrt( (TP/(TP+FP)) * (TP/(TP+FN)) )
      
      # Store results in a temporal vector
      data[[i]]$indices[[j]] <- c(accuracy,precision,recall,specificity,jaccard,fowlkes.mallows)
    }
    
    # Rename temporal vectors and transform it into a dataframe (one per dataset)
    names(data[[i]]$indices) <- namesMethods
    data[[i]]$indices <- as.data.frame(data[[i]]$indices)
    rownames(data[[i]]$indices) <- c("Accuracy","Precision","Recall",
                                     "Specificity","Jaccard","Fowlkes-Mallows")
  }
  
  # Export results in folder
  exportLaTeX(data)
  
  # Return results
  return(data)
}

exportLaTeX <- function(data){
  # Export indices in path export/evaluation
  
  # Initialize variables
  numTasks  <- length(data)
  fileNames <- names(data)
  
  # Export evaluation results
  for(i in 1:numTasks){
    text <- xtable(data[[i]]$indices, digits=4, auto=TRUE, caption=paste("Tabla con los resultados de la clusterización del dataset \\code{",fileNames[i],"}.", sep=""))
    print(text,paste("export/evaluation/indices-",fileNames[i],".txt",sep=""), type="latex")
  }
}

exportGraphs <- function(data){
  # Export clasification graphs
  
  # Initialize variables
  numTasks  <- length(data)
  fileNames <- names(data)
  numMethods    <- length(data[[1]]$results)
  namesMethods  <- names(data[[1]]$results)
  
  # Repeat for each dataset
  for(i in 1:numTasks){
    # Get original data
    df <- data[[i]]$rawData
    
    # Change color column so it can be used as a tag
    df$color         <- as.factor(df$color)
    levels(df$color) <- paste("Grupo",LETTERS[1:16])
    
    # Make and save graph of observations
    png(file=paste("export/graphs/plot_",fileNames[i],"_original.png",sep=""), width=600, height=600)
    ggplot(df,aes(x,y)) +
      geom_point(aes(color=color)) +
      labs(title=paste("Dataset:", fileNames[i]), subtitle="Observaciones originales") +
      theme_light()
    dev.off()
    
    # Repeat for each method
    for(j in 1:numMethods){
      # Select only columns x and y
      df <- select(df,x,y)
      
      # Add tags obtained current method
      df <- cbind(df,color=data[[i]]$results[[j]]$tags)
      df$color         <- as.factor(df$color)
      levels(df$color) <- paste("Grupo",LETTERS)
      
      # Make and save graph of observations
      png(file=paste("export/graphs/plot_",fileNames[i],"_",namesMethods[j],".png",sep=""), width=600, height=600)
      ggplot(df,aes(x,y)) +
        geom_point(aes(color=color)) +
        labs(title=paste("Dataset:", fileNames[i]), subtitle=paste("Etiquetas obtenidas con el método",namesMethods[j])) +
        theme_light()
      dev.off()
    }
  }
}

