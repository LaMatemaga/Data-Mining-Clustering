# Export Results.R
# Export evaluation indices in LaTeX and cluster graphs for each method and dataset
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

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