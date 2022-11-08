# Evaluation Indices.R
# Gets evaluation indices (accuracy, precision, recall, specificity, jaccard distance
# and Fowlkes-Mallows index)using confusion matrix
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

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