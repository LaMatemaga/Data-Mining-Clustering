# K-means.R
# Permorms k-means with max.iterations 5 and 10
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

get.kmeans <- function(data,kClust,iterations=c(5,10)){
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