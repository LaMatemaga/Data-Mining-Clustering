# Hierarchical Clustering.R
# Permorms hierarchical clustering (euclidean distance) with complete, single and average methods
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

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