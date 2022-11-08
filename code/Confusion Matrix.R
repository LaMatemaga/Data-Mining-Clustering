# Confusion Matrix.R
# Gets confusion matrix after making predictions
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

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