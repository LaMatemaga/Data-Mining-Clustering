# Main.R
# Core code of clusterization project
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva


# Initialize libraries, workspace, and functions
packages <- c("ggplot2",              # Create visualizations
              "dplyr",                # Manipulate dataframes
              "tibble",               # Auxiliary library for dplyr
              "plyr",                 # Combine dataframes
              "readr",                # read_csv is way faster than read.csv
              "fpc",                  # 
              "cluster")              # 
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)
rm(packages)
setwd("C:/Users/Elaia/Documents/Academia/Facultad/2022 (02) - Agosto-Diciembre/Data Mining/Clustering")
source("Toolkit.R")                   # Call custom functions

# EDA for all data. Save png files in export/
# exportEDA()

# File reading
tasks <- c(3,10,19)
kClust <- c(3,12,5)
data <- readData(tasks)
data <- normalizeData(data)

# K-means and Hierarchical Clustering Modeling
data <- get.kmeans(data,kClust)
data <- get.hclustering(data, kClust)






### Might be helpful to have later

indices <- cluster.stats(d=datosN.euc, clustering=data$basic3$realTags, alt.clustering=NULL)
indices$dunn
silh<-silhouette(tags.single,datosN.euc) 


val<-summary(silh)
val[["si.summary"]][["Mean"]] #Medida relativa   No supe como extraerlo sin guardarlo en otra variable jeje
val$si.summary$Mean



tibble.df <- bind_cols(data$boxes3$processedData,color=paste("Grupo",data$boxes3$realTags))

ggplot(tibble.df,aes(x,y)) + geom_point(aes(color=color))


+
  theme_light() + xlab("Variable x") + ylab("Variable y")




##################


##


# Single method of hierarchical clustering
HC.single     <- hclust(dataEuclidean, method = "single")
tags.single   <- as.numeric(cutree(HC.single, k=kClust[i]))
matrix.single <- getConfusionMatrix(data[[i]]$realTags,tags.single)

# Average method of hierarchical clustering
HC.average     <- hclust(dataEuclidean, method = "average")
tags.average   <- as.numeric(cutree(HC.average, k=kClust[i]))
matrix.average <- getConfusionMatrix(data[[i]]$realTags,tags.average)

# Save prediction tags in list
data[[i]]$results$hclust.complete$tags <- tags.complete
data[[i]]$results$hclust.single$tags   <- tags.single
data[[i]]$results$hclust.average$tags  <- tags.average

# Save confusion matrices in list
data[[i]]$results$hclust.complete$confusionMatrix <- matrix.complete
data[[i]]$results$hclust.single$confusionMatrix   <- matrix.single
data[[i]]$results$hclust.average$confusionMatrix  <- matrix.average