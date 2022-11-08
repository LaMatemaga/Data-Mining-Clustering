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
              "cluster",              # 
              "xtable")               # Export data frame to latex table
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

# Get evaluation indexes
data <- get.indices(data)

# Export graphs
exportGraphs(data)