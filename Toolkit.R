# Toolkit.R
# Runs all files containing auxiliary functions for clusterization project in folder ./code/
# Authors: César Isaí Mendoza Arredondo & Cynthia Elizabeth Castillo Silva

sapply(list.files(path="./code", full.names=TRUE), source)