library(ggplot2)
library(limma)
library(reshape)
setwd("C://Users//jimc//Desktop//ShinyLimmaWorkspace//data_shinyLimma")
#x <- read.ilmn(files = "LRawReport.txt", ctrlfiles = "ControlProbelimma.txt" )
targets <- readTargets()



#What kinda var groups are we talking
categories <- getCategories(targets)
subCategories <- getSubCategories(categories, targets)




#Just take the names to define broad categories
getCategories <- function(targets){
  return(colnames(targets))
}

getSubCategories <- function(categories, targets){
  subCategories <- list()
  for (i in 1:ncol(targets)){
    subCategories[[i]] = unique(targets[,i])
  }
  return (subCategories)
}

