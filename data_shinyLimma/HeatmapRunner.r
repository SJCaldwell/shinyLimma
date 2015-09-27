###Visualizations for analysis Page###
#source("http://www.bioconductor.org/biocLite.R")
#biocLite("ALL")
library(limma)
#Heat map for different groups. How well do they cluster?
setwd("C://Users//jimc//Desktop//ShinyLimmaWorkspace//data_shinyLimma")
#x <- read.ilmn(files = "LRawReport.txt", ctrlfiles = "ControlProbelimma.txt" )
targets <- readTargets()
x <- read.ilmn(files = "LRawReport.txt", ctrlfiles = "ControlProbelimma.txt" )
#x$E
for (i in 1:ncol(targets)){
  if (i == 1){
    exp_types <- (targets[[i]])
  }else{
    exp_types <- paste(exp_types, targets[[i]], sep = ".")
  }
}
design <- model.matrix(~0+exp_types)
colnames(design) <- levels(as.factor(exp_types))
  

fit <- eBayes(lmFit(x$E,design))
selected  <- p.adjust(fit$p.value[, 2]) <0.05
esetSel <- x$E [selected, ]
str(esetSel)
color.map <- function(mol.biol) {
  if (mol.biol=="ALL1/AF4") "#FF0000" 
  else "#0000FF" 
  }
patientcolors <- unlist(lapply(esetSel, color.map))
heatmap(esetSel, col=topo.colors(100), ColSideColors=patientcolors[1:91])
