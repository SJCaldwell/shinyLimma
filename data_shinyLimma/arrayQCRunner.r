library(arrayQualityMetrics)
library(lumi)
setwd("C://Users//jimc//Desktop//ShinyLimmaWorkspace//data_shinyLimma")
x.lumi <- lumiR.batch("LRawReport.txt")
#x <- read.ilmn(files = "LRawReport.txt", ctrlfiles = "ControlProbelimma.txt" )
#y <- x$E
#row.names(y) <- 1:nrow(y)
arrayQualityMetrics(x.lumi)

