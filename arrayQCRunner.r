library(arrayQualityMetrics)
library(lumi)
#setwd("C://Users//jimc//Desktop//ShinyLimmaWorkspace//data_shinyLimma")

QC_Reporter <- function(path){
  x.lumi <- lumiR.batch(path)
  arrayQualityMetrics(x.lumi, outdir = "QCReport")
  cat("Done and done.")
}
