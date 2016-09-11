FilterBy <- function(geneMatrix, correctionMethod, cutoff){
  if (correctionMethod == 'P.Value'){
      significant = subset(table, table$P.Value <= cutoff)
  }else{
      significant = subset(table, table$adj.P.Val <= cutoff)
  }
    return (significant)
}
