normalization <- function(data, style){
  if (style == 1){
    return (data)
  }
  else if (style == 2){
    newData = normalizeVSN(data)
    return (newData)
  }
  else if (style == 3){
    newData = neqc(data)
    return (newData)
  }
  else if (style == 4){
    newData = normalizeBetweenArrays(data, method = "cyclicloess", cyclic.method = "fast")
    return (newData)
  }else{
    return(-1)
  }
}