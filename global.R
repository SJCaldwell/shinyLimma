download <- FALSE

changeDownload <- function(){
  download <<- TRUE
}

getDownload <- function(){
  return (download)
}