download <<- FALSE

changeDownload <- function(){
  download <<- TRUE
}

getDownload <- function(){
  return (download)
}

targets <- NULL

changeTargets <- function(t){
	targets <<- t
}

getTargets <- function(){
    return (targets)
  }