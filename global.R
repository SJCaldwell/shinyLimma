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

validGroups <- NULL

getValidGroups <- function(){
  return (validGroups)
}

changeValidGroups <- function(valid){
  validGroups <<- valid
}

calculateValidGroups <- function(targets){
  for (i in 1:ncol(targets)){
    if (i == 1){
      exp_types <- (targets[[i]])
    }else{
      exp_types <- paste(exp_types, targets[[i]], sep = ".")
    }
  }
  return (exp_types)
}

isnt.null <- function(x) !is.null(x)
