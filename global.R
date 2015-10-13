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

computeMatrix <- function(group1, group2, correlationFix){
  typesToInclude <- strsplit(group1, ".", fixed = TRUE)
  design <- list()
  for (i in 1:length(typesToInclude[[1]])){
    type <- typesToInclude[[1]][i]
    num <- grep(type, targets)
    design <- c(design, num)
  }
  for (i in 1:length(design)){
    if (i == 1){
      type <- design[[i]]
      exp_types <- (targets[[type]])
    }else{
      type <- design[[i]]
      exp_types <- paste(exp_types, targets[[type]], sep = ".")
    }
  }
  exp_design <- model.matrix(~0+exp_types)
}
