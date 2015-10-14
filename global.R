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

getTutorial <- function(){
  return (tutorial)
}

tutorial <- '<!DOCTYPE html>
<html>
<body>

  <img src="contrastMatrix.png" alt="A contrast matrix!" width="172" height="169">
  
  <p> Contrast matrices are used in Limma to define the experiment you\'ll be looking for differential expression in. They come from Linear Algebra, and you can read more about them in the links below if you would like to dig deeper, but ShinyLimma takes care of the technical aspects. All you really need to know is how to tell ShinyLimma which groups you\'d like to compare in order to define your contrast matrix!
  </p>
  
  <h2> Syntax </h2>
  <p> You\'ll see on the right side of the screen that ShinyLimma has provided your contrast matrix. You can use these to reference your different samples to decide which are of interest. For example, if your target sample looked like this...</p>


<p>Then you would define the comparison of mutant treated and wild treated with :</p>

<img src="SyntaxExample.png" alt="example syntax" width="280" height="500">

<p>Essentially, just seperate the groups you\'re interested in by a period <b>.</b> </p>
  
  <p> Feel free to get practice with it! If you enter it wrong, ShinyLimma will let you know to fix it!</p>

<a href = "https://www.bioconductor.org/help/course-materials/2009/BioC2009/labs/limma/limma.pdf"> More contrast matrix info here.<a/>
  </body>
  </html>
  '