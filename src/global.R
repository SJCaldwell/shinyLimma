#Experimenting with Mandatory Fields
fieldsMandatoryDataset <- c("probeFile", "targets", "controlProbeFile")
fieldsMandatoryRunModel <- c("group1Syntax", "group2Syntax")

correlationChoice <-FALSE

changeCorrelationChoice <- function(choice){
  correlationChoice <<- choice
}

getCorrelationChoice <- function(){
  return (correlationChoice)
}

download <<- FALSE

changeDownload <- function(){
  download <<- TRUE
}

getDownload <- function(){
  return (download)
}


isnt.null <- function(x) !is.null(x)


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