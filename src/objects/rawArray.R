library(limma)

rawArray = R6Class("rawArray",
	public = list(
		rawData = NULL,
		initialize= function(probePath, controlPath){
		  cat("attemping to apply ilmn/n")
		  cat(probePath, "\n")
		  cat(controlPath, "\n")
		  self$rawData = read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
		},

		rawMicroarray.getData = function(){
			return (self$rawData)
		}
  )     
)