library(limma)

rawArray = R6Class("rawArray",
	public = list(
		rawData = NULL,
		initialize = function(probePath, controlPath){
		  self$rawData = read.ilmn("0" , "0", path = probePath, ctrlpath = controlPath)
		},

		rawMicroarray.getData = function(){
			return (self$rawData)
		}
  )     
)