library(Limma)

normalizedArray <- R6Class("rawArray",
	public = list(
		normalizedData = NULL,
		method = NULL,
		filter_level = NULL,
		ratio = NULL, 
		bgCorrect = NULL,

		initialize <- function(rawData, method, filter_level, ratio, bgCorrect = FALSE){
			self$ratio = ratio
			self$filter_level = filter_level
			self$method = method
			self$bgCorrect = bgCorrect
		},

		normalize <- function(){
			normData = NULL
			self$normalizedData = normData 
		},


		))