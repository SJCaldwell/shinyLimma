library(limma)

normalizedArray <- R6Class("rawArray",
	public = list(
		rawData = NULL,
		normalizedData = NULL,
		method = NULL,
		filter_level = NULL,
		ratio = NULL, 
		bgCorrect = NULL,

		initialize = function(rawData, method, filter_level, ratio, bgCorrect = FALSE){
			self$rawData = rawData
			self$ratio = ratio
			self$filter_level = filter_level
			self$method = method
			self$bgCorrect = bgCorrect
		},

		normalize = function(){
			normData = method.parser(self$rawData, self$method)
			normData = pdetection.filter(normData, self$filter_level, self$ratio)
			self$normalizedData = normData 
		},

		method.parser = function(data, style){
			NONE  = 1 
			VSN   = 2
			LOGQ  = 3
			LOESS = 4

			if (style == NONE){
				return(data)
			}
			else if (style == VSN){
				return (normalizeVSN(data))
			}
			else if (style == LOGQ){
				return (neqc(data))
			}
			else if (style == LOESS){
				return (normalizeBetweenArrays(data, method = "cyclicloess", cyclic.method = "fast"))
			}
			else{
				return (-1)
			}
		},

		##TODO: ADD ACTUAL BACKGROUND CORRECTION
		background.correct = function(data){
			if (self$bgCorrect){
			}
			return (data)
		},

		pdetection.filter = function(normData, filter_level, ratio){
			expressed = rowSums(normData$other$Detection < filter_level) >= ratio
			normData = normData[expressed,]
			return (normData)
		}

	)
)