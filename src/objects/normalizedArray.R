library(limma)
normalizedArray <- R6Class("rawArray",
	public = list(
		rawData = NULL,
		normalizedData = NULL,
		method = NULL,
		filter_level = NULL,
		ratio = NULL, 
		bgCorrect = NULL,
		before = NULL,
		after = NULL,

		initialize = function(rawData, method, filter_level, ratio, bgCorrect = FALSE){
			self$rawData = rawData
			self$ratio = ratio
			self$filter_level = filter_level
			self$method = method
			self$bgCorrect = bgCorrect
			self$normalize()
		},

		normalize = function(){
		  normData = self$background_Correct(self$rawData)
			normData = self$methodParser(normData, self$method)
			self$before = nrow(normData)
			normData = self$pdetectionFilter(normData, self$filter_level, self$ratio)
			self$normalizedData = normData
		},

		methodParser = function(data, style){
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
		background_Correct = function(data){
			if (self$bgCorrect){
			  return (backgroundCorrect(data))
			}
			return (data)
		},

		pdetectionFilter = function(normData, filter_level, ratio){
			expressed = rowSums(normData$other$Detection < filter_level) >= ratio
			normData = normData[expressed,]
			self$after = nrow(normData)
			return (normData)
		},
		
		boxplot = function(){
		  toPlot = self$normalizedData$E
		  return(boxplot(log2(toPlot),range=0,ylab="log2 intensity"))
		},

		probeFilterPlot = function(){
			twovals = c(self$before, self$after)
			names(twovals) = c("Before Filter", "After Filter")
			twovals = as.data.frame(twovals)
			colnames(twovals) = "Probe Count"
			twovals = cbind(twovals, rownames(twovals))
			colnames(twovals) = c("Probe_Count", "Status")
			d = ggplot(data= twovals, aes(x = Status, y = Probe_Count, colour = Status))
			d = d + geom_bar(stat = "identity", width = .5)
			return(d)
		}

	)
)