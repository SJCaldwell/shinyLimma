inputSL = R6Class("inputSL",
	public = list(
		dataValidator = NULL,
		dataManager = NULL,

		initialize = function(probe, control, target){
			self$dataValidator <<- Validator$new(probe, control, target)
			print(self$dataValidator)
			if (self$isValid()){
				self$loadArrayData(probeFile, controlProbeFile)
			}
		},

		isValid = function(){
		  cat('is valid?...\n')
			if (is.null(self$dataValidator)){
				return (FAlSE)
			}else{
				return(self$dataValidator$isValid())
			}
		},

		loadArrayData = function(probeFile, controlProbeFile, targetFile){
			probePath = probeFile$datapath
    		changeProbePath(probePath)
    		controlPath = controlFile$datapath
    		targetPath = targetFile$datapath
    		probePath = substr(probePath, 1, nchar(probePath)-2)
    		controlPath = substr(controlPath, 1, nchar(controlPath)-2)
    		targetPath = substr(targetPath, 1, nchar(targetPath)-2)
    		self$dataManager = rawArray$new(probePath, controlPath)
		}

	)
)

