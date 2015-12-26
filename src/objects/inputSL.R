inputSL = R6Class("inputSL",
	public = list(
		dataValidator = NULL,
		dataManager = NULL,
		targetManager = NULL,

		initialize = function(probe, control, target){
			self$dataValidator <<- Validator$new(probe, control, target)
			if (self$isValid()){
				self$loadArrayData(probe, control)
			}
		},

		isValid = function(){
			if (is.null(self$dataValidator)){
				return (FAlSE)
			}else{
				return(self$dataValidator$isValid())
			}
		},

		loadArrayData = function(probeFile, controlProbeFile){
			probePath = probeFile$datapath
    		controlPath = controlProbeFile$datapath
    		probePath = substr(probePath, 1, nchar(probePath)-2)
    		controlPath = substr(controlPath, 1, nchar(controlPath)-2)
    		self$dataManager = rawArray$new(probePath, controlPath)
		},

		loadTargetData = function(targetFile){
			targetPath = targetFile$datapath
			targetPath = substr(targetPath, 1, nchar(targetPath)-2)
			self$targetManager = metadata$new(targetPath)
		},
		
		plot = function(...){
			plot(self$dataManager)
		}
		
		

	)
)

