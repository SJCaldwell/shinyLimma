inputSL = R6Class("inputSL",
	public = list(
		dataValidator = NULL,
		dataManager = NULL,
		targetManager = NULL,
		target_path = NULL,
		probe_path = NULL,
		control_path = NULL, 

		initialize = function(probe, control, target){
		  self$probe_path = probe$name
		  self$control_path = control$name
		  self$target_path = target$name
			self$dataValidator <<- Validator$new(probe, control, target)
			if (self$isValid()){
				self$loadArrayData(probe, control)
			  self$loadTargetData(target)
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
		
		densityPlot = function(){
			return(self$dataManager$densityPlot())
		}
		
		

	)
)

