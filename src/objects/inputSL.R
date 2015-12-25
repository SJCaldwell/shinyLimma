inputSL <- R6Class("inputSL",
	public = list(
		dataValidator = NULL,
		dataManager = NULL,

		initalize <- function(probe, control, target){
			self$dataManager = Validator(probe, control, target)
		},

		isValid <- function(){
			if (self$dataValidator == NULL){
				return (FAlSE)
			}else{
				self$dataValidator.isValid()
			}
		},




	)
)