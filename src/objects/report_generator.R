report_generator <- R6Class("report_generator",
	public = list(
		input = NULL,
		preprocessing = NULL,
		contrast = NULL,
		analysis = NULL,
 		
 		initialize = function(input, preprocessing, contrast, analysis){
			self$input = input
			self$preprocessing = preprocessing
			self$contrast = contrast
			self$analysis = analysis
			},


))