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

		output_RMD = function(){
			report <<- ""
			report <<- self$add_header(report, title, author, output_type)
			return (report)


		},

		add_header = function(report, title, author, output_type){
			report <<- paste0(report, "---\n")
			report <<- paste0(report, "title: ", title, "\n")
			report <<- paste0(report, "author: ", author, "\n")
			report <<- paste0(report, "date: ", '"', date(), '"', "\n")
			report <<- paste0(report, "output: ", output_type, "\n")
			report <<- paste0(report, "---\n")
			return (report)
		},

))