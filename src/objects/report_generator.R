report_generator <- R6Class("report_generator",
	public = list(
		input = NULL,
		preprocessing = NULL,
		contrast = NULL,
		analysis = NULL,
		title = NULL, 
		author = NULL,
		output_type = NULL,
 		
 		initialize = function(input, preprocessing, contrast, analysis, title, author, output_type){
			self$input = input
			self$preprocessing = preprocessing
			self$contrast = contrast
			self$analysis = analysis
			self$title = title
			self$author = author
			self$output_type = output_type
			},

		output_RMD = function(){
			report <<- ""
			report <<- self$add_header(report, self$title, self$author, self$output_type)
			return (report)
		},

		add_header = function(report, title, author, output_type){
			report <- paste0(report, "---\n")
			report <- paste0(report, "title: ", title, "\n")
			report <- paste0(report, "author: ", author, "\n")
			report <- paste0(report, "date: ", '"', date(), '"', "\n")
			report <- paste0(report, "output: ", output_type, "\n")
			report <- paste0(report, "---\n")
			return (report)
		}

))