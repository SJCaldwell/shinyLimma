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
			report <<- generate_report(report)
			return (report)
		},

		generate_report(report){
			report <<- self$add_header(report, self$title, self$author, self$output_type)
			report <<- self$add_section_break(report) 
			report <<- self$add_opening(report)
			report <<- self$add_section_break(report) 
			report <<- self$add_preprocessing(report)
			report <<- self$add_section_break(report)
			report <<- self$add_contrast_matrix(report)
			report <<- self$add_section_break(report)
			report <<- self$add_anlaysis(report)
			return (report) 
		},

		add_section_break = function(report){
			report <- paste0(report, '\n\n')
			return (report)
		},

		add_code_snippet_opening = function(report){
			report <- add_section_break(report)
			report <- paste0(report, "```{r }")
			return (report)
		},
		
		add_code_snippet_ending = function(report){
			report <- paste0(report, "```")
			report <- add_section_break(report) 
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
		},

		add_opening = function(report){
			report <- paste0(report, "# Input\n")
			report <- paste0(report, "The dataset was made up of x arrays")
			report <- paste0(report, " and came from file .")
			report <- paste0(report, "The control probe was from a file named ")
			report <- paste0(report, "The targets file was named X and featured the following variables.")
            report <- paste0(report, "Each sample had a (number) of probes")
            return (report)
	    },

		add_preprocessing = function(report){
			report <- paste0(report, "# Preprocessing\n")
			report <- paste0(report, "The normalization method selected was: X")
			report <- paste0(report, "Background selection (was/wasn't) selected.")
			report <- paste0(report, "To pass, a probe needed a confidence level of blank% across blank% of arrays.")
			report <- paste0(report, "A total of # probes were filtered, a blank% reduction of the initial original_number.")
			report <- add_code_snippet_opening(report)
			report <- paste0(report, "codeeee")
			report <- add_code_snippet_ending(report)
			return (report)
		},

		add_contrast_matrix = function(report){
			report <- paste0(report, "# Contrast Matrix \n")
			return (report)
		},

		add_analysis = function(report){
			report <- paste0(report, "# Analysis \n")
			return (report)
		}
	
))
