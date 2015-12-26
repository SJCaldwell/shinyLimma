"Programmer: Shane Caldwell
Goal: should essentially create a file object that looks an awful 
lot like Shiny's so I can test my validator wihout reloading my
application constantly"



shinyFile = R6("shinyFile",
	public = list(
		datapath = NULL,
		name = NULL,
		type = NULL,

		initialize = function(datapath, name, type){
			self$datapath = datapath
			self$name = name
			self$type = type
		}

	)
)