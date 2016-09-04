library(tools)
"Progammer: Shane Caldwell
Goal: Functions needed to validate whether use inputted
datasets fit the format necessary to continue on with analysis.

The input object will have an instance of both this and a datamanager"

Validator = R6Class("Validator",
    public = list(
        validated = FALSE,
        PATH = 1,
        NAME = 2,
        TYPE = 3,
        
        initialize = function(probe, control, target){
	        probeList   = self$validate.packer(probe)
	        controlList = self$validate.packer(control)
	        targetList  = self$validate.packer(target)
	        acceptableData = self$validate.delegator(probeList, controlList, targetList)
	        cat(acceptableData, "\n")
	        if (acceptableData){
		        self$validated = TRUE
	        }
         },
                
        isValid = function(){
            return (self$validated)
        },

        validate.packer = function(uploaded){
		      toValidate = list(uploaded$datapath)
			    toValidate = append(toValidate, uploaded$name)
			    toValidate = append(toValidate, uploaded$type)
			    return (toValidate)
		    },

		    validate.delegator = function(probe, control, targets){
	        probePass   = self$validate.probe(probe[[self$PATH]], probe[[self$NAME]], probe[[self$TYPE]])
	        controlPass = self$validate.control(control[[self$PATH]], control[[self$NAME]], control[[self$TYPE]])
	        targetsPass = self$validate.targets(targets[[self$PATH]], targets[[self$NAME]], targets[[self$TYPE]])
	        if(probePass && controlPass && targetsPass){
		        return (TRUE)
	        }
	        return(FALSE)
        },

        validate.probe = function(filepath, filename, filetype){
	        if (filetype != "text/plain"){
		        return (FALSE)
	        }
	        return(TRUE)
        },

        validate.control = function(filepath, filename, filetype){
	        if(filetype != "text/plain"){
				return (FALSE)
	    }
	    	return(TRUE)
},

        validate.targets = function(filepath, filename, filetype){
	        if (filetype != "text/plain"){
		        return (FALSE)
	        }
	        return(TRUE)
        },

        print = function(...){
          cat("<DataValidator class> instance that has determined the validity of the data to be ", self$validated, ".", sep = "")
          invisible(self)
        }

          
                 
    )
)



