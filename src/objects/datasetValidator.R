library(tools)
"Progammer: Shane Caldwell
Goal: Functions needed to validate whether use inputted
datasets fit the format necessary to continue on with analysis.

The input object will have an instance of both this and a datamanager"



Validator = R6Class("Validator",
    public = list(
        validated = FALSE,
        PATH = 0,
        NAME = 1,
        TYPE = 2,
                
        initialize = function(probe, control, target){
	        probeList   = validate.packer(probe)
	        controlList = validate.packer(control)
	        targetList  = validate.packer(target)
	        acceptableData = validate.delegator(probeList, controlList, targetList)
	        if (acceptableData){
		        self$validated = True 
	        }
         },
                
        isValid = function(){
            return (self$validated)
        },

        validate.packer = function(uploaded){
		      toValidate = list(uploaded$datapath)
			    toValidate = list(toValidate, uploaded$name)
			    toValidate = list(toValidate, uploaded$type)
			    return (toValidate)
		    },

        validate.delgator = function(probe, control, targets){
	        probePass   = validate.probe(probe[self$PATH], probe[self$NAME], probe[self$TYPE])
	        controlPass = validate.control(control[self$PATH], control[self$NAME], control[self$TYPE])
	        targetsPass = validate.targets(targets[self$PATH], targets[self$NAME], targets[self$TYPE])
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
	        if (!("targets" %in% tolower(filename))){
		        return (FALSE)
	        }
	        return(TRUE)
        }
          
                 
    )
)



