library(tools)
"Progammer: Shane Caldwell
Goal: Functions needed to validate whether use inputted
datasets fit the format necessary to continue on with analysis."

#text/plain 
#/tmp/Rtmp7XnzxF/2871fc1ed00cc2c9992a7ed3/0 
#LungProbe.txt 

#For validating targets
#Returns BOOLEAN true/false
PATH <- 0
NAME <- 1
TYPE <- 2

validate <- function(probe, control, target){
	probeList   <- validate.packer(probe)
	controlList <- validate.packer(control)
	targetList  <- validate.packer(target)
	acceptableData <- validate.delegator
	if (acceptableData){
		return (TRUE)
	}
	return (FALSE)
}

validate.packer <- function(uploaded){
	toValidate <- list(uploaded$datapath)
	toValidate <- list(toValidate, uploaded$name)
	toValidate <- list(toValidate, uploaded$type)
	return (toValidate)
}

validate.delgator <- function(probe, control, targets){
	probePass <- validate.probe(probe[PATH], probe[NAME], probe[TYPE])
	controlPass <- validate.control(control[PATH], control[NAME], control[TYPE])
	targetsPass <- validate.targets(targets[PATH], targets[NAME], targets[TYPE])
	if(probePass && controlPass && targetsPass){
		return (TRUE)
	}
	return(FALSE)
}

validate.probe <- function(filepath, filename, filetype){
	if (filetype != "text/plain"){
		return (FALSE)
	}
	return(TRUE)
}

validate.control <- function(filepath, filename, filetype){
	if(filetype != "text/plain"){
		return (FALSE)
	}
	return(TRUE)
}

validate.targets <- function(filepath, filename, filetype){
	if (filetype != "text/plain"){
		return (FALSE)
	}
	if (!("targets" %in% tolower(filename))){
		return (FALSE)
	}
	return(TRUE)
}