metadata = R6Class("metadata",
	public = list(
		targets = NULL,
		validGroups = NULL,

		initialize = function(targetPath){
			self$targets = readTargets(file = "0", path = targetPath)
			self$calculateValidGroups(self$targets)
			},
		
		calculateValidGroups = function(targets){
		  for (i in 1:ncol(targets)){
    	  	if (i == 1){
      	  		exp_types <- (targets[[i]])
            }else{
      			exp_types <- paste(exp_types, targets[[i]], sep = ".")
   				 }
  			}
  		   self$validGroups = exp_types
		}
	)
)