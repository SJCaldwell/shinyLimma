exp_design = R6Class("exp_design",
	public = list(
		contrastMatrix = NULL,
		groupAsyntax = NULL,
		groupBsyntax = NULL,
		designExpression = NULL,
		validSyntax = FALSE,
		validGroups = NULL,

		initialize = function(groupA, groupB, validGroups, targets){
			self$validGroups = validGroups
			self$groupAsyntax = groupA
			self$groupBsyntax = groupB
			self$validSyntax = self$CMSyntaxChecker(groupA, groupB)
			self$designExpression =  (c(groupA, "-", groupB))
			self$designExpression = paste(self$designExpression, sep = "", collapse = "")
			if (self$validSyntax){
			  self$contrastMatrix = self$computeContrastMatrix(targets)
			}

		},
		computeContrastMatrix = function(targets){
			if (self$validSyntax){
				typesToInclude = strsplit(self$groupAsyntax, ".", fixed = TRUE)
				design = list()
				for (i in 1:length(typesToInclude[[1]])){
    				type = typesToInclude[[1]][i]
    				num = grep(type, targets)
    				design = c(design, num)
				}
  				for (i in 1:length(design)){
    				if (i == 1){
      					type = design[[i]]
      					exp_types = (targets[[type]])
    				}else{
      					type = design[[i]]
      					exp_types = paste(exp_types, targets[[type]], sep = ".")
    				}
  				}
  				exp_matrix = model.matrix(~0+exp_types)
  				colnames(exp_matrix) = levels(as.factor(exp_types))
  				return (exp_matrix)
			}else{
				return (NULL)
				}
		},

		CMSyntaxChecker = function(group1, group2){
			if (self$cleanSyntax(group1) & self$cleanSyntax(group2)){
				return (TRUE)
			}else{
				return (FALSE)
			}
		},

		cleanSyntax = function(expr){
			results = grep(expr, self$validGroups)
			if (length(results > 0)){
				return (TRUE)
			}else{
				return (FALSE)
			}
		},

		countCharOccurrences = function(char, s){
			count = gsub(char, "", s)
			return (nchar(s) - nchar(count))
		},

		finalExpression = function(){
			if(self$validSyntax){
				return(self$designExpression)
			}else{
				return (NULL)
			}
		}
))