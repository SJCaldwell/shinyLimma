
CMSyntaxChecker <- function(group1, group2){
	if (cleanSyntax(group1) & cleanSyntax(group2)){
		return (TRUE) 
	}else{
		return (FALSE)
	}
}

cleanSyntax <- function(tag){
	results <- (grep(tag, getValidGroups()))
	if (length(results > 0)){
	  return (TRUE)
	}else{
	  return (FALSE)
	}
	
}

#######################TO-DO######################################
#Use this to make sure both char have the same number of dots i.e. same splitting
#######################TO-DO######################################
countCharOccurrences <- function(char, s) {
    s2 <- gsub(char,"",s)
    return (nchar(s) - nchar(s2))
}