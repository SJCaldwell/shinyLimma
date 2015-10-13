
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

countCharOccurrences <- function(char, s) {
    s2 <- gsub(char,"",s)
    return (nchar(s) - nchar(s2))
}