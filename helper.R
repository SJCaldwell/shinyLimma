
isnt.null <- function(x) !is.null(x)

getExploratoryPlot <- function(input, x){

	if(input == 1){
		return (NULL)
	}
	else if (input == 2){
        heatmap <- heatmapBuilder(x)
        return (heatmap)
	}
	else if (input == 3){
		boxplotted <- boxplotBuilder(x)
        return(boxplotted)
	}
}