getExploratoryPlot <- function(input, x, targets){

	if(input == 1){
		return (NULL)
	}
	else if (input == 2){
        heatmap <- heatmapBuilder(x, targets)
        return (heatmap)
	}
	else if (input == 3){
		boxplotted <- boxplotBuilder(x)
        return(boxplotted)
	}
}