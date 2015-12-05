##############
#Check that the currently install version of R
#is at least the minimum required version
##############
R_min_version = "3.1.0"
R_version = paste0(R.Version()$major, ".", R.Version()$minor)

if(compareVersion(R_version, R_min_version) < 0){
	stop("You do not have the latest required.\n",
		"Launch will fail if you continue.\n",
		"Go to http://cran.r-project.org/ to update your version of R.")
}

######
#Install basic required packages if not available/installed
######
download_not_installed <- function(x){
	availpacks = .packages(all.available = TRUE)
	source("http://bioconductor.org/biocLite.R")
	missingPackages = x[!(x %in% availpacks)]
	message("The following packages were missing. Installation attempted...")
	message(missingPackages)
	if (length(missingPackages) > 0){
		for (i in missingPackages){
			message("Installing", i, "package using biocLite...\n")
			biocLite(i)
		}
	}
}

vanilla_install_packages <- c("limma",
							'markdown',
							'shiny',
							'shinyjs',
							'shinyBS'
							'scales',
							'markdown',
							'ggplot2',
							'png',
							'vsn',
							'RUnit')
download_not_installed(vanilla_install_packages)

###################################
#Should use latest version of shiny
###################################
shiny_okay <- FALSE
if ("shiny" %in% .packages(all.available = TRUE)){
	shiny_min_version = "0.11"
	shiny_compare = compareVersion(as.character(packageVersion("shiny")), shiny_min_version)
	if (shiny_compare >= 0){
		shiny_okay <- TRUE
	}
}

if(!shiny_okay){
	install.packages('devtools')
	devtools::install_github("rstudio/shiny")
}