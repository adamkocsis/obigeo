
#' Plotting of a biogeographic partitioning
#' 
#' The function takes a single time slice from a grouping output and plots a geographic rendering. 
#' 
#' Option B. coorindates go to bdat, triggering 
#' 
#' @param bdat \code{data.frame} Biogeographic partitioning.
#'
#' @param map A map object, plottable with \code{\link{mapplot}}.
#' 
#' @param colors \code{character} Variable name of the colors.
#'
#' @param labels \code{character} Variable name of the labels.
#' 
#' @param cell \code{character} Where is the cell information? "rownames" or column name.
#' 
#' @param circles \code{logical} Should the Equator and the 30 degree latitudinal circles be plotted?
#' 
#' @param icosa \code{hexagrid} or \code{trigrid} Optional argument, a grid from the icosa package (if cells of such grid are used). Otherise the biogeographic regions will be indicated with circles (not yet?).
#' 
#' @param xlim \code{numeric} the standard argument of plot() - not tested yet!
#' 
#' @param ylim \code{numeric} the standard argument of plot() - not tested yet!
#' 
#' @param xlab \code{character} the standard argument of plot() - not tested yet!
#' 
#' @param ylab \code{character} the standard argument of plot() - not tested yet!
#' 
#' @param alpha \code{numeric} Numeric value between 0 and 1 indicating the opacity of the partitioning.
#' 
#' @param fademap \code{numeric} Numeric value between 0 and 1 indicating the opacity of the map.
#' 
#' @param map.args \code{list} Additional arguments passed to the \code{\link{mapplot}} function to render the maps.
#' 
#' @param between \code{expression} code to be executed between the map drawing and the plotting of the partitioning.
#' 
#' @param grid.args \code{logical} Additional arguments to plot the grid itself. 
#'
#' @param lab.args \code{logical} Additional arguments to plot the labels with the text function. 
#'
#' @export
bgplot<-function(bdat, map=NULL, lng=NULL, lat=NULL,  colors=NULL, labels=NULL, cell="rownames", circles=TRUE, icosa=NULL,
	xlim=c(-180, 180), ylim=c(-90, 90), xlab="longitude", ylab="latitude", alpha=0.5, 
	between=NULL, fademap=0.5,map.args=NULL, border="gray50", grid.args=NULL, lab.args=NULL){


	
# bdat<-first
# map<-maps[[94]]
# cell<-"rownames"
# lab<-"grouping"
# over<-expression()
# circles <- FALSE
# icosa <- "gr"
# xlim=c(-180, 180)
# xlab="longitude"
# ylab="latitude"
# ylim=c(-90, 90)
# icosa<- gr
# colors<-"col"
# alpha<- 0.5
# map.args <- list()
# between <- NULL
	

	# at least a color or a label is required
	if(is.null(colors) & is.null(labels)) stop("Please provide at least a color or a label variable. ")

	# run under including setup
	plot(NULL, NULL,xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i")
	# plot maps
	map.args$map <- map
	map.args$add <- TRUE
	if(!any("legend"==names(map.args))) map.args$legend <- FALSE
	if(!is.null(map)) do.call(mapplot, map.args)

	if(is.finite(fademap)){
		fade<-paste("#FFFFFF", sprintf("%x", round(fademap*255)), sep="")
		rect(xleft=-180, xright=180, ytop=90,ybottom=-90, col=fade)
	}

	if(!is.null(between)){
		eval(between)
	}


	# plot bioregions
	if(!is.null(icosa)){
		if(requireNamespace("icosa", quietly = TRUE)){
			if("rownames"==cell){
				cellVect <- rownames(bdat)
			}
			if(any(cell%in%colnames(bdat))){
				cellVect <- bdat[, cell]
			}

			# plot clors
			if(!is.null(colors)){
				# append alpha values to bioregions
				usedColors <- bdat[,colors]
				# save NAs
				
				if(!is.null(alpha)){
					bNa <- is.na(usedColors)
					alpha<-sprintf("%x", round(alpha*255))
					usedColors<- paste(usedColors, alpha, sep="")
					usedColors[bNa]<-NA
				}

				theused<<- usedColors
				# subset the grid
				names(usedColors)<-cellVect
				# plot the grid
				icosa::plot(icosa, col=usedColors[rownames(icosa@faces)], add=T,border=NA)
			}

			if(!is.null(labels)){
				labVect<-bdat[,labels]

				#NAs
				labVect[is.na(labVect)] <- "N/A"

				coordlabs<-icosa::centers(icosa)[cellVect,, drop=FALSE]
			
				# the arguments
				lab.args$labels <- labVect
				lab.args$x <- coordlabs[,1]
				lab.args$y <- coordlabs[,2]
				if(!any("cex"==names(lab.args))) lab.args$cex <-0.6

				do.call(text, lab.args)
			}

	}



	}

	if(circles){
		segments(x0=-180, x1=180, y0=0,y1=0)
		segments(x0=-180, x1=180, y0=-60,y1=-60, lty=2)
		segments(x0=-180, x1=180, y0=-30,y1=-30, lty=2)
		segments(x0=-180, x1=180, y0=60,y1=60, lty=2)
		segments(x0=-180, x1=180, y0=30,y1=30, lty=2)
	}

	# run over
	if(!is.null(grid.args)){
		grid.args$x <- icosa
		grid.args$add <- TRUE
		do.call(plot,grid.args)
	}
}
