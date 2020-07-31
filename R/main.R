# load colors
#	file<-system.file("data", "allHex.RData", package="obigeo")
#	load(file)


#' Biogeographic partitioning of occurrence data.
#' 
#' This function will produce bioregion/cell membership tables based on a variety of partitioning algorithms.
#' 
#' A lot stuff should come here!
#' 
#' @param dat \code{(data.frame)} Occurrence table.
#' 
#' @param tax \code{(character)} Variable name of the occurring taxa (variable type: \code{factor} or \code{character} - such as \code{"genus"}
#' 
#' @param bin \code{(character)} Variable name of the time bin numbers of the occurrences. This variable should be \code{numeric}.
#' 
#' @param ocq \code{numeric} The minimum occurrence/cell quota. Cells with a lower number of occurrences will be omitted from the analysis.
#' @param slq \code{numeric} Only used if bin is not NULL. The minimum occurrence/time slice quota. Time slices with a lower number of occurrences will be omitted from the analysis.
#' 
#' @param cell (\code{character}) The variable name of the cell, site or sample entries.
#' @param coll Used only for the 'network' approaches implemented in \code{\link{groupNet}}, and when bipartite is FALSE. The column name of the collection/sample identifiers. Setting this value to a valid column name will enforce a collection-based correction (Rojas et al., 2016), and will make 'sampcorr' argument irrelevant. 
#' 
#' @param feedback (\code{logical}) Should the functions output information about progress?
#' @param base (\code{character}) The basic type of the partitiong method. Currently only \code{"network"} or \code{"distance"}-based methods are provided, 
#' which are implemented in the \code{\link{groupNet}} and \code{\link{groupDist}} functions, respectively. 
#' 
#' @param cols \code{logical} Should colors be automatically assigned to each group?
#' @param ...  Arguments passed to the \code{\link{groupNet}} or \code{\link{groupDist}} functions.
#' 
#' @examples
#' # load example dataset
#' data(ceno6)
#' # single-slice partitioning (using infomap)
#' oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
#' 
#' # iteration of sing slice partitiongs for every stg, separately (using infomap)
#' slicewiseC6 <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
#' 
#' # tracing method, coarser resolution (using infomap)
#' tracing <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", tracing=TRUE)
#' 
#' # SEE THE bgplot() function for plotting!	
#' @export
bgpart <- function(dat,  tax, cell, bin=NULL,ocq=0, base="network", feedback=FALSE, slq=50, tracing=FALSE, coll=NULL,cols=TRUE,omitted=TRUE, ...){
	addArgs <- list(...)

	data(allHex)

	# rudimentary check
	if(nrow(dat)==0) stop("The supplied data.frame is empty")
	
	# A. if there is just one slice (e.g. OBIS data)
	if(is.null(bin)){
		# 1. create a contingency table for the data
			contingency<-table(dat[,tax], dat[,cell])
			class(contingency)<-"matrix"
	
			# minimum occurrence quota omission
			tCells<-table(dat[,cell])
			keepCells <-names(tCells)[tCells>=ocq]
			contingency <- contingency[,keepCells]
	
			# remove the original source from the memory to free up some space
			rm(dat)
			gc()

			# omit taxa that are not present
			contingency <- contingency[apply(contingency, 1, sum)!=0,]
	
		# 2. base-dependent continuation:
		if(base=="network"){
			# sampcorr is missing!- collection passing
			output <- groupNet(contingency, feedback=feedback, ...)
			
			# in csae the bipartite network returns taxa
			if(is.list(output) & !is.data.frame(output) & !("igraph"%in%class(output))){
				taxa<-output[[2]]
				output<-output[[1]]
			}else{
				taxa<- NULL
			}
		}
		if(base=="distance"){
			output <- groupDist(contingency, feedback=feedback, ...)

			taxa <-NULL
		}

		#add omitted, but sampled cells to the final
		if(omitted){
			if(is.vector(output)){
				output<-output[names(tCells)]
				names(output)<- names(tCells)
			}
			if(is.data.frame(output)){
				out <- matrix(nrow=length(tCells), ncol=ncol(output))
				colnames(out) <- colnames(output)
				rownames(out) <- names(tCells)
				out[rownames(output),] <- as.matrix(output)
				output <- out
			}
		}
	
	# multiple bins
	}else{
		# # slice quota
			tSlice<-table(dat[,bin])
			keepSlice<-as.numeric(names(tSlice[tSlice>=slq]))

			if(any(!unique(dat[,bin])%in%keepSlice)){
				# avoid, if possible!
				dat<-dat[dat[,bin]%in%keepSlice, ]
			}
			
		if(tracing){
			# create spatiotemporal cells
			sep<-"_"
			dat$stc<-paste(dat[, bin], sep,dat[,cell], sep="")

			if(feedback) fb("Calculating adjacency matrix.")
			
			# get the occurences per slice
			tCells<-table(dat$stc)
			keepCells<-names(tCells[tCells>=ocq])
			
			# get the things above the minimum occurrence quota
			datSub <- dat[dat$stc%in%keepCells, ]

			# correction by collection number needs to be calculated now!
			if(!is.null(coll)){
				addArgs$sampcorr<- unlist(tapply(X=datSub[, coll], INDEX=datSub$stc, function(x) length(unique(x))))
			}

			# contingency table
			contingency<-table(datSub[,tax], datSub$stc)
			class(contingency) <- "matrix"
			
			if(feedback) fb("Calling base-specific methods.")

			if(base=="network"){
				callArgs <- list(
					contingency=contingency,
					feedback=feedback
				)

				callArgs <- c(callArgs, addArgs)

				output <- do.call(groupNet, callArgs)

				# in case the bipartite network returns taxa
				if(is.list(output) & !("igraph"%in%class(output))){
					taxa<-output[[2]]
					output<-output[[1]]
				}else{
					taxa<- NULL
				}
	
				#if the method was set, and something is output
				if(!"igraph"%in%class(output)){	
					
					# if vector, promote to data.frame
					if(is.null(dim(output))){
						# force deep copy
						out1 <- as.numeric(output)
						nam <- names(output)
						output <- data.frame(grouping=out1)
						rownames(output) <- nam
					}
				
					# should the missing values be here?
					if(omitted){
						# this was buggy in R: above it had problems making this vector a data.frame
						# I had to repeat it here for no apparent reason.
						# and then out-of-boudns subset was not working fine. 
						# added forcing of copy above
		#				output <- output[names(tCells),]
		#				if(is.null(dim(output))){
		#					output <- data.frame(grouping=output)
		#				}
		#				rownames(output)<- names(tCells)

						# MANUAL HARD COPY +extension- slower but reliable
						newDF<-data.frame(nrow=length(tCells))
						for(i in 1:ncol(output)){
							column <- rep(NA, length(tCells))
							names(column) <-names(tCells)
							column[rownames(output)] <- output[,i]
							newDF<-cbind(newDF, column)
						}
						newDF$nrow <- NULL
						colnames(newDF) <- colnames(output)
						output <- newDF
					}

					# decompose stc to bin and cell
					stcEntries<-rownames(output)
					listStc<-strsplit(stcEntries, sep)
					resDat <- data.frame(
						bin=as.numeric(unlist(lapply(listStc,function(x)x[1]))),
						cell=as.character(unlist(lapply(listStc,function(x)x[2]))),
						stringsAsFactors=FALSE)
					colnames(resDat)[1:2]<-c(bin, cell)

					if(feedback) fb("Summing up.")
	
					output<-cbind(resDat, output)
				}
			# distance case
			}else{
			
				callArgs <- list(
					contingency=contingency,
					feedback=feedback
					)

				callArgs <- c(callArgs, addArgs)
					
				# the partitioning already run
				output <- do.call(groupDist, callArgs)

				# if vector, promote to data.frame
				if(is.null(dim(output))){
							
					output <- data.frame(grouping=output)
				}
				
				# should the missing values be here?
				if(omitted){
					output <- output[names(tCells),]
					if(is.null(dim(output))){
						output <- data.frame(grouping=output)
					}
					rownames(output)<- names(tCells)
				}

				# decompose stc to bin and cell
				stcEntries<-rownames(output)
				listStc<-strsplit(stcEntries, sep)
				resDat <- data.frame(
					bin=as.numeric(unlist(lapply(listStc,function(x)x[1]))),
					cell=as.character(unlist(lapply(listStc,function(x)x[2]))),
					stringsAsFactors=FALSE)
				colnames(resDat)[1:2]<-c(bin, cell)

				if(feedback) fb("Summing up.")
	
				output<-cbind(resDat, output)
				taxa <- NULL
			}		
		# bin-wise partitioning
		}else{
			# recursive iteration, with bin=NULL
			rows<-1:nrow(dat)
			callArgs <- list(
				tax=tax,
				cell=cell,
				bin=NULL,
				ocq=ocq,
				tracing=tracing,
				feedback=feedback,
				base=base
				)
			callArgs<-c(callArgs,addArgs)

			output <- tapply(rows, dat[,bin], function(y){
				# rewrite this all the time
				callArgs$dat <- dat[y,]

				# recursive call
				slicePart<-do.call(bgpart,callArgs)	
				
				# return value
				return(slicePart)

			})

			taxa <- NULL
		}

	}
	if(cols){
		if(is.vector(output)){
			colors<- allHex[output]
			output<-data.frame(grouping=output, col=colors, stringsAsFactors=FALSE)
		}
		if(is.data.frame(output)){
			output$col<- allHex[output$grouping]
		}
	}

	if(!is.null(taxa)){
		output <- list(output, taxa)
		names(output)<-c(cell, tax)
	}

	return(output)
}
