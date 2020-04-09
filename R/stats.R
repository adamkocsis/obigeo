# The number of units in biogeographic partitions
# 
# The function returns the number of units 
# 
# @param bg Output of the \code{\link{bgpart}} function.
# 
# @param bin (\code{character}) Variable name of the time slice identifier.
# 
# @param bu (\code{character}) Variable name of grouping (biogeographic units).
# 
nGroups <- function(bg, bu="grouping", bin=NULL){
	if(is.list(bg) & !is.data.frame(bg)){
		nBioreg<-lapply(bg, function(x){
			biorTab2<-x[!is.na(x[,bu]),]
			length(levels(factor(biorTab2[, bu])))
		})
		nBioreg <- unlist(nBioreg)
	}else{
		if(is.null(bin)){
			if(is.vector(bg)){
				nBioreg<-length(levels(factor(bg)))
			}
			if(is.data.frame(bg)){
				nBioreg<-length(levels(factor(bg[,bu])))
			}
		}else{
		
			if(is.data.frame(bg)){
				biorTab2<-bg[!is.na(bg[,bu]),]
				# the number of cells used in the data
				nBioreg<-tapply(INDEX=biorTab2[, bin], X=biorTab2[,bu], function(x){
					length(levels(factor(x)))
				})
			}
		}
	}	
	return(nBioreg)
}
	


# The number of sampled localities in biogeographic partitionings
# 
# The function returns the number of sampled localities/cells/sites. 
# 
# @param bg Output of the \code{\link{bgpart}} function.
# 
# @param bin (\code{character}) Variable name of the time slice identifier.
# 
# @param bu (\code{character}) Variable name of grouping (biogeographic units).
#
# @param omitted (\code{logica}) Should the omitted (NA assignment) cells be counted?
# 
nSites <- function(bg, bu="grouping", bin=NULL, omitted=TRUE){
	if(is.null(bin)){
		if(is.vector(bg)){
			if(!omitted){
				bg<-bg[!is.na(bg)]
			}
			nBioreg<-length(bg)
		}
		if(is.data.frame(bg)){
			vect<-bg[,bu]
			if(!omitted){
				vect<-vect[!is.na(vect)]
			}
			nBioreg<-length(vect)
		}
	}else{
	
		if(is.data.frame(bg)){
			# the number of cells used in the data
			nBioreg<-tapply(INDEX=bg[, bin], X=bg[,bu], function(x){
				if(!omitted){
					x<-x[!is.na(x)]
				}
				length(x)
			})
		}
	}

	if(is.list(bg) & !is.data.frame(bg)){
		nBioreg<-lapply(bg, function(x){
			# recursion on time-slices
			nSites(x,bu=bu, bin=NULL, omitted=omitted)
		})
		nBioreg <- unlist(nBioreg)
	}
		
	return(nBioreg)
}
	



# Total (by-cell) biogeographic turnover of a 'tracing' partitioning output
# 
# The function outputs a time series of biogeographic turnover sensu Kocsis et al., 2018
# @section References: Kocsis, Á. T., Reddin, C. J. and Kiessling, W. 2018. The biogeographical imprint of mass extinctions. Proceedings of the Royal Society B 285:20180232. https://doi.org/10.1098/rspb.2018.0232
#
# @param bg Output of the \code{\link{bgpart}} function, with \code{tracing=TRUE}.
# 
# @param bin (\code{character}) Variable name of the time slice identifier.
# 
# @param bu (\code{character}) Variable name of grouping (biogeographic units).
#
# @param cell (\code{character}) Variable name of the spatial cells.
#
# @param mjc (\code{numeric}) Minimum number of jointly sampled cells betwen a pair of time slices.
bgturnover<-function(bg, bin, cell, bu="grouping", mjc=3){
	
	bioregTurn <- rep(NA,max(bg[,bin], na.rm=T))
	
	for(i in 2:length(bioregTurn)){

		# the time slice specific part
		subTab<-bg[bg[,bin]==i, ,drop=F]
		# subset of previous slice
		prevTab<-bg[bg[,bin]==i-1, ,drop=F]

		# there are more than 0 entries
		if(nrow(subTab)>0 & nrow(prevTab)>0){
			# bu vector (this)
			biorSub<-subTab[,bu]
			names(biorSub)<-subTab[,cell]
			biorSub<-biorSub[!is.na(biorSub)]
			
			# preivous vector (this)
			biorPrev<-prevTab[,bu]
			names(biorPrev)<-prevTab[,cell]
			biorPrev<-biorPrev[!is.na(biorPrev)]
			
			# cells that are present in both
			bothCell<-names(biorSub)[names(biorSub)%in%names(biorPrev)]
			
			if(length(bothCell)>=mjc){
				# among the jointly sampled cells, which have different assignments?
				bCompare <- biorSub[bothCell]!=biorPrev[bothCell]
				bioregTurn[i]<-sum(bCompare)/length(bCompare)
			
			}
			
		}
	
	}
	return(bioregTurn)
}



PIE <- function(grouping){

	N<-length(grouping)
	m<-as.numeric(table(grouping))
	
	N/(N-1)*(1-sum((m/N)^2))

}

nPIE <- function(bg, bu="grouping", bin=NULL){
	if(is.list(bg) & !is.data.frame(bg)){
		nPIEser<-lapply(bg, function(x){
			biorTab2<-x[!is.na(x[,bu]),]
			PIE(biorTab2[, bu])
		})
		nPIEser <- unlist(nPIEser)
	}else{
		if(is.null(bin)){
			if(is.vector(bg)){
				nPIEser<-PIE(bg)
			}
			if(is.data.frame(bg)){
				nPIEser<-PIE(bg[,bu])
			}
		}else{
		
			if(is.data.frame(bg)){
				biorTab2<-bg[!is.na(bg[,bu]),]
				# the number of cells used in the data
				nPIEser<-tapply(INDEX=biorTab2[, bin], X=biorTab2[,bu], function(x){
					PIE(x)
				})
			}
		}
	}
	
	return(nPIEser)
}
	

#' Statistics calculated from a biogeographic partitioning
#' 
#' This function will output various statistics of biogeographic partitioning files 
#' 
#' Description of variables.
#' @param bg Output of the \code{\link{bgpart}} function, with \code{tracing=TRUE}.
#' 
#' @param bin (\code{character}) Variable name of the time slice identifier.
#' 
#' @param bu (\code{character}) Variable name of grouping (biogeographic units).
#'
#' @param cell (\code{character}) Variable name of the spatial cells.
#'
#' @param mjc (\code{numeric}) Minimum number of jointly sampled cells betwen a pair of time slices. For slicewise results, this can be set to \code{NULL}, which will not return patterns of jointly sampled cells.
#' @param bb \code{logical} Should by-bioregion turnover be calculated (requires the divDyn package).
#' @param noNAStart (\code{logical}) Same as divDyn. 
#' @export
bgstats <- function(bg, cell, bin=NULL, bu="grouping",  mjc=3, bb=TRUE, noNAStart=FALSE){
# 	bg<-traceWhole
# 	cell<-"icos2"
# 	bin<- "stg"
# 	bu<-"grouping"
# 	mjc<-3
# 	bb<-TRUE


	cellsAll <- nSites(bg=bg, bu=bu, bin=bin, omitted=TRUE)
	cellsUsed <- nSites(bg=bg, bu=bu, bin=bin, omitted=FALSE)
	groups <- nGroups(bg=bg, bu=bu, bin=bin)
	PIE <- nPIE(bg=bg, bu=bu, bin=bin)

	if(!is.null(bin)){
		# integer binning
		maxBin <- max(bg[,bin], na.rm=T)
		cellsAll <- cellsAll[as.character(1:maxBin)]
		cellsUsed <- cellsUsed[as.character(1:maxBin)]
		PIE <- PIE[as.character(1:maxBin)]
		groups <- groups[as.character(1:maxBin)]
	}

	add <- c("groups","PIE", "cellsAll", "cellsUsed")

	# tracing result
	if(!is.null(bin) & is.data.frame(bg)){
		bycellTO <- bgturnover(bg=bg, cell=cell, bin=bin, bu=bu, mjc=mjc)

		# jointly sampled cells
		cellsJoint <- rep(NA,max(bg[,bin], na.rm=T))
			
		for(i in 2:length(cellsJoint)){
			# the time slice specific part
			subTab<-bg[bg[,bin]==i, ,drop=F]
			# subset of previous slice
			prevTab<-bg[bg[,bin]==i-1, ,drop=F]
	
			# there are more than 0 entries
			if(nrow(subTab)>0 & nrow(prevTab)>0){
				# bu vector (this)
				biorSub<-subTab[,bu]
				names(biorSub)<-subTab[,cell]
				biorSub<-biorSub[!is.na(biorSub)]
				
				# preivous vector (this)
				biorPrev<-prevTab[,bu]
				names(biorPrev)<-prevTab[,cell]
				biorPrev<-biorPrev[!is.na(biorPrev)]
				
				# cells that are present in both
				bothCell<-names(biorSub)[names(biorSub)%in%names(biorPrev)]
				
				cellsJoint[i] <- length(bothCell)
			}
		
		}


		if(bb){
			if(requireNamespace('divDyn', quietly=T)){
				bg2<-bg[!is.na(bg[,bu]),]
				ddBior<-divDyn::divDyn(bg2, tax=bu, bin=bin)

				disint<-ddBior[, "extProp"]
				emerge<-ddBior[, "oriProp"]

			}

		}

		add<- c(add,"cellsJoint","bycellTO", "disint", "emerge")
	}
	

	# slicewise result
	if(is.null(bin) & is.list(bg) & !is.data.frame(bg)){
		if(!is.null(mjc)){
			# jointly sampled cells
			cellsJoint <- rep(NA,length(bg), na.rm=T)
				
			for(i in 2:length(cellsJoint)){
				# the time slice specific part
				subTab<-bg[[i]]
				# subset of previous slice
				prevTab<-bg[[i-1]]
		
				# there are more than 0 entries
				if(nrow(subTab)>0 & nrow(prevTab)>0){
					# bu vector (this)
					biorSub<-subTab[,bu]
					names(biorSub)<-rownames(subTab)
					biorSub<-biorSub[!is.na(biorSub)]
					
					# preivous vector (this)
					biorPrev<-prevTab[,bu]
					names(biorPrev)<-rownames(prevTab)
					biorPrev<-biorPrev[!is.na(biorPrev)]
					
					# cells that are present in both
					bothCell<-names(biorSub)[names(biorSub)%in%names(biorPrev)]
					
					cellsJoint[i] <- length(bothCell)
				}
			
			}

			add<- c(add,"cellsJoint")
		}
	}

	# concantenate the final results
	end<-c()
	for(i in 1:length(add)){
		end<-cbind(end, get(add[i]))
	}

	# if the output is a simple vector
	if(dim(end)[1]==1){
		end<-as.numeric(end)
		names(end)<-add
	}else{
		if(!is.null(noNAStart)){
			if(!is.null(bin)){
				binRange<-range(bg[,bin], na.rm=T)
			}else{
				binRange<-range(as.numeric(names(bg)), na.rm=T)
				maxBin<-binRange[2]
			}

			# fill potential gaps with NA lines
			endMat<-matrix(NA, ncol=ncol(end), nrow=length(1:maxBin))
			rownames(endMat) <- 1:maxBin
			# omit NAs the beginning
			end<-end[!is.na(rownames(end)),]
			endMat[rownames(end),] <- end
				
			# should not be NAs at the start
			if(noNAStart){
				
				binSeq<- binRange[1]:binRange[2]
				end<- endMat[as.character(binSeq),]

			# NAs should be at the start
			}else{
				end<-endMat
			}
		}
	
		colnames(end)<-add
		end<-as.data.frame(end, stringsAsFactors=FALSE)
	}

	return(end)
}
