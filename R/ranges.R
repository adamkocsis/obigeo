#' Range rarity metric of endemism
#' 
#' The function calculates the cumulated range rarity for every geographic cell given an occurrence table
#' 
#' The metric was originally impelmented by Selig et al., 2014 (Plos ONE). For every cell or locality (\code{loc} argument), 
#' the function sums up the inverse occupancy (1/occupancy) of every taxon that occurrs there. 
#' If available, the function will weigh each of these taxon/locality specific values by a fraction. 
#' As in Selig et al. (2014), this is the proportion of finer-scale occurrence-points (e.g. coordinates) in the locality (cell) compared to the maximum observed number of fine-scale occurrences in that particular locality (cell). 
#' In other words, entries in \code{loc} cluster entries in \code{fine}. The weights will be the number a taxon has compared to the maximum number observed in the whole dataset.
#' 
#' @param dat \code{data.frame or RasterStack} The occurrence table.
#' @param tax \code{character} Variable name of the taxon names.
#' @param loc \code{character} Variable name of the locality information (e.g. geographic cells). Range rarity will be output for every entry in this column.
#' @param fine \code{character} Variable name of the basic geographic information (optional) that is clustered to coarser units (in \code{loc}). 
#' 
#' @export
rangerarity <- function(dat, tax, loc, fine=NULL){

#	dat <- species
#	tax <- "SpeciesID"
#	loc <- "cell"
#	fine <- "CsquareCode"

#	if(class(dat)=="data.frame"){
	# copy the data and omit redundancy
	# omit the unnecessary information (jsut to make sure)

	# do the factorization now
	locVar<- factor(dat[,loc])
	levLoc <- levels(locVar)

	# make them integers
	locVar <- as.numeric(locVar)
	taxVar <- as.numeric(factor(dat[,tax]))
	if(!is.null(fine))  fineVar <- as.numeric(factor(dat[,fine]))



#	system.time({
#		stringRep <- paste(taxVar, locVar, fineVar, sep="_")
#		dupl<-!duplicated(stringRep)
#
#		taxVar<-dat[dupl,tax]
#		locVar<-dat[dupl,loc]
#		if(!is.null(fine)) fineVar<-dat[dupl,fine]
#
#	})
#
#	system.time({
	if(!is.null(fine)){
		ordThree <- order(locVar, taxVar, fineVar)
		locVar<- locVar[ordThree]
		taxVar<- taxVar[ordThree]
		fineVar<- fineVar[ordThree]

		index <- .Call("_obigeo_ChangeIndexThree", locVar, taxVar, fineVar)+1
		
		taxVar<-taxVar[index]
		locVar<-locVar[index]
		fineVar<-fineVar[index]
	}
#	})
#
#	rm(dat)
#	rm(stringRep)
#	rm(dupl)

	
	# 1. calculate the number of max fine per loc
	if(!is.null(fine)){
		# first get the order for the two variables
			ordLoFi <- order(locVar,fineVar)
			locOrd <- locVar[ordLoFi]

		# call the function
			index <- .Call("_obigeo_ChangeIndexTwo", locOrd, fineVar[ordLoFi])+1

		#omit duplicates
			locLF <- locOrd[index]
		
		maxCell <- tabulate(locLF)
		names(maxCell) <- levLoc
	}
	# 2. number of fine per cell per taxon
	# order everything by locality and by taxon
		ordLocTax<-order(locVar, taxVar)

		#do the ordering
		locVar<-locVar[ordLocTax]
		taxVar <- taxVar[ordLocTax]

		# get the index, where a combination changes
		index <- .Call("_obigeo_ChangeIndexTwo", locVar, taxVar)+1
		
		# omit the 'fine' information completely, conserve order: locality-first, taxon-second
		
		locVar2 <-locVar[index]
		taxVar2 <-taxVar[index]
		
		if(!is.null(fine)){
			# the number of identical entries with a combination
			te<- diff(c(index, length(locVar)+1))
	
			# the proportions
			propLoc <- te/maxCell[locVar2]
		}else{
			propLoc <- rep(1, length(locVar2))
		}

	# 3. calculate occupancy for every taxon
		occup <- tabulate(taxVar2)

	# 4 calculate range rarity
		endRes <- .Call("_obigeo_RangeRarity", locVar2-1, taxVar2-1, propLoc, occup)

	# add names
		names(endRes) <- levLoc
		
	return(endRes)

#	}
#	if(class(dat)=="RasterStack") stop("not yet")

}
