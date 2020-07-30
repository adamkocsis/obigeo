#' Endemism of a set of localities
#' 
#' The function calculates either the proportion of endemic taxa at every locality or counts the number of endemic species
#' 
#' @param dat \code{(data.frame)} Occurrence table, continency (rows are taxa, columns are localities) table or a RasterStack.
#' @param tax \code{(character)} Variable name of the occurring taxa (variable type: \code{factor} or \code{character} - such as \code{"genus"}
#' @param loc \code{(character)} Name of the locality variable, for instance samples IDs, sites, regions or geographic cells. 
#' @param prop \code{(logical)} Set to \code{TRUE} for proportional endemism (default). Setting this to \code{FALSE} will output the the number of endemic species.
#' 
#' @export
#' @examples
#' # create a partitioning
#' data(ceno6)
#' oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
#' # add membership to every row based on the cells
#' ceno6$membership <- oneC6[ceno6$icos,"grouping"]
#' # endemism in regions
#' endemism(ceno6, tax="trinomen", loc="membership")
endemism <- function(dat, tax=NULL, loc=NULL, prop=TRUE){
	# not a raster stack
	if(is.data.frame(dat) | is.matrix(dat)){
		# contingency table - both tax and loc are null
		if(is.null(tax) & is.null(loc)){
			# convert to boolean
			dat[dat>1] <-1

			# total occupancy
			occup <- apply(dat,1,sum)

			te <- apply(dat,2, function(x){
				sum(x==occup)
			})
			if(prop){
				# total diversity
				te <- te/apply(dat, 2, sum)
			}
			return(te)
			
		# occurrence data file
		}else{
			# one of them is 
			if(is.null(tax) | is.null(loc)) stop("You have to specify both the 'tax' and the 'loc' arguments. ")
			
			# get rid of NAs
				dat <- dat[!is.na(dat[,tax]) & !is.na(dat[,loc]),]
			
			# the range of every species
				locVar <- factor(dat[,loc])
				taxVar <- factor(dat[,tax])
				
			# omit redundancy (unique can be super slow)
				ordLocTax<-order(locVar, taxVar)

				#do the ordering
				locVar<-locVar[ordLocTax]
				taxVar <- taxVar[ordLocTax]
		
				# get the index, where a combination changes
				index <- .Call("_obigeo_ChangeIndexTwo", locVar, taxVar)+1
				
				# omit the 'fine' information completely, conserve order: locality-first, taxon-second
				
				locVar <-locVar[index]
				taxVar <-taxVar[index]
				
			# the total range of the species
		
			# pass to Cpp function: locVar and taxVar
			# with index shift
			passTax <- as.numeric(taxVar)-1
			passLoc <- as.numeric(locVar)-1
			
			# invoke the cpp function
			te <- .Call("_obigeo_EndemicTable", passTax, passLoc, prop)
		
			# add names
			names(te) <- levels(locVar)
			return(te)
		}
	}
}

# cumulative endemism
