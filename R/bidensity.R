#' Rescaled bipartite density (BC) of Sidor et al. 2013
#' 
#' The proportion of manifested connections out of all potential connections.
#' 
#' The variables is called BC in Sidor et al. 2013 and is caclulated as:
#' BC = (O-N)/LN-N'
#' 
#' "where O is the number of links in the occurrence network (number of occurrences),
#'  N is the number of taxa, and L is the number of localities. The numerator is the number of occurrences of taxa beyond a single locality (hence why N is subtracted from O), and
#' the denominator is the number of occurrences that could conceivably
#' occur (LN), minus one occurrence for each species because
#' each species must occur at least once. This measure is
#' bounded between 0 (when O = N) and 1 (when O = LN), which
#' correspond to extreme occurrence network topologies of minimum
#' and maximum homogeneity"
#' 
#' Sidor, C. A., D. A. Vilhena, K. D. Angielczyk, A. K. Huttenlocker, S. J. Nesbitt, B. R. Peecook, J. S. Steyer, R. M. H. Smith, and L. A. Tsuji. 2013: Provincialization of terrestrial faunas following the end-Permian mass extinction. Proceedings of the National Academy of Sciences 110:8129-8133.
#' @param dat (\code{data.frame}) Occurrence dataset.
#' @param tax (\code{character}) Column name of taxon entries
#' @param loc (\code{character}) Column name of locality/cell entries.
#' @param bin (\code{character}) Column name of bin ids. Optional, if the function is to be iterated for multiple bins. 
#' @export
#' @examples
#' data(ceno6)
#' bidensity(ceno6, tax="trinomen", loc="icos", bin="stg")
bidensity <- function(dat, tax, loc, bin=NULL){
#	dat <- cambSp[cambSp$slc==5, ]
#	tax <- "species"
#	loc<- "cell5"
	
	# singlebin case
	if(is.null(bin)){
		primOcc <- unique(dat[, c(tax, loc)])
	
		O<- nrow(primOcc)
		N <- length(unique(primOcc[,tax]))
		L<- length(unique(primOcc[,loc]))
	
		return((O-N)/(N*L-N))

	# multibin case
	}else{
		allBin <-sort(unique(dat[,bin]))
		
		ret <- rep(NA, length(allBin))
		
		for(i in 1:length(allBin)){
			newDat <- dat[dat[,bin]==allBin[i],]

			# recursive 
			ret[i]<- bidensity(newDat, tax=tax, loc=loc, bin=NULL)
		}
		names(ret) <- allBin
		return(ret)
	}
}
