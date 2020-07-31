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


#' Modularity in time-slice specific graph subsets
#' 
#' The function returns the modularity in an occurrence graph given a provided partitioning
#' 
#' @param x (\code{data.frame}) The output of the \code{\link{bgpart}} function.
#' @param graph (\code{igraph}) The occurrence graph, which can also be retrieved by \code{\link{bgpart}}.
#' @param bin (\code{character}) The column name of the bin identifier.
#' @examples
#' data(ceno6)
#' # 1. sinlge-slice partitioning
#' oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
#' # return the used graph
#' oneC6_graph <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method=NULL)
#' # modularity in this single slice
#' submodularity(oneC6, oneC6_graph)
#' 
#' # 2. multi-slice partitioning
#' trace <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", tracing=TRUE)
#' # return the used graph
#' trace_graph <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method=NULL, tracing=TRUE)
#' # modularity in this single slice
#' submodularity(trace, trace_graph, bin="stg")
# calculate slice-specific modularity
submodularity <- function(x, graph, bin=NULL){
	
	# only where there are groups
	tab <- x[!is.na(x$grouping),]

	# base case
	if(is.null(bin)){
		# get the subset for this 
		subgraph <- induced_subgraph(graph, rownames(tab))
		vertInGraph <- names(V(subgraph))

		# reorder table entries to this order
		# membership has to start with 1
		modVar <- as.numeric(factor(tab$grouping))
		names(modVar) <- rownames(tab)

		memb <- modVar[vertInGraph]
		modul <- modularity(subgraph, memb)
	
	# recursive case
	}else{

		# the number of vertices should be length(vect)
		different <- sort(unique(tab[, bin]))
	
		# output modularities
		modul <- rep(NA, length(different))
		
		# for every subset of the graph
		for(i in 1:length(different)){
			# slice-specific modularity
			sliceTab <- tab[tab[,bin]==different[i], ]
	
			# one-level recursion
			modul[i] <- submodularity(sliceTab, graph=graph, bin=NULL)
	
		}
		names(modul) <- different

	}
	return(modul)

}
