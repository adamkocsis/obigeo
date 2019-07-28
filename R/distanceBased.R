#' Single time slice compositional partitioning based on distance-based cluster analysis
#' 
#' This function calculates a distance matrix from the data and runs cluster analysis on it. The dendrogram is then cut to form higher level units.
#' 
#' @param contingency (\code{matrix}) Numeric matrix, species in rows, localities in columns. 
#' 
#' @param binary (\code{logical}) If set to \code{TRUE}, the contingency table will be reduced to a binary matrix before the distances are calculated.
#'
#' @param counts (\code{logical}) If \code{binary} is \code{FALSE}, should proportions be used, or counts?
#' 
#' @param dist (\code{character}) The distance calculation method, the \code{method} argument of the \code{\link[vegan]{vegdist}} function in vegan. 
#' 
#' @param method (\code{character}) The type of clustering used, the \code{method} argument of \code{\link[stats]{hclust}} function.
#' 
#' @param kgs (\code{logical}) Should the \code{\link[maptree]{kgs}} (\code{TRUE}) or the \code{\link[stats]{cutree}} (\code{FALSE}) function be used for cutting the trees?
#' 
#' @param plot (\code{logical}) Should the dendrogram be plotted?
#' 
#' @param ... functions passed to the \code{\link[stats]{cutree}} or the \code{\link[maptree]{kgs}} functions.
#' 
#' @export
groupDist <- function(contingency, binary=TRUE, dist="jaccard", method="ward.D2", counts=FALSE, kgs=FALSE, plot=FALSE, feedback=FALSE,  ...){

	# 1. changes to the contingency matrix
	if(binary){
		contingency[contingency>1]<-1
	}else{
		# change everything to proportions
		if(!counts){
			contingency <- apply(contingency, 2, function(x){
				x<-x/sum(x)
			})
		}
		# otherwise use the counts
	}
	#2. get a distance matrix
	if(feedback) fb("Compositional distances.")
	distMat<-vegan::vegdist(t(contingency), method=dist)

	#3. cluster analysis
	if(feedback) fb("Clustering.")
	clustObj<-stats::hclust(distMat, method=method)

	# 4. cut the dendrogram
	if(kgs){
		if(requireNamespace("kgs", quietly=TRUE)){
			penalty<-maptree::kgs(clustObj, distMat,...)
			ind<-which(penalty==min(penalty))
			grouping<-stats::cutree(clustObj, k=as.numeric(names(penalty))[ind])
		}else{
			stop("To use the argument kgs=TRUE, please install the 'maptree' package.")
		}	
	}else{
		grouping<-stats::cutree(clustObj,...)	
	} # end h

	# 5. plot the dendrogram if wanted
	if(plot){
		plot(clustObj)
	}

	# output grouping
	return(grouping)

}
