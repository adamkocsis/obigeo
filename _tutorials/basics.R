# Example script to use the package
# load the packages
library(icosa)
library(chronosphere)

# under development package
library(obigeo)

data(ceno6)

gr<- hexagrid(c(4,3), sf=T)

# basic examples using networks
	oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
	slicewiseC6 <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
	tracingC6 <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", tracing=TRUE)

	# to show how mpas are used
		land <- fetch("NaturalEarth", "land")
	
	# plot the results of a single partitioning
	bgplot(oneC6, map=land, icosa=gr, colors="col", labels="grouping")
	
	# for plotting the tracing results
	par(mfrow=c(3,1))
	allStage <- unique(tracingC6$stg)
	for(i in 1:length(allStage)){
		onePart <- tracingC6[tracingC6$stg==allStage[i], ]
		bgplot(onePart, map=land, icosa=gr, colors="col", labels="grouping", cell="icos")
	}



# basic example using cluster-analysis
	# calculates: jaccard distance
	# does clustering based on complete-linkage
	# cuts tree based on the kgs algorithm (selects cutting height automatically)
	oneClust <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="distance", dist="jaccard", method="average", plot=TRUE, kgs=TRUE)
	bgplot(map=land, oneClust, icosa=gr, colors="col", labels="grouping")

	# same with genera
	oneClust <- bgpart(ceno6,bin=NULL, tax="clgen", cell="icos", ocq=10, base="distance", dist="jaccard", method="average", plot=TRUE, kgs=TRUE)
	bgplot(map=land, oneClust, icosa=gr, colors="col", labels="grouping")

	# cut dendrogram to form 10 groups (h=0.96)
	oneClust <- bgpart(ceno6,bin=NULL, tax="clgen", cell="icos", ocq=10, base="distance", dist="jaccard", method="average", plot=TRUE, kgs=FALSE, h=0.96)
	bgplot(map=land, oneClust, icosa=gr, colors="col", labels="grouping")


	# tracing with clustering - 
	clusTrace <- bgpart(ceno6, tracing=TRUE, bin="stg", tax="trinomen", cell="icos", ocq=10, base="distance", dist="jaccard", method="average", plot=TRUE, kgs=TRUE)
	clusTrace <- bgpart(ceno6, tracing=TRUE, bin="stg", tax="clgen", cell="icos", ocq=10, base="distance", dist="jaccard", method="average", plot=TRUE, kgs=TRUE)
	
	# for plotting the tracing results
	par(mfrow=c(3,1))
	allStage <- unique(clusTrace$stg)
	for(i in 1:length(allStage)){
		onePart <- clusTrace[clusTrace$stg==allStage[i], ]
		bgplot(onePart, map=land, icosa=gr, colors="col", labels="grouping", cell="icos")
	}

