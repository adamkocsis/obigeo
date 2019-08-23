# load the packages
library(divDyn)
library(icosa)
library(obigeo)
library(restools)

if(getOS()=="windows") osPath <- "D:/"
if(getOS()=="linux") osPath <- "/media/adam/data/shared/"
workdir<-paste(osPath, "Dropbox/WorkSpace/2019-08-23_obigeo", sep="")
setwd(workdir)

data(ceno6)
gr<- hexagrid(c(4,3), sp=T)
gr2<- hexagrid(c(7), sp=T)

# look up coordinates in the cells
ceno6$icos <- locate(gr, ceno6[, c("paleolng", "paleolat")])
ceno6$icos2 <- locate(gr2, ceno6[, c("paleolng", "paleolat")])


# basic examples for development
	oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
	slicewiseC6 <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
	tracingC6 <- bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", tracing=TRUE)

	slicewiseW <- bgpart(whole,bin="stg", tax="trinomen", cell="icos2", ocq=10, base="network", method="infomap")
	tracingW <- bgpart(whole,bin="stg", tax="trinomen", cell="icos2", ocq=10, base="network", method="infomap", tracing=TRUE)

	# running the console application
	oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", console=TRUE,
		cpath="/media/adam/data/shared/Dropbox/Programs/Infomap_linux", cargs=NULL)
	oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", console=TRUE,
		cpath="D:\\Dropbox\\Programs\\Infomap_0.19.14\\win", cargs=NULL)

traceRes<-bgstats(tracingW, cell="icos2", bin="stg")



nGroups(one)
nGroups(slicewise)
nGroups(tracing)
nGroups(tracing, bin="stg")



# over the whole
	

nGroups(slicewise)
nGroups(tracing, bin="stg")


# still quite good


nSites(one)
nSites(one, omitted=FALSE)
nSites(slicewise)
nSites(slicewise, omitted=FALSE)


nGroups(tracing)
nSites(tracing, bin="stg")
nSites(tracing, bin="stg", omitted=FALSE) # not exactly the same as above


# cenozoic 6

# with a single partition
	bgsingle<-bgstats(oneC6,  bu="grouping")
	expect_true(is.null(dim(bgs)))

# cenozoic 6
	# tracing - nona
	bgstats(tracingC6, bin="stg", cell="icos", bu="grouping",noNAStart=TRUE)

	# tracing - nona
	bgstats(tracingC6, bin="stg", cell="icos", bu="grouping",noNAStart=FALSE)
	
	# slicewise - nona
	# tracing - nona
	bgstats(slicewiseC6,  bu="grouping",noNAStart=TRUE)

	# tracing - nona
	bgstats(slicewiseC6,  bu="grouping",noNAStart=FALSE)
	

# reproducing the by-cell turnover!
	# for phanerozoic
	
	traceWhole <- bgpart(whole,bin="stg", tax="trinomen", cell="icos2", ocq=10, base="network", method="infomap", tracing=TRUE)
	
	temp<- bgturnover(traceWhole, bin="stg", cell="icos2")
	tsplot(stages, boxes="per", shading="per")
	lines(stages$mid, temp)



# WHOLE - dynamic tests!
	# tracing- whole with NAs
	wholeTraceNA<-bgstats(traceWhole, bin="stg", cell="icos2", bu="grouping")
	# with
	wholeTrace<-bgstats(traceWhole, bin="stg", cell="icos2", bu="grouping",noNAStart=TRUE)
	
	
	# slicewise- whole with NAs
	wholeSWNA<-bgstats(slicewiseW, bu="grouping")
	# with
	wholeSW<-bgstats(slicewiseW,  bu="grouping",noNAStart=TRUE)




