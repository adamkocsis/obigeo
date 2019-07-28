context("ceno6 - bgpart() multi-bin tracing")

# preparation
	testPath <- "package/test/results/ceno6/bgpart_tracing_ceno6/"



# Assignment phase
assignList<-list()

# test assignment
title <- "unipartite_infomap_tracing"
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="trinomen", ocq=10, cell="icos", base="network", method="infomap", bipartite=F, tracing=T)

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "unipartite_infomap_tracing_collCorrection" # this needs to be checked!!!
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="trinomen", ocq=10, cell="icos", base="network", method="infomap", bipartite=F, tracing=T, coll="collection_no")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "unipartite_infomap_tracing_occCorrection" # this needs to be checked!!!
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="trinomen", ocq=10, cell="icos", base="network", method="infomap", bipartite=F, tracing=T, sampcorr="occ")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "bipartite_infomap_tracing" # this needs to be checked!!!
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="trinomen", ocq=10, cell="icos", base="network", method="infomap", bipartite=T, tracing=T)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_infomap_tracing_genera"
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="clgen", ocq=10, cell="icos", base="network", method="infomap", bipartite=F, tracing=T)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_louvain_tracing_genera"
assignList$new<-expression(

	bgpart(ceno6, bin="stg", tax="clgen", ocq=10, cell="icos", base="network", method="louvain", bipartite=F, tracing=T)

)
names(assignList)[names(assignList)=="new"]<- title



# execution
for(i in 1:length(assignList)){
	
	testName <- names(assignList)[i]
	setwd(workdir)
	
	filename<-paste(testPath,testName , sep="")
	
	# get rid of randomness?
	set.seed(i)
	test_that(testName,{
		expect_silent(result<<-eval(assignList[[i]]))
		expect_known_output(result, filename, update=TRUE, print = TRUE)
	})
}


