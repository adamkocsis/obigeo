context("ceno6 - bgpart() multi-bin slicewise")

# preparation
	testPath <- "package/test/results/ceno6/bgpart_slicewise_ceno6/"


# Assignment phase
assignList<-list()

# test assignment
title <- "unipartite_infomap_slicewise"
assignList$new<-expression(

	bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "unipartite_infomap_slicewise_ocq100"
assignList$new<-expression(

	bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=100, base="network", method="infomap")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "bipartite_infomap_slicewise_ocq100"
assignList$new<-expression(

	bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=100, base="network", method="infomap", bipartite=TRUE)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_louvain_slicewise_ocq10"
assignList$new<-expression(

	bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="louvain", bipartite=FALSE)

)
names(assignList)[names(assignList)=="new"]<- title



# test assignment
title <- "unipartite_netcarto_slicewise_ocq10"
assignList$new<-expression(

	bgpart(ceno6,bin="stg", tax="trinomen", cell="icos", ocq=10, base="network", method="netcarto", bipartite=FALSE)

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



#	# for infomap - unipartite - with export
#	a<-bgpart(occs, tax="valid_name", cell="cells", cellQuota=20, network="unipartite", export="tempGraph.gexf")
#	
#	# for infomap - unipartite (console)
#	a<-bgpart(occs, tax="valid_name", cell="cells", cellQuota=20, implement="console", consoleArgs="-N 10", network="unipartite")
#	
#	# for the bipartite console
#	# to be written
#	
