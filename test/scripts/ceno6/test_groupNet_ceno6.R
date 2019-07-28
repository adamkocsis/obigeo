context("ceno6 - groupNet() basic")

# preparation
	testPath <- "package/test/results/ceno6/groupNet_ceno6/"

	# testing the single slice partitionings
		cell <- "icos"
		tax <- "trinomen"
		ocq <- 10
	
	# 1. create a contingency table for the data
		contingency<-table(ceno6[,tax], ceno6[,cell])
		class(contingency)<-"matrix"
		# minimum occurrence quota omission
		tCells<-table(ceno6[,cell])
		keepCells <-names(tCells)[tCells>=ocq]
		contingency <- contingency[,keepCells]
		# omit taxa that are not present
		contingency <- contingency[apply(contingency, 1, sum)!=0,]



# Assignment phase
assignList<-list()

# test assignment
title <- "unipartite_infomap"
assignList$new<-expression(

	groupNet(contingency, bipartite=F, method="infomap")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "unipartite_louvain"
assignList$new<-expression(

	groupNet(contingency, bipartite=F, method="louvain")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "bipartite_infomap"
assignList$new<-expression(

	groupNet(contingency, bipartite=T, method="infomap")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "bipartite_infomap_taxonoutput"
assignList$new<-expression(

	groupNet(contingency, bipartite=T, method="infomap", onlyloc=FALSE)

)
names(assignList)[names(assignList)=="new"]<- title


test_that("NULL output",{
	expect_is(groupNet(contingency, bipartite=F, method=NULL), "igraph")
	expect_is(groupNet(contingency, bipartite=T, method=NULL), "igraph")
})


# test assignment
title <- "unipartite_infomap_sampcorr_occ"
assignList$new<-expression(

	groupNet(contingency, bipartite=F, method="infomap", sampcorr="occ")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_infomap_sampcorr_dom"
assignList$new<-expression(

	groupNet(contingency, bipartite=F, method="infomap", sampcorr="dom")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "unipartite_infomap_sampcorr_coll"
assignList$new<-expression(

	groupNet(contingency, bipartite=F, method="infomap", sampcorr="dom")

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