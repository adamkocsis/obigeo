context("ceno6 - groupDist() basic")

# preparation
	testPath <- "package/test/results/ceno6/groupDist_ceno6/"

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
title <- "bray_wardD2_1"
assignList$new<-expression(

	groupDist(contingency, binary=FALSE, dist="bray", method="ward.D2", h=1, plot=F)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "binary_jaccard_complete_h0.99"
assignList$new<-expression(

	groupDist(contingency, binary=TRUE, dist="jaccard", method="complete", h=0.99, plot=F)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "binary_jaccard_complete_h0.80"
assignList$new<-expression(

	groupDist(contingency, binary=TRUE, dist="jaccard", method="complete", h=0.8, plot=F)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "euclidean_complete_h0.80"
assignList$new<-expression(

	groupDist(contingency, binary=FALSE, dist="euclidean", method="complete", h=0.1, plot=F)

)
names(assignList)[names(assignList)=="new"]<- title



# test assignment
title <- "jaccard_complete_kgs"
assignList$new<-expression(

	groupDist(contingency, binary=TRUE, dist="jaccard", method="complete", kgs=TRUE)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "ward_complete_kgs"
assignList$new<-expression(

	groupDist(contingency, binary=TRUE, dist="jaccard", method="complete", kgs=TRUE)

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
