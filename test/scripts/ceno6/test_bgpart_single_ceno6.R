context("ceno6 - bgpart() single time-slice (bin=49)")

# preparation
	testPath <- "package/test/results/ceno6/bgpart_single_ceno6/"


# Assignment phase
assignList<-list()

# test assignment
title <- "unipartite_infomap"
assignList$new<-expression(

	bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "bipartite_infomap"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", bipartite=TRUE)

)
names(assignList)[names(assignList)=="new"]<- title

# test assignment
title <- "bipartite_infomap_with_taxa"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap", bipartite=TRUE, onlyloc=FALSE)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_louvain"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=10, base="network", method="louvain")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_infomap_ocq200"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=200, base="network", method="louvain")

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_infomap_ocq200_omittedFALSE"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=200, base="network", method="louvain", omitted=FALSE)

)
names(assignList)[names(assignList)=="new"]<- title


# test assignment
title <- "unipartite_netcarto"
assignList$new<-expression(

	bgpart(ceno6, tax="trinomen", cell="icos", ocq=200, base="network", method="netcarto", omitted=FALSE)

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



# context("bgplot() runs - single ceno6")
# 
# 
# # both grouping and color
# bgplot(first, map=dems[[94]], icosa=gr, fademap=0.5, labels="grouping", colors="col")
# 
# # only color
# bgplot(first, map=dems[[94]], icosa=gr, fademap=0.5,  colors="col")
# 
# # only label
# bgplot(first, map=dems[[94]], icosa=gr, fademap=0.5,  labels="grouping")
# 
# 
# bgplot(first, map=maps[[94]], icosa=gr, fademap=0.5)
# bgplot(first, map=land, icosa=gr, fademap=0.5)
# 
# bgplot(first, map=land, icosa=gr, fademap=0.5, mapArgs=list(col="gray65"))
# 
# 
# bgplot(first, map=maps[[94]], icosa=gr, fademap=0.5, border=NA)
# 
# 
# bgplot(first, map=maps[[94]], icosa=gr, alpha=0.8, fademap=0.5, border=NA, grid.args=list(border="gray25"))


