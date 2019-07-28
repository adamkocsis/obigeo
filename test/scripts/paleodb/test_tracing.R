context("total tracing")

trace <- bgpart(orig, bin="stg", tax="trinomen", cell="icos", 
	base="network", method="infomap", tracing=T, ocq=20, bipartite=FALSE, colors=T)

bgplot(trace[trace$stg==93,], cell="icos", map=maps[[93]], icosa=gr ,colors="col", labels="grouping")

#higher cell sizes
bgplot(trace[trace$stg==93,], cell="icos", map=maps[[93]], icosa=gr ,colors="col", labels="grouping", lab.args=list(cex=0.9))



#higher cell sizes
bgplot(trace[trace$stg==50,], cell="icos", map=maps[[50]], icosa=gr ,colors="col", labels="grouping", lab.args=list(cex=0.9))
