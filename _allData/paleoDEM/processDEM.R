# the scotese maps/2
setwd("D:/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/paleoDEM/files")

library(raster)

files<-list.files()

listFiles <- strsplit(files, "\\.")
bFiles<-lapply(listFiles, function(x){
	if(x[length(x)]=="nc"){
		return(TRUE)
	}else{
		return(FALSE)
	}


})

bFiles<- unlist(bFiles)

files<-files[bFiles]


underSplit <- strsplit(files,"_")
underSplit<- unlist(lapply(underSplit, function(x){
	x[length(x)]
}))

no <- unlist(lapply(strsplit(underSplit, "Ma"), function(x) x[1]))


# read everything to memroy
allDEM<- list()

for(i in 1:length(files)){
	harddrive <- raster(files[i])

	rtemp <- raster()
	extent(rtemp) <-c(-180.5,180.5,-90.5,90.5)
	dim(rtemp) <- c(181,361)
	crs(rtemp)<-crs(harddrive)
	values(rtemp)<-values(harddrive)

	allDEM[[i]]<-rtemp
}

names(allDEM) <- no

paleoDEM <- allDEM

paleoDEM<-paleoDEM[names(paleoDEM)[order(as.numeric(names(paleoDEM)))]]

save(paleoDEM, file="paleoDEM.RData")
