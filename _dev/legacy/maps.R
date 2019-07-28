
#' Order maps to match custom time scale
#' 
#' The function takes a list of dated maps, and reorganizes it to best match the ages provided in a vector.
#' 
#' @param maplist A named list of maps. The names argument must be convertible to numeric ages that will be used to search
#' 
#' @param ages (\code{numeric}) The vector of ages to order the maps.
#' 
#' @param rename (\code{logical}) Should the maps in the output list use original names or the entries provided in \code{ages}?
#' 
mapindex<-function(maplist, ages, rename=FALSE){
	mapID <- names(maplist)
	if(is.null(mapID)) stop("You need appropriate names - edit later!")
	mapAge <- as.numeric(mapID)
	if(any(is.na(mapAge))) warning("Non-convertible names are detected, assign ages to all maps!")

	newIndex <- rep(NA, length(ages))

	for(i in 1:length(ages)){
		absDiff <- abs(ages[i]-mapAge)
		# which is closest
		newIndex[i] <- which(min(absDiff)==absDiff)[1]
	}

	newList <- maplist[newIndex]
	if(rename) names(newList) <- ages

	return(newList)	

}



#' Function to load the PaleoMAP paleoraster series of C. Scotese
#' 
#' The current version reads the maps from a dedicated path. Future versions will download and unzip an archive, with multiple resolutions (these are 720*360).
#' 
#' @param path (\code{character}) Path to the raster files.
#' @export
getPaleoRasters<-function(path=NULL){
	workdir <- getwd()
	# the scotese maps/2
	if(!is.null(path)){
		setwd(path)
	}else{
		setwd("D:/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/paleorasters/files")
	}
	
	files<-list.files()
	
	listFiles <- strsplit(files, "\\.")
	bFiles<-lapply(listFiles, function(x){
		if(x[length(x)]=="jpg"){
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
	
	
	underSplit <- strsplit(no,"\\.")
	underSplit<- as.numeric(unlist(lapply(underSplit, function(x){
		x[1]
	})))
	
	paleoRasters <- list()
	for(i in 1:length(files)){
		x<-files[i]

		require(jpeg)
		require(raster)
		scot <- readJPEG(x)
		
		r<-raster(res=0.5)
		g<-raster(res=0.5)
		b<-raster(res=0.5)
		values(r)<-scot[,,1]*255
		values(g)<-scot[,,2]*255
		values(b)<-scot[,,3]*255
		RGBstack<-stack(r,g,b)

		paleoRasters[[i]] <- RGBstack
	}
	
	names(paleoRasters) <- underSplit
	
	paleoRasters<-paleoRasters[order(underSplit)]

	setwd(workdir)
	return(paleoRasters)
}



