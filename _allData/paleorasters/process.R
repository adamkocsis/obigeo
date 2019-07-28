
#' Function to load the PaleoMAP paleoraster series of C. Scotese
#' 
#' The current version reads the maps from a dedicated path. Future versions will download and unzip an archive, with multiple resolutions (these are 720*360).
#' 
#' @export
getPaleoRasters<-function(){
	workdir <- getwd()
	# the scotese maps/2
	setwd("D:/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/paleorasters/files")

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


