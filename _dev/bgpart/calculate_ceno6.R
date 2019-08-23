	
	# load the packages
	library(divDyn)
	library(icosa)
	library(obigeo)
	library(restools)
	
	if(getOS()=="windows") osPath <- "D:/"
	if(getOS()=="linux") osPath <- "/media/adam/data/shared/"

	workdir<-paste(osPath, "Dropbox/WorkSpace/2019-08-23_obigeo", sep="")
	setwd(workdir)

	# load data
		# get the necessary data
		load(file="obigeo/_allData/formatted_2018-12-05.RData")
		
		# load phanerozoic stages
		data(stages)
	
		# paleoDEMs
		data(paleoDEM)
	
		# get rasters to be plotted
		paleoRasters <- getPaleoRasters(path="/media/adam/data/shared/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/paleorasters/files")
		paleoRasters <- getPaleoRasters(path="D:/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/paleorasters/files")

	# process maps
		# match bin numbers to maps
		maps<-mapindex(paleoRasters, stages$mid)
		dems<-mapindex(paleoDEM, stages$mid)

	# gridding
		gr<- hexagrid(c(4,3), sp=T)
		gr2<- hexagrid(c(7), sp=T)
		
		# look up coordinates in the cells
		dat$icos <- locate(gr, dat[, c("paleolng", "paleolat")])
		dat$icos2 <- locate(gr2, dat[, c("paleolng", "paleolat")])
	
		# omit where not found
		dat<- dat[!is.na(dat$icos),]
	
		# designate spatio-temporal cells
		dat$stc <- paste(dat$stg, dat$icos, sep="_")	
	
	
	# omit all raws, where species information is unavailable
		dat <- dat[!is.na(dat$trinomen),]

	# total dataset
		whole <- dat

	# cenozoic 6
		cenozoic6<- whole[whole$bin==49,]
		ceno6<-cenozoic6[, c(
			"collection_no", 
			"early_interval",
			"late_interval",
			"group",
			"lat",
			"lng", 
			"paleolat", 
			"paleolng", 
			"clgen",
			"trinomen",
			"icos",
			"icos2",
			"stg",
			"stc"
			)]

		rownames(ceno6)<-NULL
	# write the package 
		save(ceno6, file="obigeo/data/ceno6.RData")




