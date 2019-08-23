# test setup
	osPath <- "D:/"
#	osPath <- "/media/kocsis/Data/"

	workdir<-paste(osPath, "Dropbox/WorkSpace/2018-12-04_provinciality", sep="")
	setwd(workdir)

	# load the packages
		library(divDyn)
		library(icosa)
		library(obigeo)
		library(testthat)
		
	# load data
		# get the necessary data
		load(file="Data/PaleoDB/formatted_2018-12-05.RData")
		
		# load phanerozoic stages
		data(stages)
	
		# paleoDEMs
		data(paleoDEM)
	
		# get rasters to be plotted
		paleoRasters <- getPaleoRasters()

	# gridding
		gr<- hexagrid(c(4,3), sp=T)
	
		# look up coordinates in the cells
		dat$icos <- locate(gr, dat[, c("paleolng", "paleolat")])
	
		# omit where not found
		dat<- dat[!is.na(dat$icos),]
	
		# designate spatio-temporal cells
		dat$stc <- paste(dat$stg, dat$icos, sep="_")	
	
	# process maps
		# match bin numbers to maps
		maps<-mapindex(paleoRasters, stages$mid)
		dems<-mapindex(paleoDEM, stages$mid)

	
	# omit all raws, where species information is unavailable
		dat <- dat[!is.na(dat$trinomen),]

	# total dataset
		whole <- dat

	# cenozoic 6
		ceno6<- whole[whole$bin==49,]
		ceno6<-ceno6[!is.na(ceno6$bin), ]

# testdat directory: cd D:\Dropbox\WorkSpace\2018-12-04_provinciality\package\test\results

# tests on the cenozoic 6
	test_dir("package/test/scripts/ceno6", reporter=SummaryReporter)

	test_file("package/test/scripts/ceno6/test_groupNet_ceno6.R", reporter=SummaryReporter)
	test_file("package/test/scripts/ceno6/test_groupDist_ceno6.R", reporter=SummaryReporter)
	test_file("package/test/scripts/ceno6/test_bgpart_single_ceno6.R", reporter=SummaryReporter)
	test_file("package/test/scripts/ceno6/test_bgpart_slicewise_ceno6.R", reporter=SummaryReporter)
	test_file("package/test/scripts/ceno6/test_bgpart_tracing_ceno6.R", reporter=SummaryReporter)


