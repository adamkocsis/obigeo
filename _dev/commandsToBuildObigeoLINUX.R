# package building scripts
	setwd("/media/adam/data/shared/Dropbox/WorkSpace/2018-12-04_provinciality/obigeo")
	
	pkgbuild::compile_dll()
	devtools::document()
	
	# then remove the binaries
	system("rm src/init.o src/basic.o src/RcppExports.o src/obigeo.so")

	# 1. render vignettes
		# replace '' and –

		# a. manually render for testing and development (faster)
	#	rmarkdown::render("vignettes/handout.Rmd")

		# run all the tests!

		# b. use tools to 
		# vignette render for the archive (produces the .R file too, and index entries!)
	#	tools::buildVignettes(dir = '.', tangle=TRUE)

	# 2. run ghostscript compaction for the handout
	#	tools::compactPDF(paths="vignettes/handout.pdf", gs_quality = "ebook")

	# 3. copy the outputs of this to inst/doc (otherwise the windows binaries won't have the vignettes) 


	

######################################################################	
# OUTSIDE R -  BASH


cd /media/adam/data/shared/Dropbox/WorkSpace/2018-12-04_provinciality

# for building the vignettes and testing
sudo env PATH=$PATH R CMD INSTALL --clean obigeo 

sudo R CMD build --resave-data obigeo
