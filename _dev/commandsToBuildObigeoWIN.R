# package building scripts
	setwd("D:/Dropbox/WorkSpace/2019-08-23_obigeo/obigeo")
	pkgbuild::compile_dll()
	devtools::document()
	
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


	# the other thing, maybe add it later
		# the phanerozoic analysis for the paper
	#	rmarkdown::render("D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/doc/0.3/dd_phanero.Rmd")
		
		# ghostscript compaction for the vignette
	#	tools::compactPDF(paths="D:/Dropbox/WorkSpace/2017-04-05_divDyn/ddPhanero/doc/0.3/dd_phanero.pdf", gs_quality = "ebook")

	

######################################################################	
# OUTSIDE R -  windows shell

D:

cd D:\Dropbox\WorkSpace\2019-08-23_obigeo\

rem # for building the vignettes and testing
R CMD INSTALL --clean obigeo


R CMD build --resave-data obigeo


R CMD INSTALL --clean --build obigeo
