runAssignments <- function(theList, path){
	runs <- length(theList)

	# execution
	for(i in 1:runs){
		
		testName <- names(theList)[i]

		setwd(workdir)
		
		filename<-paste(path,testName , sep="")
		
		# get rid of randomness?
		set.seed(i)
		test_that(testName,{
			expect_silent(result<<-eval(theList[[i]]))
			expect_known_output(result, filename, update=TRUE, print = TRUE)
		})
	}
} 
