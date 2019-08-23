
fb <- function(char){
	# paste in as many spaces as necessary
	no<-80-nchar(char)
	val<-paste(c(char, rep(" " ,no),"\r"), collapse="")
	cat(val)
	flush.console()
	
}

getListElement<-function(li, n, unlist=T){
	res <- lapply(li, function(x) x[n])
	
	if(unlist){
		unlist(res)
	}
	return(res)
}
