setwd("/media/adam/data/shared/Dropbox/WorkSpace/2018-12-04_provinciality/package/obigeo")

library(obigeo)

dat <- matrix(c(
	"a","A",
	"b","A",
	"b","B",
	"c","A",
	"c","B",
	"d","B"
), ncol=2, byrow=TRUE)

dat <- as.data.frame(dat)
colnames(dat) <- c("trinomen", "icos")

tax <- "trinomen"
loc <- "icos"


data(ceno6)

# with the ceno6
dat <- ceno6

# code reproduce endemicity function with R code
endR <- function(dat, loc ,tax, prop){
	
	
	# copy the data and omit redundancy
		dat <- unique(dat[, c(tax, loc)])

	# get rid of NAs
		dat <- dat[!is.na(dat[,tax]) & !is.na(dat[,loc]),]
	
	# the range of every species
		locVect <- factor(dat[,loc])
		taxVect <- factor(dat[,tax])
			
	# index correlates to the total amount
		tem <- table(locVect, taxVect)
		class(tem) <- "matrix"
		at <-	apply(tem, 2, sum)
	

	tab <- table(dat[,loc])
	
	# endval
	result <- rep(NA, length(tab))
	
	names(result) <- rownames(tem)
	for(i in 1:nrow(tem)){
		result[i]<- sum(at==tem[i,])
	}
	if(prop){
		result <-result/apply(tem,1, sum)
	}
	return(result)
}

#########################################
# examples

c6 <- unique(ceno6[, c(loc, tax)])
t6 <- table(c6[,tax],c6[,loc])
class(t6)<-"matrix"

system.time(fs <- endemism(t6, prop=FALSE))



system.time(puR<<-endR(ceno6, loc="icos", tax="trinomen", prop=FALSE))
system.time(cpp<<-endemism(ceno6, loc="icos", tax="trinomen", prop=FALSE))

comp <- cbind(puR, cpp)


system.time(puR<<-endR(whole, loc="icos", tax="trinomen", prop=TRUE))
system.time(cpp<<-endemism(whole, loc="icos", tax="trinomen", prop=TRUE))

comp <- cbind(puR, cpp)

# single locality
whole$sing <- "a"

endemism(whole, loc="sing", tax="trinomen", prop=FALSE)


