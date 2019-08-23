#################################################################################	
# used colors
	colorAll <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
	allHex<-gplots::col2hex(colorAll)		
	# red
	mainHex<-c("#F21414",
	
	# green
	"#07F80C",
	
	# blue
	"#0712F8",
	
	# yellow
	"#F8F500",
	
	# purple
	"#9E00F8",
	
	# orange
	"#FF9C00",
	
	# cyan
	"#00F0FF",
	
	# pink
	"#F00FE3",
	
	# brown
	"#744932",
	
	# grass
	"#A5EA21")
	set.seed(0)	
	allHex<-rep(c(mainHex, sample(allHex)),8)
	
	save(allHex, file="allHex.Rdata")
	