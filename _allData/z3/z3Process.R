setwd("D:/Dropbox/WorkSpace/2018-12-04_provinciality/package/data/z3")

library(rgdal)
land<-readOGR(dsn="land_polygons_z3.shx", lay="land_polygons_z3")
crs<- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
land<-sp::spTransform(land, crs)



