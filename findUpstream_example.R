library(sp)
library(tidyverse)
library(rgdal)
library(RSQLite)
library(rgrass7)
source("grassTools.r")

leakyDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\sqLiteDatabase\\LeakyData.db3")
dbListTables(leakyDB)

#DB is incomplete, more data import to do.  Nonetheless, lots of data here:
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


#call init function
InitGrass_byRaster()

#r.watershed is a good enough reason alone to use GRASS
#this runs on ~10,000 km^2 at ~9x9 meter resolution
execGRASS("r.watershed",elevation="dem@PERMANENT",threshold=1000,drainage="flowDir",stream="streams_rast", flags=c("overwrite", "a"))

execGRASS("r.thin",input="streams_rast",output="streams_rast",flags="overwrite")

execGRASS("r.to.vect",input="streams_rast",output="streams_vect",type="line")
#execGRASS("r.to.vect")
execGRASS("g.list",type="all")



ER_data=dbGetQuery(leakyDB,"SELECT * From Data LEFT JOIN DataTypes ON Data.DataTypeIDX = DataTypes.DataTypeIDX WHERE Metric='ER'")

getUpstream(targetPointIDX=ER_data$PointIDX[2],range=1000)

#result=lapply(ER_data$PointIDX,getUpstream)

#other stuff:
# #easy to read directly to R
# localPoints=readVECT(vname="nearPoints_upstream")
# localPoints
# str(localPoints)
# plot(localPoints)
# 
# #r is slow
# streams=readVECT(vname="streams_vect")
# class(streams)
# 
# plot(streams)
# 
# #at some level, a line is an ordered set of cordinate pairs
# #you can dig all the way down to the raw coordinates if you want
# str(streams[1])
# streams@lines[[1]]@Lines[[1]]@coords
# streams=getHeadings(streams)
# head(streams@data)
# 
# #@data gives a normal data frame!
# head(streams@data)
# class(streams@data)
# 
# plot(localPoints)
# plot(streams,add=T)

#also easy to write to .shp, or .tif or whatever file w/ r.out.gdal or v.out.ogr