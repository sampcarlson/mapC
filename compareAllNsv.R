library(sp)
library(tidyverse)
library(rgdal)
library(RSQLite)
library(rgrass7)
library(PerformanceAnalytics)
library(MuMIn)
library(randomForest)
source("grassTools.r")

leakyDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\sqLiteDatabase\\LeakyData.db3")

#call init function
InitGrass_byRaster(rasterPath="C:/Users/Sam/Documents/R Projects/BuildLeakyDB/flowDir_xxl.tif",grassRasterName = "flowDir")


############-----------build all nsv characteristics list#################

dbGetQuery(leakyDB,"select * from Networks")
nsv_out=dbGetQuery(leakyDB,"select * From Coordinates WHERE CoordinateIDX = 1")

dataTypes='%'
nearby=dbGetQuery(leakyDB,paste0("SELECT Points.PointIDX, Data.DataTypeIDX, Coordinates.X, Coordinates.Y, Data.Value FROM
                                   Coordinates LEFT JOIN 
                                       ( Reaches LEFT JOIN ( Data LEFT JOIN Points ON Data.PointIDX = Points.PointIDX ) ON Reaches.ReachIDX = Points.ReachIDX )
                                       ON Coordinates.CoordinateIDX = Reaches.CoordinateIDX
                                       WHERE Data.DataTypeIDX LIKE '",dataTypes,"'"))
names(nearby)[names(nearby)=="PointIDX:1"]="PointIDX"

writeVECT(SDF=SpatialPointsDataFrame(coords=nsv_out[,c("X","Y")],data=nsv_out[,c("Name","CoordinateIDX")]),
          vname="targetPoint",v.in.ogr_flags = c("o", "overwrite","quiet"))

pts=SpatialPointsDataFrame(coords=nearby[,c("X","Y")],data=nearby[,c("PointIDX","DataTypeIDX","Value")])
#GRASS is fussy about attribute names - clean them up
names(pts)=c("PointIDX","DataTypeIDX","Value")
writeVECT(SDF=pts,vname="nearPoints",v.in.ogr_flags = c("o", "overwrite", "quiet"))

#identify area upstream of ER point
print("r.water.outlet...")
execGRASS("r.water.outlet",input="flowDir",output="above_target",coordinates=as.numeric(c(nsv_out$X,nsv_out$Y)),flags=c("overwrite","quiet"))
print("r.to.vect...")
execGRASS("r.to.vect", input="above_target",output="above_target_vect",type="area", flags=c("overwrite","quiet"))
print("v.select...")
execGRASS("v.select", ainput="nearPoints",binput="above_target_vect",output="nearPoints_upstream", flags=c("overwrite","quiet"))
#read attribute table back to R
print("read to R")
all_nsv=grassTableToDF(execGRASS("v.db.select",map="nearPoints_upstream",intern = T))


dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

all_nsv_data=list(
  jamCount=all_nsv[all_nsv$DataTypeIDX==28,c("PointIDX","Value")],
  slope_gis=all_nsv[all_nsv$DataTypeIDX==10,c("PointIDX","Value")],
  elevRange25=all_nsv[all_nsv$DataTypeIDX==9,c("PointIDX","Value")],
  latRange10=all_nsv[all_nsv$DataTypeIDX==12,c("PointIDX","Value")],
  latRange25=all_nsv[all_nsv$DataTypeIDX==13,c("PointIDX","Value")],
  latRange50=all_nsv[all_nsv$DataTypeIDX==14,c("PointIDX","Value")],
  elevation=all_nsv[all_nsv$DataTypeIDX==11,c("PointIDX","Value")],
  UAA=all_nsv[all_nsv$DataTypeIDX==15,c("PointIDX","Value")],
  SPI=all_nsv[all_nsv$DataTypeIDX==16,c("PointIDX","Value")],
  landMgmt=all_nsv[all_nsv$DataTypeIDX==26,c("PointIDX","Value")]
)
all_nsv_data$jamCount$Value=as.numeric(all_nsv_data$jamCount$Value)
all_nsv_data$slope_gis$Value=as.numeric(all_nsv_data$slope_gis$Value)
all_nsv_data$elevRange25$Value=as.numeric(all_nsv_data$elevRange25$Value)
all_nsv_data$latRange10$Value=as.numeric(all_nsv_data$latRange10$Value)
all_nsv_data$latRange25$Value=as.numeric(all_nsv_data$latRange25$Value)
all_nsv_data$latRange50$Value=as.numeric(all_nsv_data$latRange50$Value)
all_nsv_data$elevation$Value=as.numeric(all_nsv_data$elevation$Value)
all_nsv_data$UAA$Value=as.numeric(all_nsv_data$UAA$Value)
all_nsv_data$SPI$Value=as.numeric(all_nsv_data$SPI$Value)



#############-------------build nsv jam surveyed areas df---------------
#run in findUpstream.R

#added pointID< filter based on all_nsv$pointIDX (created above)
nsv_jams_data=read.csv("jamCount_up_data.csv")
nsv_jams_data=nsv_jams_data[nsv_jams_data$pointIDX %in% all_nsv$PointIDX,]
##############-----------------compare------------------
boxplot(list(all=all_nsv_data$elevation$Value,jammed=nsv_jams_data$elevation),range=0)

boxplot(list(all=all_nsv_data$SPI$Value,jammed=nsv_jams_data$SPI))

boxplot(list(all=all_nsv_data$UAA$Value,jammed=nsv_jams_data$UAA),range=0)

boxplot(list(all=all_nsv_data$slope_gis$Value ,jammed=nsv_jams_data$slope_gis ),range=0)

boxplot(list(all=all_nsv_data$latRange10$Value ,jammed=nsv_jams_data$latRange_10 ),range=0)

boxplot(list(all=all_nsv_data$elevRange25$Value ,jammed=nsv_jams_data$elevRange_25 ),range=0)
