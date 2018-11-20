library(sp)
library(tidyverse)
library(reshape2)
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

execGRASS("v.in.ogr",input="C:/Users/Sam/Documents/LeakyRivers/Data/sites/allJamSurveyAreas.shp",output="jamSurveyExtent",flags="quiet")
execGRASS("v.select", ainput="nearpoints_upstream",binput="jamSurveyExtent",output="points_surveyArea",flags="quiet")
execGRASS("v.db.addcolumn",map="points_surveyArea",columns="inSurvey")
execGRASS("v.db.update",map="points_surveyArea",column="inSurvey",value="TRUE")

execGRASS("v.select", ainput="nearpoints_upstream",binput="jamSurveyExtent",output="points_notSurveyArea",flags=c("r","quiet"))
execGRASS("v.db.addcolumn",map="points_notSurveyArea",columns="inSurvey")
execGRASS("v.db.update",map="points_notSurveyArea",column="inSurvey",value="FALSE")

#read attribute table back to R
print("read to R")
all_nsv=rbind(grassTableToDF(execGRASS("v.db.select",map="points_surveyArea",intern = T)),
              grassTableToDF(execGRASS("v.db.select",map="points_notSurveyArea",intern = T)))
all_nsv[,1:4]=data.frame(sapply(all_nsv[,1:3], function(x) as.numeric(as.character(x))))



dbGetQuery(leakyDB,"SELECT * FROM DataTypes")


getVarFromDF=function(PointIDX,DataTypeIDX,thisDF=all_nsv){
  return(mean(thisDF[thisDF$PointIDX==PointIDX & thisDF$DataTypeIDX==DataTypeIDX,"Value"],na.rm=T))
}

allNsvDF=data.frame(pointIDX=unique(all_nsv$PointIDX))
allNsvDF$jamCount=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=28)

allNsvDF$slope_gis=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=10)
allNsvDF$elevRange25=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=9)
allNsvDF$latRange10=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=12)
allNsvDF$latRange25=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=13)
allNsvDF$latRange50=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=14)
allNsvDF$elevation=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=11)
allNsvDF$UAA=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=15)
allNsvDF$SPI=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=16)
allNsvDF$landMgmt=sapply(allNsvDF$pointIDX, getVarFromDF, DataTypeIDX=26)

allNsvDF = left_join(allNsvDF,all_nsv[,c("PointIDX","inSurvey")],by=c("pointIDX"="PointIDX"))

boxplot(allNsvDF$SPI~allNsvDF$inSurvey)
boxplot(allNsvDF$SPI~allNsvDF$inSurvey)
