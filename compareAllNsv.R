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

removeLargerThan20km2=T

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

#identify all points withn nsv watershed
print("r.water.outlet...")
execGRASS("r.water.outlet",input="flowDir",output="above_target",coordinates=as.numeric(c(nsv_out$X,nsv_out$Y)),flags=c("overwrite","quiet"))
print("r.to.vect...")
execGRASS("r.to.vect", input="above_target",output="above_target_vect",type="area", flags=c("overwrite","quiet"))
print("v.select...")
execGRASS("v.select", ainput="nearPoints",binput="above_target_vect",output="nearPoints_upstream", flags=c("overwrite","quiet"))

execGRASS("v.in.ogr",input="C:/Users/Sam/Documents/LeakyRivers/Data/sites/allJamSurveyAreas_2.shp",output="jamSurveyExtent",flags="quiet")
execGRASS("v.select", ainput="nearPoints_upstream",binput="jamSurveyExtent",output="points_surveyArea",flags="quiet")
execGRASS("v.db.addcolumn",map="points_surveyArea",columns="inSurvey")
execGRASS("v.db.update",map="points_surveyArea",column="inSurvey",value="TRUE")

execGRASS("v.select", ainput="nearPoints_upstream",binput="jamSurveyExtent",output="points_notSurveyArea",flags=c("r","quiet"))
execGRASS("v.db.addcolumn",map="points_notSurveyArea",columns="inSurvey")
execGRASS("v.db.update",map="points_notSurveyArea",column="inSurvey",value="FALSE")

#read attribute table back to R
print("read to R")
all_nsv=rbind(grassTableToDF(execGRASS("v.db.select",map="points_surveyArea",intern = T)),
              grassTableToDF(execGRASS("v.db.select",map="points_notSurveyArea",intern = T)))

all_nsv[,1:3]=data.frame(sapply(all_nsv[,1:3], function(x) as.numeric(as.character(x))))

all_nsv_coords = grassTableToDF( execGRASS("v.report",map="nearPoints_upstream",option="coor",intern=T) )[,c("PointIDX","x","y")]
all_nsv_coords=data.frame(sapply(all_nsv_coords[,1:3], function(x) as.numeric(as.character(x))))

getVarFromDF=function(PointIDX,DataTypeIDX,makeNumeric=T,thisDF=all_nsv){
  if(makeNumeric){
    return(mean(as.numeric(as.character(thisDF[thisDF$PointIDX==PointIDX & thisDF$DataTypeIDX==DataTypeIDX,"Value"])),na.rm=T))
  } else {
    if(length(thisDF[thisDF$PointIDX==PointIDX & thisDF$DataTypeIDX==DataTypeIDX,"Value"])==1){
      return(thisDF[thisDF$PointIDX==PointIDX & thisDF$DataTypeIDX==DataTypeIDX,"Value"])
    } else {
      return(NA)
      }
  }
}

allNsvDF=data.frame(PointIDX=unique(all_nsv$PointIDX))
allNsvDF$jamCount=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=28)

allNsvDF$slope_gis=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=10)
allNsvDF$elevRange25=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=9)
allNsvDF$latRange10=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=12)
allNsvDF$latRange25=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=13)
allNsvDF$latRange50=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=14)
allNsvDF$elevation=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=11)
allNsvDF$UAA=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=15)
allNsvDF$SPI=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=16)

allNsvDF$landMgmt=sapply(allNsvDF$PointIDX, getVarFromDF, DataTypeIDX=26, makeNumeric=F)

allNsvDF = left_join(allNsvDF,all_nsv[,c("PointIDX","inSurvey")],by=c("PointIDX"="PointIDX"))
allNsvDF$inSurvey=as.logical(allNsvDF$inSurvey)

allNsvDF=left_join(allNsvDF,all_nsv_coords)

if(removeLargerThan20km2=T){

uaa=raster::raster("C:/Users/Sam/Documents/R Projects/BuildLeakyDB/flowAccum_xxl.tif")
allNsvDF$under20km2=raster::extract(x=uaa,y=allNsvDF[,c("x","y")])*(9.118818^2)/(1000^2)<=20
allNsvDF=allNsvDF[allNsvDF$under20km2==T,]

allNsvDF$UAA=allNsvDF$UAA*(9.118818^2)
allNsvDF=allNsvDF[allNsvDF$UAA<=20*(1000^2),]
}
gc()
dbDisconnect()
write.csv(allNsvDF,"allNsvDF.csv")

allNsvDF=read.csv("allNsvDF.csv")


options(na.action=na.omit)
#check that all jams are in jam-surveyed areas:
boxplot(allNsvDF$jamCount~allNsvDF$inSurvey)



boxplot(allNsvDF$slope_gis~allNsvDF$inSurvey, main="slope")
t.test(allNsvDF$slope_gis~allNsvDF$inSurvey)

boxplot(allNsvDF$elevRange25~allNsvDF$inSurvey,main="elev range 25")
t.test(allNsvDF$elevRange25~allNsvDF$inSurvey)

boxplot(allNsvDF$latRange10~allNsvDF$inSurvey,main="lat range 10")
t.test(allNsvDF$latRange10~allNsvDF$inSurvey)

boxplot(allNsvDF$latRange25~allNsvDF$inSurvey, main = "lat range 25")
t.test(allNsvDF$latRange25~allNsvDF$inSurvey)

boxplot(allNsvDF$latRange50~allNsvDF$inSurvey, main = "lat range 50")
t.test(allNsvDF$latRange50~allNsvDF$inSurvey)

boxplot(allNsvDF$SPI~allNsvDF$inSurvey, main = "SPI")
t.test(allNsvDF$SPI~allNsvDF$inSurvey)

boxplot(allNsvDF$UAA~allNsvDF$inSurvey, main = "UAA")
t.test(allNsvDF$UAA~allNsvDF$inSurvey)

boxplot(allNsvDF$elevation~allNsvDF$inSurvey, main = "elevation")
t.test(allNsvDF$elevation~allNsvDF$inSurvey)

predictMe=allNsvDF[,c("inSurvey","slope_gis","latRange10","latRange25","UAA","SPI")]
predictMe=predictMe[complete.cases(predictMe),]
plot(predictMe)

options(na.action = na.fail)
m=glm(inSurvey~slope_gis*latRange25*UAA,data=predictMe,family=binomial)
aic=dredge(m,extra = "R^2", m.lim=c(1,31))
head(aic)
options(na.action=na.omit)

m=glm(inSurvey~slope_gis+latRange25+UAA+latRange25:slope_gis+latRange25:UAA+slope_gis:UAA,data=predictMe,family=binomial)
summary(m)
plot(m$fitted.values,m$data$inSurvey)
sum(m$fitted.values>0.5)


m=glm(inSurvey~(SPI+UAA+latRange25+slope_gis)^2,data=predictMe,family=binomial)
summary(m)
plot(m$fitted.values,m$data$inSurvey)
sum(m$fitted.values>0.5)

#hist(m$fitted.values)
