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
dbListTables(leakyDB)

#call init function
InitGrass_byRaster(rasterPath="C:/Users/Sam/Documents/R Projects/BuildLeakyDB/flowDir_xxl.tif",grassRasterName = "flowDir")


getValueFromResult=function(resultDF,dataTypeIDXs){
  if(sum(resultDF$DataTypeIDX %in% dataTypeIDXs)>=1){
    return( mean(resultDF$Value[resultDF$DataTypeIDX %in% dataTypeIDXs],na.rm = T))
  } else return(NA)
}

getCategoryFromResult=function(resultDF,dataTypeIDXs){
  if(sum(resultDF$DataTypeIDX %in% dataTypeIDXs)>=1){
    pointIDX=resultDF$PointIDX[resultDF$DataTypeIDX %in% dataTypeIDXs][1]
    dbGetQuery(leakyDB,paste0("SELECT VALUE FROM Data WHERE DataTypeIDX = '",dataTypeIDXs,"' AND PointIDX = '",pointIDX,"'"))$Value[1]
  } else return(NA)
}

dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

ER_data=dbGetQuery(leakyDB,"SELECT * From Data LEFT JOIN DataTypes ON Data.DataTypeIDX = DataTypes.DataTypeIDX WHERE Metric='ER'")

result=lapply(ER_data$PointIDX,getUpstream, range=1000, upOnly=T)
gc()

ER_data=data.frame(#ERPointIDX=ER_data$PointIDX,
  ER=sapply(result,getValueFromResult,dataTypeIDXs=24),
  wetWidth=sapply(result,getValueFromResult,dataTypeIDXs=c(1,2,3)),
  depArea_pct=sapply(result,getValueFromResult,dataTypeIDXs=c(7,8)),
  slope_gis=sapply(result,getValueFromResult,dataTypeIDXs=c(10)),
  slope_obs=sapply(result,getValueFromResult,dataTypeIDXs=c(17)),
  conf_obs=sapply(result,getValueFromResult,dataTypeIDXs=18),
  woodDepth=sapply(result,getValueFromResult,dataTypeIDXs=19),
  coarseDepth=sapply(result,getValueFromResult,dataTypeIDXs=20),
  fineDepth=sapply(result,getValueFromResult,dataTypeIDXs=21),
  POMDepth=sapply(result,getValueFromResult,dataTypeIDXs=22),
  Temperature=sapply(result,getValueFromResult,dataTypeIDXs=25),
  elevRange25=sapply(result,getValueFromResult,dataTypeIDXs=9),
  jamsPerKm=sapply(result,getValueFromResult,dataTypeIDXs=28),
  jamPoolsPerKm=sapply(result,getValueFromResult,dataTypeIDXs=29),
  latRange_10=sapply(result,getValueFromResult,dataTypeIDXs=12),
  latRange_25=sapply(result,getValueFromResult,dataTypeIDXs=13),
  latRange_50=sapply(result,getValueFromResult,dataTypeIDXs=14),
  landMgmt=sapply(result,getCategoryFromResult,dataTypeIDXs=26)
)
ER_data$managed=ER_data$landMgmt=="YM"
write.csv(ER_data,"ER_data.csv")

ER_data=read.csv("ER_data.csv")
ER_data=ER_data[complete.cases(ER_data$jamPoolsPerKm),]

ER_data$ER=-ER_data$ER
ER_data$logLogs=log10(ER_data$jamsPerKm+2)



plot(ER_data)

summary(lm(ER_data$ER~ER_data$jamsPerKm ))
lm_loglogjams=lm(ER_data$ER~ER_data$logLogs)
summary(lm_loglogjams)

par(mar=c(5,5,2,2))
plot(ER_data$logLogs,ER_data$ER,xlab="Log Logjams ( jam count / channel km)", ylab=expression(paste("Aerobic Respiration (mmol O"^2," m"^2, " day"^-1,")")), pch=19)


plot(ER_data$Temperature, lm_loglogjams$residuals)

summary(lm(ER_data$ER~ER_data$logLogs:ER_data$Temperature))

options(na.action = "na.fail")
dredge(glm(ER_data$ER~ER_data$jamsPerKm+ER_data$logLogs*ER_data$Temperature ),extra="R^2")
options(na.action = "na.omit")



jamCount_data=base::unique(dbGetQuery(leakyDB,"SELECT Data.DataTypeIDX, PointIDX, SourceIDX, Value From Data LEFT JOIN DataTypes ON Data.DataTypeIDX = DataTypes.DataTypeIDX WHERE Metric='Jams per km'"))


write.csv(dbGetQuery(leakyDB, paste0("SELECT Reaches.ReachName, Points.PointIDX, Coordinates.X, Coordinates.Y, Coordinates.CoordinateTypeIDX FROM Coordinates LEFT JOIN (
                           Reaches LEFT JOIN Points on Reaches.ReachIDX=Points.ReachIDX
                           ) ON Coordinates.CoordinateIDX = Reaches.CoordinateIDX WHERE PointIDX IN (",paste(jamCount_data$PointIDX,collapse = ', '),")")),
          "livers_reaches.csv")

jam_result=lapply(jamCount_data$PointIDX,getUpstream, range=500, upOnly=T)
gc()
dbGetQuery(leakyDB,"SELECT * FROM DataTypes")

jamCount_up_data=data.frame(
  pointIDX=jamCount_data$PointIDX,
  jamCount=sapply(jam_result,getValueFromResult,dataTypeIDXs=28),
  #wetWidth=sapply(jam_result,getValueFromResult,dataTypeIDXs=c(1,2,3)),
  #bankWidth=sapply(jam_result,getValueFromResult,dataTypeIDXs=c(4,5,6)),
  slope_gis=sapply(jam_result,getValueFromResult,dataTypeIDXs=c(10)),
  slope_obs=sapply(jam_result,getValueFromResult,dataTypeIDXs=c(17)),
  confinement=sapply(jam_result,getValueFromResult,dataTypeIDXs=18),
  elevRange_25=sapply(jam_result,getValueFromResult,dataTypeIDXs=9),
  latRange_10=sapply(jam_result,getValueFromResult,dataTypeIDXs=12),
  latRange_25=sapply(jam_result,getValueFromResult,dataTypeIDXs=13),
  latRange_50=sapply(jam_result,getValueFromResult,dataTypeIDXs=14),
  elevation=sapply(jam_result,getValueFromResult,dataTypeIDXs=11),
  UAA=sapply(jam_result,getValueFromResult,dataTypeIDXs=15),
  SPI=sapply(jam_result,getValueFromResult,dataTypeIDXs=16),
  landMgmt=sapply(jam_result,getCategoryFromResult,dataTypeIDXs=26)
)
jamCount_up_data$latRangePerUp=sapply(jamCount_data$PointIDX,getValueBreak,compareDataType=13)
jamCount_up_data$SPIHerePerUp=sapply(jamCount_data$PointIDX,getValueBreak,compareDataType=16)
#reclass land management as managed (true) or unmanaged (false)
jamCount_up_data$managed=jamCount_up_data$landMgmt=="YM"
write.csv(jamCount_up_data,"jamCount_up_data.csv")


jamCount_up_data=read.csv("jamCount_up_data.csv")

plot(jamCount_up_data)
chart.Correlation(jamCount_up_data[,c(2:11,13,14,1)])

glm(jamCount_up_data$jamCount~jamCount_up_data$managed)

glm(jamCount_up_data$jamCount~jamCount_up_data$slope_gis)

glm(jamCount_up_data$jamCount~jamCount_up_data$latRange_10*jamCount_up_data$managed)

glm(jamCount_up_data$jamCount~jamCount_up_data$latRange_10*jamCount_up_data$managed*jamCount_up_data$elevation^2)

jamCount_complete = jamCount_up_data[,names(jamCount_up_data)!="slope_obs"]

r=randomForest(jamCount ~ ., data = jamCount_complete,ntree=5000)
str(r)

j=jamCount_up_data
j$UAA=log10(j$UAA)
plot(j)

summary(lm(j$loglogs~j$slope_gis+j$managed))

options(na.action = "na.fail")
dredge(glm(j$jamCount~j$slope_gis*j$latRange_10*j$UAA*j$managed),extra="R^2")
options(na.action = "na.omit")

chart.Correlation(jamCount_by_mgmt)

slopeModel=summary(lm(jamCount_up_data$jamCount~jamCount_up_data$slope))
mgmtModel=summary(lm(jamCount_up_data$jamCount~jamCount_up_data$landMgmt))

summary(lm(jamCount_up_data$jamCount~jamCount_up_data$landMgmt+jamCount_up_data$slope))

jamCount_up_data$resid=mgmteModel$residuals



hist(jamCount_up_data$jamCount,breaks=c(0,5,10,15,20,25,30,35,40,45),main="")
hist(jamCount_up_data$jamCount[jamCount_up_data$managed],breaks=c(0,5,10,15,20,25,30,35,40,45),main="")
hist(jamCount_up_data$jamCount[!jamCount_up_data$managed],breaks=c(0,5,10,15,20,25,30,35,40,45),main="")
