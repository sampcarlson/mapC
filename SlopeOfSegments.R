require(sp)
require(raster)
require(rgdal)
require(rgrass7)
require(rgeos)
require(tidyverse)
library(RSQLite)
require(reshape2)

getCoordCount=function(subFeature){
  return(nrow(subFeature@Lines[[1]]@coords))
}


getEndpoints=function(multiLineFeature,dem=raster("~/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif")){
  endpointsHelper=function(segment,segID,dem){
    numCoords=nrow(segment@Lines[[1]]@coords)
    pointOne=cbind(segment@Lines[[1]]@coords[1,1],segment@Lines[[1]]@coords[1,2])
    pointTwo=cbind(segment@Lines[[1]]@coords[numCoords,1],segment@Lines[[1]]@coords[numCoords,2])
    if(raster::extract(dem,pointOne)>raster::extract(dem,pointTwo)){
      upPoint_x=pointOne[1]
      upPoint_y=pointOne[2]
      downPoint_x=pointTwo[1]
      downPoint_y=pointTwo[2]
    }else{
      downPoint_x=pointOne[1]
      downPoint_y=pointOne[2]
      upPoint_x=pointTwo[1]
      upPoint_y=pointTwo[2]
    }
    return(c(segID=segID,upPoint_x=upPoint_x,upPoint_y=upPoint_y,downPoint_x=downPoint_x,downPoint_y=downPoint_y))
  }
  
  #return(sapply(multiLineFeature@lines,endpointsHelper,dem=dem))
  
  return(mapply(endpointsHelper,multiLineFeature@lines,multiLineFeature$AUTO,MoreArgs=list(dem=dem),SIMPLIFY = T))
}



nsvNet=shapefile("~/R Projects/mapC/Spatial_Inputs/NSVStreamSegmentsIDX.shp")
#as compared to Q, the data is stored in attributes, and the attributes are stored as data
#field 'AUTO' is unique segment ID

#drop segments shorter than 2 coordinate pairs
nsvNet=subset(nsvNet,sapply(nsvNet@lines,getCoordCount)>1)

#nsvNet=addLengths(nsvNet) 
#Function (below) from rgeos works 
nsvNet$length=gLength(nsvNet,byid=T)

#add points every ~100 meters
#nsvPts=spsample(x=nsvNet,n=sum(nsvNet$length)/100,type="regular")





#load as SpatialGridDataFrame object
dem=readGDAL("~/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif")

#set up grass:
initGRASS("C:/Program Files/GRASS GIS 7.4.0",SG=dem,override=T)
writeRAST(dem,"dem")
writeVECT(nsvNet,vname="nsvNet",v.in.ogr_flags = "o")
execGRASS("v.rast.stats",map="nsvNet",raster="dem",column_prefix="elevRange",method="range")
nsvNet=readVECT("nsvNet")
nsvNet$slope=(nsvNet$elevRange_range/nsvNet$length) #as ratio

hist(nsvNet$slope)
#looks reasonable

reachData=data.frame(t(getEndpoints(nsvNet)))

reachData=left_join(reachData,data.frame(segID=as.numeric(nsvNet$AUTO),length=nsvNet$length,slope=nsvNet$slope))

#make two tibbles, location and characteristics, in long format, 
reachCoords=rbind(data.frame(segID=reachData$segID,type="UpstreamPoint",X=reachData$upPoint_x,Y=reachData$upPoint_y),
                  data.frame(segID=reachData$segID,type="DownstreamPoint",X=reachData$downPoint_x,Y=reachData$downPoint_y))

reachData=melt(as.tibble(reachData[c("segID","length","slope")]),
               id.vars="segID")


#####--------------write to leakyDB-------------
source("C:/Users/Sam/Dropbox/Logjams/R_geomorph/wrangleData/leakyFunctions.R")


leakyDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\sqLiteDatabase\\LeakyData.db3")
dbListTables(leakyDB)

batchName ="rGis_length_slope"
thisBatchID=addBatch(batchName)

writeIfNew(leakyDB,"QCStatus",data.frame(Description='ok'),"Description","QCStatusIDX")
qcStatusOK=dbGetQuery(leakyDB,"SELECT QCStatusIDX FROM QCStatus WHERE Description = 'ok'")[[1]]

#write coordinate types and metrics (if new)

writeIfNew(leakyDB,"CoordinateTypes",data.frame(Description=unique(reachCoords$type)),"Description","CoordinateTypeIDX")
unique(reachData$variable)
#join coord types back to coords
reachCoords=left_join(reachCoords,dbGetQuery(leakyDB,"SELECT * FROM CoordinateTypes"),by=c("type"="Description"))

writeIfNew(leakyDB,"DataTypes",data.frame(Metric=c("length","slope"),Unit=c("meters","ratio"),Method="RGIS"),c("Metric","Unit","Method"),"DataTypeIDX")
#join data types back to data
reachData=left_join(reachData,dbGetQuery(leakyDB,"SELECT Metric, DataTypeIDX FROM DataTypes"),by=c("variable"="Metric"))

#write coordinates
reachCoords$CoordinateIDX=as.numeric(reachCoords$segID)+max(as.numeric(dbGetQuery(leakyDB,"SELECT MAX(CoordinateIDX) FROM Coordinates")),0,na.rm = T)
reachCoords$EPSG=32613
writeIfNew(leakyDB,"Coordinates",data.frame(CoordinateIDX=reachCoords$CoordinateIDX,
                                            Name=paste("NSVmodel_",reachCoords$segID),
                                            CoordinateTypeIDX=reachCoords$CoordinateTypeIDX,
                                            X=reachCoords$X,
                                            Y=reachCoords$Y,
                                            EPSG=reachCoords$EPSG),
           c("Name","X","Y"),"RowIDX")

#reaches
dbGetQuery(leakyDB,"SELECT NetworkIDX FROM Networks WHERE NEtworkName = 'NSV' ")[[1]]
reaches=data.frame(ReachName=paste("NSVmodel_",reachCoords$segID),
                   CoordinateIDX=reachCoords$CoordinateIDX,
                   LandUse="Turkey Ranch",Confinement='',
                   NetworkIDX=dbGetQuery(leakyDB,"SELECT NetworkIDX FROM Networks WHERE NEtworkName = 'NSV' ")[[1]],
                   IsReal=TRUE)
reaches=base::unique(reaches)

writeIfNew(leakyDB,"Reaches",reaches,c("ReachName","CoordinateIDX","LandUse","Confinement","NetworkIDX","IsReal"),"ReachIDX")
#join reach idx back to reaches
reaches_idx=data.frame(ReachName=reaches$ReachName)
reaches_idx=left_join(reaches_idx,dbGetQuery(leakyDB,"Select ReachIDX, ReachName FROM Reaches"))

#points
points=data.frame(ReachIDX=reaches_idx$ReachIDX,IsReal=FALSE,PointName="FakeNsvPoint")
writeIfNew(leakyDB,"Points",points,c("PointName","ReachIDX"),"PointIDX")

#sensors, sources, data
writeIfNew(leakyDB,"Sensors",data.frame(SensorType="GRASS GIS",SerialNumber=7),"SensorType","SensorIDX")
thisSensor=dbGetQuery(leakyDB,"SELECT SensorIDX FROM Sensors WHERE SensorType = 'GRASS GIS'")[[1]]

writeIfNew(leakyDB,"Sources",data.frame(SensorIDX=thisSensor,Person="Sam Carlson"),c("SensorIDX","Person"),"SourceIDX")
thisSource=dbGetQuery(leakyDB,paste0("SELECT SourceIDX FROM Sources WHERE SensorIDX = '",thisSensor,"'"))

reachData=left_join(reachData,
                    left_join(reachCoords[c("segID","CoordinateIDX")],
                              left_join(dbGetQuery(leakyDB,"SELECT PointIDX, ReachIDX From Points"),dbGetQuery(leakyDB,"SELECT ReachIDX, CoordinateIDX From Reaches"))
                    )
)

writeData=data.frame(BatchIDX=thisBatchID,
                     DataTypeIDX=reachData$DataTypeIDX,
                     PointIDX=reachData$PointIDX,
                     SourceIDX=thisSource,
                     DateTime='',
                     QCStatusIDX=qcStatusOK,
                     Value=reachData$value)
dbWriteTable(leakyDB,"Data",writeData,append=T)
