require(tidyverse)
library(RSQLite)
require(reshape2)

pf_raw=read.csv("pfeiffer_thesis_data.csv")

splitOnComma=function(inString,which){
  inString=as.character(inString)
  if(which=='x'){return(paste0("-",strsplit(inString,", ")[[1]][2]))}
  if(which=='y'){return(strsplit(inString,", ")[[1]][1])}
}

pf_raw$x=sapply(pf_raw$coordinates,splitOnComma,which='x')
pf_raw$y=sapply(pf_raw$coordinates,splitOnComma,which='y')


pf_raw$woodPerArea=pf_raw$woodLoad_m3 /(pf_raw$length_m *pf_raw$width_m )
pf_raw$coarseSedPerArea=pf_raw$coarseSed_m3 /(pf_raw$length_m *pf_raw$width_m )
pf_raw$fineSedperArea=pf_raw$fineSed_m3 /(pf_raw$length_m *pf_raw$width_m )
pf_raw$pomPerArea=pf_raw$pom_m3 /(pf_raw$length_m *pf_raw$width_m )


names(pf_raw)
#split coords from data, use name as id:
pf_location=pf_raw[c("Name","x","y")]
pf_data=pf_raw[c("Name","gradient_deg","confinementRatio","width_m","woodPerArea","coarseSedPerArea","fineSedperArea","pomPerArea")]
names(pf_data)=c("Name","slope","Confinement","Wetted width","Wood Volume", "Coarse Sed Volume","Fine Sed Volume", "POM Volume")
pf_data$Confinement[pf_data$Confinement=="*"]=NA
pf_data=melt(pf_data,id.vars="Name",na.rm=T,factorsAsStrings = T)
#####--------------write to leakyDB-------------
source("C:/Users/Sam/Dropbox/Logjams/R_geomorph/wrangleData/leakyFunctions.R")


leakyDB=dbConnect(SQLite(),"C:\\Users\\Sam\\Documents\\LeakyRivers\\Data\\sqLiteDatabase\\LeakyData.db3")
dbListTables(leakyDB)

batchName ="pfeiffer_whol_geomorph"
thisBatchID=addBatch(batchName)

writeIfNew(leakyDB,"QCStatus",data.frame(Description='ok'),"Description","QCStatusIDX")
qcStatusOK=dbGetQuery(leakyDB,"SELECT QCStatusIDX FROM QCStatus WHERE Description = 'ok'")[[1]]

#coordinate types and metrics (write if new)
coordType=dbGetQuery(leakyDB,"SELECT CoordinateTypeIDX FROM CoordinateTypes WHERE Description = 'Midpoint'")[[1]]
pf_location$CoordinateTypeIDX=coordType
#unique(pf_data$variable)
#dbGetQuery(leakyDB,"SELECT * FROM DataTypes")
dataTypes=data.frame(Metric=c("slope","Confinement","Wetted Width","Wood Volume","Coarse Sed Volume","Fine Sed Volume","POM Volume"),
           Unit=c("angle (degrees)","width ratio","m^3/m^2","m^3/m^2","m^3/m^2","m^3/m^2","m^3/m^2"),
           Method="Field Observation")


writeIfNew(leakyDB,"DataTypes",dataTypes,c("Metric","Unit","Method"),"DataTypeIDX")
#join data types back to data
pf_data=left_join(pf_data,dbGetQuery(leakyDB,"SELECT Metric, DataTypeIDX FROM DataTypes WHERE Method = 'Field Observation'"),by=c("variable"="Metric"))

#write coordinates
pf_location$CoordinateIDX=1:nrow(pf_location)+max(as.numeric(dbGetQuery(leakyDB,"SELECT MAX(CoordinateIDX) FROM Coordinates")),0,na.rm = T)
pf_location$EPSG=4326
writeIfNew(leakyDB,"Coordinates",data.frame(CoordinateIDX=pf_location$CoordinateIDX,
                                            Name=paste0("Pfeiffer_reach_",pf_location$Name),
                                            CoordinateTypeIDX=pf_location$CoordinateTypeIDX,
                                            X=pf_location$x,
                                            Y=pf_location$y,
                                            EPSG=pf_location$EPSG)
           ,c("Name","X","Y"),"RowIDX")



#reaches
nsvID=dbGetQuery(leakyDB,"SELECT NetworkIDX FROM Networks WHERE NetworkName = 'NSV' ")[[1]]
reaches=data.frame(ReachName=paste("Pfeiffer_reach_",pf_location$Name),
                   CoordinateIDX=pf_location$CoordinateIDX,
                   LandUse="Turkey Ranch",Confinement='',
                   NetworkIDX=nsvID,
                   IsReal=TRUE)
reaches=base::unique(reaches)

writeIfNew(leakyDB,"Reaches",reaches,c("ReachName","CoordinateIDX","LandUse","Confinement","NetworkIDX","IsReal"),"ReachIDX")
#join reach idx back to reaches
reaches_idx=data.frame(ReachName=reaches$ReachName)
reaches_idx=left_join(reaches_idx,dbGetQuery(leakyDB,"Select ReachIDX, ReachName FROM Reaches"))

#points
points=data.frame(ReachIDX=reaches_idx$ReachIDX,IsReal=FALSE,PointName="FakeReachPoint")
writeIfNew(leakyDB,"Points",points,c("PointName","ReachIDX"),"PointIDX")

#sensor
thisSensor=2
writeIfNew(leakyDB,"Sources",data.frame(SensorIDX=thisSensor,Person="Andrew Pfeiffer"),c("SensorIDX","Person"),"SourceIDX")
dbGetQuery(leakyDB,"SELECT * FROM Sources ")
thisSource=8

pf_data=left_join(pf_data,
                    left_join(pf_location[c("Name","CoordinateIDX")],
                              left_join(dbGetQuery(leakyDB,"SELECT PointIDX, ReachIDX From Points"),dbGetQuery(leakyDB,"SELECT ReachIDX, CoordinateIDX From Reaches"))
                    )
)

writeData=data.frame(BatchIDX=thisBatchID,
                     DataTypeIDX=pf_data$DataTypeIDX,
                     PointIDX=pf_data$PointIDX,
                     SourceIDX=thisSource,
                     DateTime='',
                     QCStatusIDX=qcStatusOK,
                     Value=pf_data$value)

dbWriteTable(leakyDB,"Data",writeData,append=T)

