removeSingleBatchData=function(batchID){
  pointIDXs=dbGetQuery(leakyDB, paste0("SELECT DISTINCT PointIDX FROM Data WHERE BatchIDX = '",batchID,"'"))$PointIDX
  sourceIDXs=dbGetQuery(leakyDB, paste0("SELECT DISTINCT SourceIDX FROM Data WHERE BatchIDX = '",batchID,"'"))$SourceIDX
  reachIDXs=dbGetQuery(leakyDB,paste0("SELECT DISTINCT ReachIDX FROM Points WHERE PointIDX IN (",paste0(pointIDXs,collapse=", "),")"))$ReachIDX
  coordIDXs=dbGetQuery(leakyDB, paste0("SELECT DISTINCT CoordinateIDX FROM Reaches WHERE ReachIDX IN(",paste0(reachIDXs,collapse=", "),")"))$CoordinateIDX
  sensorIDXs=dbGetQuery(leakyDB, paste0("SELECT DISTINCT SensorIDX FROM Sources WHERE SourceIDX IN(",paste0(sourceIDXs,collapse=", "),")"))$SensorIDX
  dataTypeIDXs=dbGetQuery(leakyDB, paste0("SELECT DISTINCT DataTypeIDX FROM Data WHERE BatchIDX = '",batchID,"'"))$DataTypeIDX
  
  dbExecute(leakyDB,paste0("DELETE FROM Data WHERE BatchIDX = '",batchID,"'"))
  dbExecute(leakyDB,paste0("DELETE FROM Sources WHERE SourceIDX IN (",paste0(sourceIDXs,collapse=", "),")"))
  dbExecute(leakyDB,paste0("DELETE FROM Sensors WHERE SensorIDX IN (",paste0(sensorIDXs,collapse=", "),")"))
  dbExecute(leakyDB,paste0("DELETE FROM DataTypes WHERE DataTypeIDX IN (",paste0(dataTypeIDXs[!is.na(dataTypeIDXs)],collapse=", "),")"))
  dbExecute(leakyDB,paste0("DELETE FROM Points WHERE PointIDX IN (",paste0(pointIDXs,collapse=", "),")"))
  dbExecute(leakyDB,paste0("DELETE FROM Reaches WHERE ReachIDX IN (",paste0(reachIDXs,collapse=", "),")"))
  dbExecute(leakyDB,paste0("DELETE FROM Coordinates WHERE CoordinateIDX IN (",paste0(coordIDXs,collapse=", "),")"))
  return(NULL)
}
