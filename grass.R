require(rgrass7)
initGRASS("C:/Program Files/GRASS GIS 7.4.0",SG=dem,override=T)
readRAST("C:/Users/Sam/Documents/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif")
writeRAST(dem,"dem")
writeVECT(nsvNet,vname="nsvNet")
