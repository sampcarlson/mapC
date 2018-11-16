require(sp)
require(raster)
require(rgdal)
require(rgrass7)
require(rgeos)

nsvNet=shapefile("~/R Projects/mapC/Spatial_Inputs/NSVStreamSegmentsIDX.shp")
#as compared to Q, the data is stored in attributes, and the attributes are stored as data
#load as raster (package) object
#dem=raster("~/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif")

#loads as SpatialGridDataFrame object
dem=readGDAL("~/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif")

#drop segments shorter than 2 coordinate pairs
nsvNet=subset(nsvNet,sapply(nsvNet@lines,getCoordCount)>1)


#nsvNet=addLengths(nsvNet) Function (below) from rgeos works 
nsvNet$length=gLength(nsvNet,byid=T)

nsvPts=spsample(x=nsvNet,n=sum(nsvNet$length)/500,type="regular")#pts every 100 meters-ish
margin=500
plot(dem,xlim=extent(nsvNet)[1:2]+c(-margin,margin),ylim=extent(nsvNet)[3:4]+c(-margin,margin))
plot(nsvNet,add=T)
plot(nsvPts,add=T,pch=1)


gCrosses(nsvNet,nsvPts)
plot(gPointOnSurface(nsvNet,byid=T),add=T)

