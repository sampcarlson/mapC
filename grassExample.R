require(rgrass7)
require(rgdal)
?initGRASS()
initGRASS("C:/Program Files/GRASS GIS 7.4.0",override=T,mapset="NsvExample")
#projection
#can also take projection from a file
execGRASS("g.mapset",flags="l")
execGRASS("g.mapset",mapset="PERMANENT")
execGRASS("g.proj",epsg=32613, flags="c")
execGRASS("g.region", flags="s") #set as default region
execGRASS("g.mapset",mapset="NsvExample")
execGRASS("g.region", flags="d") #set from default region

#write TO grass FROM R
?writeRAST
#write is vectorized(i.e., pass it a vector of rastes via x and vname)
#note that this does not come through R environment
writeRAST(x=readGDAL("C:/Users/Sam/Documents/R Projects/mapC/Spatial_Inputs/BigDemWGS84.tif"),vname='dem')

execGRASS("g.region",raster="dem") #set processing extent by raster
#other input formats?
execGRASS("r.in.gdal",flags="f")

#look at 'dem' info
execGRASS("r.info",map='dem')

#bring in vector as Spatial*DataFrame
#nsvNet=shapefile("~/R Projects/mapC/Spatial_Inputs/NSVStreamSegmentsIDX.shp")
#writeVECT(nsvNet,vname="nsvNet")

execGRASS("v.in.ogr",input="C:/Users/Sam/Documents/R Projects/mapC/Spatial_Inputs/NSVStreamSegmentsIDX.shp",output="nsvNet")
execGRASS("v.info",map='nsvNet')

execGRASS("v.to.points",input="nsvNet",output="nsvPoints",dmax=1000)
#execGRASS("v.build",map="nsvPoints")

execGRASS("v.out.ogr",input="nsvPoints",type="point",output="C:/Users/Sam/Documents/R Projects/mapC/Spatial_Inputs/NSVStreamPoints.shp",format="ESRI_Shapefile",flags="overwrite")


execGRASS("v.rast.stats",map="nsvNet",raster="dem",column_prefix="elev",method="range")

execGRASS("v.out.ogr",input="nsvNet",type="line",output="C:/Users/Sam/Documents/R Projects/mapC/Spatial_Inputs/NSVSegmentsRange",format="ESRI_Shapefile",flags="overwrite")

