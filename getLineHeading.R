#calculate length(s) (segmentwise) of spatialLinesDataFrame
getHeadings=function(feature,smoothScope=1){
  getSegHeadings=function(seg,smoothScope){
    if(nrow(seg@Lines[[1]]@coords)>1)
      segHeadings
      for(i in 1:(nrow(seg@Lines[[1]]@coords)-1)){
        coordDif=seg@Lines[[1]]@coords[(i+1),]-seg@Lines[[1]]@coords[i,]
        thisHeading=atan2(y=coordDif[2],x=coordDif[1])
      }
  }
  
  lengths=sapply(feature@lines,getSegHeadings,smoothScope=smoothScope)
  feature$Heading=lengths
  return(feature)
}


#nsvNet@lines[[1]]@Lines[[1]]@coords[1,]
#diag(d)[diag(d)!=0]