#calculate length(s) (segmentwise) of spatialLinesDataFrame
addLengths=function(feature){
  getSegLength=function(seg){
    distSum=0
    if(nrow(seg@Lines[[1]]@coords)>1)
    {
      #cant figure out how to mapply here, but still seems fast enough
      for(i in 1:(nrow(seg@Lines[[1]]@coords)-1))
      {
        distSum=distSum+dist(seg@Lines[[1]]@coords[i:(i+1),])[1]
      }
      return(distSum)
    } else {
      return(0)
    }
  }
  lengths=sapply(feature@lines,getSegLength)
  feature$length=lengths
  return(feature)
}


#nsvNet@lines[[1]]@Lines[[1]]@coords[1,]
#diag(d)[diag(d)!=0]