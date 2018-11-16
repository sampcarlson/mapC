getCoordCount=function(subFeature){
  return(nrow(subFeature@Lines[[1]]@coords))
}
