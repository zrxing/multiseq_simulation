agg.counts=function(data,window.size){
  m=dim(data)[1]
  n=dim(data)[2]
  nwin=ceiling(n/window.size)
  inds=seq(1,n,window.size)
  inde=c(seq(window.size,n,window.size),n)
  data.agg=NULL
  for(i in 1:length(inds)){
    data.sub=as.matrix(data[,inds[i]:inde[i]])
    data.agg=cbind(data.agg,rowSums(data.sub))
  }
  return(data.agg)
}