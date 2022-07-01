INV<-function(x){
  x/sum(x^2)
}
geps<-function(theta,threshold){
  row=nrow(theta)
  t<-matrix(0,row,kk)
  row=row-1
  while(row>1){
    for(k in 1:row){
      t[k,]=t[k+1,]+INV(theta[k+1,]-theta[k,])
    }
    row=row-1
    if(row>0){
      for(k in 1:row){
        theta[k,]=theta[k+1,]+INV(t[k+1,]-t[k,])
        if(k>1&&metrics(theta[k,],theta[k-1,])<threshold){
          return(theta[k,])
        }
      }
      row=row-1
    }
  }
  return(theta[1,])#or return sequence or anyone of sequence
}
#theta is a EM sequence
