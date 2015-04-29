

svdDenoise=function(A,nrg=.9,remove=0){
  require(matlab)
  require(matrixStats)
  require(stats)
  if (!(nrg<=1 && nrg>=0)){print("Energy must be between 0 and 1")}
  #identify dimensions
  m<-dim(A)[1];n<-dim(A)[2];

  s<-svd(A);singularv<-s$d;sumsv<-sum(singularv)
  if (remove==0){
    for (i in 1:n){
      Pnrg=sum(singularv[1:i])/sumsv
      if (Pnrg>=nrg){
        numFeat=i
        #     print(numFeat)
        break
      }
    }
    #   plot(cumsum(s$d))
    newSing=s$d[1:numFeat]
  } else {
    newSing=s$d[1:(n-remove)]
    i=n-remove
  }
  red_S=zeros(n)
  for (j in 1:i){
    red_S[j,j]=newSing[j]
  }

  newA=((s$u)%*%(red_S))%*%t(s$v)


}

