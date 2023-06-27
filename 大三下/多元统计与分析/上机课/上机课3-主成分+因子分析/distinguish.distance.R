distinguish.distance=function
(TrnX, TrnG, TstX = NULL, var.equal = FALSE){
  if ( is.factor(TrnG) == FALSE){
    mx=nrow(TrnX); mg=nrow(TrnG)
    TrnX=rbind(TrnX, TrnG)
    TrnG=factor(rep(1:2, c(mx, mg)))
  }
  if (is.null(TstX) == TRUE) TstX=TrnX
  if (is.vector(TstX) == TRUE)  TstX=t(as.matrix(TstX))
  else if (is.matrix(TstX) != TRUE)
    TstX=as.matrix(TstX)
  if (is.matrix(TrnX) != TRUE) TrnX=as.matrix(TrnX)
  
  nx=nrow(TstX)
  blong=matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
  g=length(levels(TrnG))
  mu=matrix(0, nrow=g, ncol=ncol(TrnX))
  for (i in 1:g)
    mu[i,]=colMeans(TrnX[TrnG==i,]) 
  D=matrix(0, nrow=g, ncol=nx)
  if (var.equal == TRUE  || var.equal == T){
    for (i in 1:g)
      D[i,]= mahalanobis(TstX, mu[i,], var(TrnX))
  }
  else{
    for (i in 1:g)
      D[i,]= mahalanobis(TstX, mu[i,], var(TrnX[TrnG==i,]))
  }
  for (j in 1:nx){
    dmin=Inf
    for (i in 1:g)
      if (D[i,j]<dmin){
        dmin=D[i,j]; blong[j]=i
      }
  }
  blong
}