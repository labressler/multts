require(xgboost)
options(stringsAsFactors=F)
require(dplyr)

multts <- function(tsmatrix,predictvector = runif(3,-2,2),futureperiods=3,prevperiods=nrow(tsmatrix)-futureperiods+1)
{
  if(length(predictvector)==1)
  {
    predictvector <- tsmatrix[,predictvector]
  }
  laggedv <- data.frame(V1=numeric(0),V2=numeric(0),V3=numeric(0),V4=numeric(0),V5=numeric(0))
  labelz <- vector()
  for (i in 1:(nrow(tsmatrix)-prevperiods))
  {
    for (j in 1:ncol(tsmatrix))
    {
      vec <- as.vector(tsmatrix[i:(i+prevperiods-1),j])
      laggedv <- rbind.data.frame(laggedv,vec)
      labelz <- append(labelz,tsmatrix[i+prevperiods,j])
    }
  }
  ugh <- cbind.data.frame(laggedv,labelz)

  traincf <- xgb.DMatrix(data=data.matrix(ugh[,colnames(ugh)!="labelz"]),label=ugh$labelz,missing=NaN) #modeling
  cvs <- data.frame()
  vecit <- vector()
  vecerror <- vector()
  for (i in 1:3)
  {
    for (useless in 1:3)
    {
      xg <- xgb.cv(params=list("objective"="reg:linear","eval_metric"="rmse"),data=traincf,nrounds=10000,nfold=4,max.depth=i,eta=.2,missing=NaN,maximize=FALSE,early.stop.round=10,verbose=F)
      vecit <- append(vecit,which.min(as.matrix(xg[,3,with=F])))
      vecerror <- append(vecerror,xg[which.min(as.matrix(xg[,3,with=F])),3,with=F])
    }
    ourvec <- c(Iterations=mean(vecit),Error=mean(unlist(vecerror)))
    cvs <- rbind(cvs,ourvec)
    vecit <- vector()
    vecerror <- vector()
  }
  colnames(cvs) <- c("AvIterations","AvError")
  bestdepth <- which.min(cvs$AvError)
  averror <- cvs$AvError[bestdepth]
  bestiter <- round(cvs$AvIter[bestdepth]*1.0625,0)
  bstcf <- xgb.train(data=traincf,eta=.2,nrounds=bestiter,objective="reg:linear",max.depth=bestdepth,eval_metric="rmse")
	#matrx <- c(3,2,1)
  matrx <- predictvector
	leng <- futureperiods
	for (i in 1:leng)
	{
		#lgg <- length(matrx)
		mx1 <- t(matrx[1:3])
		bcf <- xgb.DMatrix(data=data.matrix(mx1),missing=NaN)
		pdd <- predict(bstcf,bcf)
		matrx <- append(pdd,matrx)
	}
	matrx1 <- rev(matrx)
	#matrx1 <- t(cbind(matrx,pdd))
	#print(matrx1)
	plott <- cbind.data.frame(seq(1-length(predictvector),leng,by=1),matrx1)
	lgg <- nrow(plott)
	plot(plott,pch=20,cex=.6,col=c(rep("black",3),rep("blue",lgg-3)))
	linecols <- c(rep("black",2),rep("blue",lgg-3))
	for (i in 1:(lgg-1))
	{
		lines(plott[c(i,i+1),],col=linecols[i])
	}

	#xg <-xgb.cv(params=list("objective"="reg:linear","eval_metric"="rmse"),data=traincf,nrounds=33,nfold=10,max.depth=3,eta=.15,missing=NaN,verbose=F)
	#conf <- xg[which.min(as.data.frame(xg)[,3]),3,with=F]
	pttt <- data.frame()
	#pttt <- rbind(plott[3,],plott[4,],plott[4,])
	for (i in 1:leng)
	{
	  pttt <- bind_rows(pttt,plott[lgg-leng+i-1,],plott[lgg-leng+i,],plott[lgg-leng+i,])
	  pttt[3*(i-1)+2,2] <- as.numeric(pttt[3*(i-1)+2,2]+sqrt(i*averror^2)*1.28)
	  pttt[3*i,2] <- as.numeric(pttt[3*i,2]-sqrt(i*averror^2)*1.28)
	}
	#pttt[2,2] <- as.numeric(pttt[2,2]+conf[1]*1.28)
	#pttt[3,2] <- as.numeric(pttt[3,2]-conf[1]*1.28)
	plot(plott,pch=20,cex=.6,col=c(rep("black",lgg-futureperiods),rep("blue",futureperiods)),ylim=c(min(pttt[,2]-.5),max(pttt[,2]+.5)))
	linecols <- c(rep("black",lgg-futureperiods-1),rep("blue",futureperiods))
	for (i in 1:(lgg-1))
	{
		lines(plott[c(i,i+1),],col=linecols[i])
	}
	lines(pttt[1:2,],col="red")
	lines(pttt[c(1,3),],col="red")
	if (leng>=2)
	{
	  for (i in 2:leng)
	  {
	    lines(pttt[c((3*i-4),(3*i-1)),],col="red")
	    lines(pttt[c(3*i-3,3*i),],col="red")
	  }
	}
	predi <- plott[(lgg-leng+1):lgg,]
	colnames(predi) <- c("FuturePeriod","Prediction")
	return(list(prediction=as.vector(predi),firstpredictionerror=averror))
	#lines(pttt[1:2,],col="red")
	#lines(pttt[c(1,3),],col="red")
}
