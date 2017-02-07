options(stringsAsFactors=F)

multtslinear <- function(tsmatrix, prevperiods=5, futureperiods = 5)
{
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
  fit <- lm(labelz~.,data=ugh)
  return(fit)
}
