#' @title Minkowski Function
#' @description computes minkowski distances
#' @param two numeric vector (x,y) and another numeric value/character string
#' @return computed distance
minkowski <- function(x,y,p=1){
  if(p=='max'){
  p=1}
  if(length(x)!=length(y)){
    stop("x and y have different lengths")
  } else{
  if(class(p)=='numeric'){
    if(p>=1){
      s<-rep(0,n)
      t=0
      for(i in 1:n){
        t=(abs(x[i]-y[i]))^p
        s[i]=t
        distance <- (sum(s,na.rm = TRUE))^(1/p)
      }
      return(distance)}
  } else{
      stop("p cannot be less than 1")
    }
      if(class(p)=='character'){
        if(p == "max"){
          s<-rep(0,n)
          t=0
          for(i in 1:n){
            t=(abs(x[i]-y[i]))^p
            s[i]=t
            distance <- (sum(s,na.rm = TRUE))^(1/p)
        }} else{
          stop("invalid character value for p")
        }
        }else{
          stop()}
    }}



#这是随意试的地方
dis<-function(x,y,p=1){
  if(p=='max'){
    p=1
  }
  s<-rep(0,n)
  t=0
for(i in 1:n){
  t=(abs(x[i]-y[i]))^p
  s[i]=t
  distance <- (sum(s,na.rm = TRUE))^(1/p)
}
  return(distance)}
dis(c(2,2),c(3,3),'max')






