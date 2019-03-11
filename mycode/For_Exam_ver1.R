f<-function(x){x^4-20*x^3-2*x^2+6*x-2}

# 이분법

`` <- function(f, a, b, tol=1e-7) {
  f <- f
  if (f(a)==0) return(a)
  else if (f(b)==0) return(b)
  else if (f(a)*f(b)>0)
  {
    stop("signs of a and b are the same")
  } else {
    m= (a+b)/2
    if (f(m)==0) return(m)
    else if (f(a)*f(m)<0)
    {
      b<-m
    } else {
      a <- m
    }
    if (abs(b-a)<tol)
    {
      return((a+b)/2)
    } else {
      bs(f,a,b)
    }
  }
}


# Built-in functions

# sqrt
squrert = function(x)
{
  return(x^(1/2))
}

# abs
abss <- function(x) 
{
  if (x>=0) {
    print(x)
  } else {
    print(-x)
  }
}

# min
minn <- function(x)
{
  if (all(x[1] < x[-1])) {
    return(x[1])
  } else {
    x <- x[-1]
    return(minn(x))
  }
}

# max
maxx <- function(x)
{
  if (all(x[1] > x[-1])) {
    return(x[1])
  } else {
    x <- x[-1]
    return(maxx(x))
  }
}

# sum
sum2 <- function(x)
{
  x1 <- as.vector(x)
  A <- rep(1,length(x1))
  return(as.vector(x1%*%A))
}

# prod
prodd <- function(x)
{
  z <- 1
  for (i in 1:length(x)) {
    z <- c(x[i]*z)
  }
  return(z)
}

# cumsum
csum <- function(x)
{
  z <- x[1]
  for (i in 1:length(x)-1)
  {
    z <- c(z, z[i]+x[i+1])
  }
  return(z)
}

# cumprod
cprod <- function(x)
{
  z <- x[1]
  for (i in 1:length(x)-1)
  {
    z <- c(z,z[i]*x[i+1])
  }
  return(z)
}

# round
roundd <- function(x)
{
  if (x%%1 >= 0.5 )
  {
    return(x+(1-x%%1))
  } else {
    return(x-x%%1)
  }
}

# floor
floorr <- function(x)
{
  return(x - x%%1)
}

# ceiling
ceilingg <- function(x)
{
  return(x+(1-x%%1))
}

# factorial
rfact <- function(x)
{
  if (x==0)
  {
    return(1)
  } else {
    return(x*rfact(x-1))
  }
}

bs <- function(f, a, b, tol=1e-7) {
  if (f(a)*f(b)>0)
  {
    stop("signs or f(a) and f(b) are the same")
  } else {
    m = (a+b)/2
    if (f(a)*f(m)<0)
    {
      b <- m
    } else {
      a <- m
    }
    bs(a,b)
  }
}

# bisection method
bs <- function(f, a, b, tol=1e-7) {
  f <- f
  if (f(a)==0)
  {
    return(a)
  }
  if (f(b)==0)
  {
    return(b)
  }
  if (f(a)*f(b)>0)
  {
    stop("signs of a and b are the same")
  } else {
    m= (a+b)/2
    if (f(a)*f(m)==0)
    {
      return(m)
    }
    if (f(a)*f(m)<0)
    {
      b<-m
    } else {
      a <- m
    }
    if (abs(b-a)<tol)
    {
      return(a)
    } else {
      bs(f,a,b)
    }
  }
}

# find area under the curve
f = function(x) {
  -x^2+x +5
}
plot(f, xlim=c(-5,5),ylim=c(-30,30))
abline(h=0)
abline(v=0)

fa <- function(f,a,b,n=100000)
{
  if (f(a)>0&&f(b)>0)
  {
    c <- max(c(f(a),f(b))) + 1
    w <- b-a
    rh <- runif(n,a,b)
    rw <- runif(n,0,c)
    d <- rw < f(rh)
    in.it <- sum(d)
    p<-in.it/n
    return((c*w)*p)
  }
  if (f(a)<0&&f(b)<0)
  {
    c <- min(c(f(a),f(b))) -1
    w <- b-a
    rh <- runif(n,a,b)
    rw <- runif(n,c,0)
    d <- rw > f(rh)
    in.it <- sum(d)
    p<-in.it/n
    return((c*w)*p)
  }
  if (f(a)>0&&f(b)<0)
  {
    m <- uniroot(f,c(a,b))[[1]]
    c1 <- f(a) + 1
    w1 <- m-a
    rh1 <- runif(n*((m-a)/(b-a)),a,m)
    rw1 <- runif(n*((m-a)/(b-a)),0,c1)
    d1 <- rw1 < f(rh1)
    in.it1 <- sum(d1)
    p1<-in.it1/(n*((m-a)/(b-a)))
    c2 <- f(b) -1
    w2 <- b-m
    rh2 <- runif(n*(b-m)/(b-a),m,b)
    rw2 <- runif(n*(b-m)/(b-a),c2,0)
    d2 <- rw2 > f(rh2)
    in.it2 <- sum(d2)
    p2<-in.it2/(n*(b-m)/(b-a))
    return((c1*w1)*p1+(c2*w2)*p2)
  }
  if (f(a)<0&&f(b)>0)
  {
    m <- uniroot(f,c(a,b))[[1]]
    c1 <- f(a) - 1
    w1 <- m-a
    rh1 <- runif(n*((m-a)/(b-a)),a,m)
    rw1 <- runif(n*((m-a)/(b-a)),c1,0)
    d1 <- rw1 > f(rh1)
    in.it1 <- sum(d1)
    p1<-in.it1/(n*((m-a)/(b-a)))
    c2 <- f(b) + 1
    w2 <- b-m
    rh2 <- runif(n*(b-m)/(b-a),m,b)
    rw2 <- runif(n*(b-m)/(b-a),0,c2)
    d2 <- rw2 < f(rh2)
    in.it2 <- sum(d2)
    p2<-in.it2/(n*(b-m)/(b-a))
    return((c1*w1)*p1+(c2*w2)*p2)
  }
}

# monte ssibal carlo
mc <- function(f,a,b, n=10000)
{
  points <- seq(a,b,length.out = n)
  height <- mean(f(points))
  return((b-a)*height)
}


# confidence interval
cfv = function(n, R=100, mu=0, sd=1, alpha=0.05)
   {
    + B = 5 * sd/sqrt(n)
    + K = 0
    + plot(c(0,R+1), c(mu,mu), type='l', col='blue',
           + lwd=2, ylim=c(mu-B,mu+B), xlab='Repetition',
           + ylab='Confidence Interval', main='Simulation')
    + for (i in 1:R)
      + {
        + x = rnorm(n,mean=mu,sd=sd)
        + LB = mean(x) + qt(alpha/2,n-1) * sd(x) / sqrt(n)
        + UB = mean(x) + qt(1-alpha/2,n-1) * sd(x) / sqrt(n)
        + if (LB > mu | UB < mu) lines(c(i,i),c(LB,UB),col='red',lwd=2)
        + else {
          + lines(c(i,i),c(LB,UB))
          + K = K + 1 }
        + Sys.sleep(0.25)
         }
    + sprintf('Coverage Probability: %.3f',K/R)
     }






######################## practice ###################
bs<-function(f,a,b,ep=1/(10^7)){
  if (f(a)*f(b)>0){
    stop("signs or f(a) and f(b) are the same")
  }
  else if (f(a)==0) return(a)
  else if (f(b)==0) return(b)
  else {
    m<-(a+b)/2
    if (f(m)==0) return(m)
    else if(f(m)*f(a)>0){
      a<-m
    }
    else (b<-m)
  }
  if(abs(b-a)<ep) return((a+b)/2)
  else (bs(f,a,b))
}
  

cumsum.r<-function(x){
  z<-x[1]
  for (i in 1:c(length(x)-1)){
    z<-c(z,z[i]+x[i+1])
  }
  return(z)
}

cumprod.r<-function(x){
  z<-x[1]
  for (i in 1:c(length(x)-1)){
    z<-c(z,z[i]*x[i+1])
  }
  return(z)
}


# 적중법 
Junk<-function(f,a,b,n=10000){
  if (f(a)>=0 & f(b)>=0){
  mh<-max(c(f(a),f(b)))+1
  al<-runif(n,0,mh)
  inl<-runif(n,a,b)
  p<-sum(al<=f(inl))/n
  s<-p*(b-a)*mh
  return(s)
  }
  
  
}

# circle
cir<-function(r,n=100000){
  x<-runif(n,0,r)
  y<-runif(n,0,r)
  z<-(x^2+y^2)<=(r^2)
  return(4*(r^2)*(sum(z)/n))
  
}

pi.r<-function(n=1000000){
  x<-runif(n,0,1)
  y<-runif(n,0,1)
  z<-(x^2+y^2)<=1
  return((sum(z)/n)*4)
}

# confidential interval

cv<-function(n, R=100, mu=0, sd=1 ,alpha=0.05){
  K=0
  yl<-5*sd/sqrt(n)
  plot(c(1,R+1),c(mu,mu),type='l',col='blue',lty=2,xlab="cibal",
        ylab='fuck',main='sorry',ylim=c(-yl,yl))
  for ( i in 1:R){
    x<-rnorm(n,mu,sd)
    LB<-mean(x)+qt(alpha/2,n-1)*sd(x)/sqrt(n)
    UB<-mean(x)+qt(1-alpha/2,n-1)*sd(x)/sqrt(n)
    if(UB<mu | LB>mu) lines(c(i,i),c(LB,UB),col='red')
    else{lines(c(i,i),c(LB,UB))
         K= K+1}
    
  }
    sprintf( "CV is %.2f",K/R)
  
  
}

# 이분법 own

bss<-function(f,a,b,tol=1e-8){
  m<-(a+b)/2
  if (f(a)*f(b)>0) stop ("signs of a and b are same :( ")
  else if (f(a)==0) return (a)
  else if (f(b)==0) return (b)
  else if (f(m)==0) return (m)
  else if (f(a)*f(b)<0) {
    if(f(m)*f(a)<0) b<-m
    else (a<-m)
    
  }
  if (abs(b-a)<tol) return ( (a+b)/2 )
  else(return(bss(f,a,b)))
  
}


##pie own 

pie<-function(r){
  M=0
  plot(c(0,r+1),c(pi,pi),type='l',col="red",lwd=1.2)
  
  oldp<-0
  for (i in 1:r){
    x<-runif(2,0,1)
    if((x[1]^2+x[2]^2) <= 1) M<-M+1
    newp<-4*M/i
    lines(c(i-1,i),c(oldp,newp),col='blue')
    oldp<-newp
  }
  return(newp)
  
  
}

# 원 넓이 
circle<-function(n,r){
  M=0
  for (i in 1:n){
    x<-runif(2,0,r)
    if((x[1]^2+x[2]^2) <= (r^2) ) M<-M+1
  }
  return(4*r^2*M/n)
  
  
}


## CLT
CLT<-function(dist=c('normal','expo
                     nential','uniform'),...){
  nlist<-c(3,5,10,30)
  par(mfrow=c(2,2))
  if (dist=='normal') {
  for ( i in nlist){
  f<- rnorm(i*5000,...)
  m<-matrix(f,n,5000)
  avg<-apply(m,2,mean)
  hist(avg,col='blue',freq=F,main=sprintf('normal n=%.0f',i ))
  curve(dnorm(x,mean(x),sd(x)),col='red',add=T)
  }
  }
  else if (dist=='exponential') {
    for ( i in nlist){
      f<- rexp(i*5000,...)
      m<-matrix(f,n,5000)
      avg<-apply(m,2,mean)
      hist(avg,col='blue',freq=F,main=sprintf('exponential n=%.0f',i ))
    }
  }
  else if (dist=='uniform') {
    for ( i in nlist){
      f<- runif(i*5000,...)
      m<-matrix(f,n,5000)
      avg<-apply(m,2,mean)
      hist(avg,col='blue',freq=F,main=sprintf('uniform n=%.0f',i ))
    }
  }
  }

# CLT new (use get)
CLT<-function(dist=c('normal','exponential','uniform'),...){
  if (dist=='normal') fn<-'rnorm'
  else  if (dist=='exponential') fn<-'rexp'
  else  if (dist=='uniform') fn<-'runif'
  else stop("dist is worng")
  par(mfrow=c(2,2))
  for (i in c(3,5,7,10)){
    x<-get(fn)(i*5000,...)
    t<-apply(matrix(x,i,5000),2,mean)
    hist(t,breaks=50,main=sprintf("n = %.0f",i),freq=F)
    mtext(paste('Central Limit Theorem: ', dist),outer=T,line=-1.5,cex=1.5)
    curve(dnorm(x,mean(x),sd(x)/sqrt(i)),col='blue',add=T)
  }
  
  
}
# clt for binom (여기서 10 -> 1 이면 베르누이)
clt<-function(n,r=5000){
  a<-apply(matrix(rbinom(r*n,10,0.1),n,r),2,mean)
  plot(table(a)/r*n)
  
}

## 몬테칼로 파이값 추정 (속도 최고, 같은 정확도라면 난수 더 적게 필요함, 최고 우월)
SM<-function(n){
  x<-runif(n)
  return(4*mean(sqrt(1-x^2)))
}
curve(sqrt(1-x^2),0,1)
abline(v=0);abline(h=0)
SM(100000)

## 표본평균 적중법 몬테칼로 
SM2<-function(f,a,b,n=10000){
  x=a+(b-a)*runif(n)
  return((b-a)*mean(f(x)))
}


f<-function(x) x^2+1

mon<-function(f,a,b,n=100000){
  p<-runif(n,a,b)
  h<-mean(f(p))
  return( (b-a)*h)
  
}


# 교안 마지막 pie 추정 간단하니까 그걸로 해보자 


mi<-function(x){
  if (all(x[1]<=x[-1])) return (x[1])
  else (x<-x[-1])
  return(mi(x))
  
}









