#----------------------------------------------------------------#
#----------------------------------------------------------------#

#### Distribution plotting functions ####

poisplot<-function(lambda){
  x<-seq(0,30)
  y<-dpois(x,lambda=lambda)
  plot(y~x,pch=19)
  for(i in 1:length(x)){
    segments(x[i],0,x[i],y[i])  
  }
}

binomplot<-function(size,prob){
  x<-seq(0,size,1)
  y<-dbinom(x,size=size,prob=prob)
  plot(y~x,pch=19)
  for(i in 1:length(x)){
    segments(x[i],0,x[i],y[i])  
  }
}

nbinomplot<-function(mu,size){
  x<-seq(0,size,1)
  y<-dnbinom(x,mu=mu,size=size)
  plot(y~x,pch=19)
  for(i in 1:length(x)){
    segments(x[i],0,x[i],y[i])  
  }
}

normplot<-function(mean,sd){
  curve(dnorm(x,mean=mean,sd=sd),-30,30,col='darkblue')
  xs<-seq(-30,30,0.01)
  ys<-c(0,dnorm(xs,mean=mean,sd=sd),0)
  xs<-c(-30,xs,30)
  polygon(xs,ys,col='blue')
}

# compare poisson and normal distributions
pois.normplot<-function(lambda){
  x<-seq(0,80)
  y<-dpois(x,lambda=lambda)
  plot(y~x,pch=19)
  for(i in 1:length(x)){
    segments(x[i],0,x[i],y[i])  
  }
  xs<-seq(0,80,0.01)
  ys<-c(0,dnorm(xs,mean=lambda,sd=sqrt(lambda)),0)
  xs<-c(0,xs,80)
  polygon(xs,ys,col=rgb(1, 0, 0,0.5))
}

# beta plot function
betaplot<-function(mu,phi){
  curve(dbeta2(x,mu=mu,phi=phi),0,1,col='darkblue')
  xs<-seq(0.01,0.99,0.005)
  ys<-c(0,dbeta2(0.001,mu=mu,phi=phi),dbeta2(xs,mu=mu,phi=phi),dbeta2(0.999,mu=mu,phi=phi),0)
  xs<-c(0.001,0.001,xs,0.999,0.999)
  polygon(xs,ys,col='blue')
}

# gamma plot function
gammaplot<-function(shape,scale){
  curve(dgamma(x,shape=shape,scale=scale),0,40,col='darkblue')
  xs<-seq(0.001,40.001,0.02)
  ys<-c(0,dgamma(xs,shape=shape,scale=scale),0)
  xs<-c(0.001,xs,40.001)
  polygon(xs,ys,col='blue') 
}

# exponential plot function
expplot<-function(rate){
  curve(dexp(x,rate=rate),0,40,col='darkblue')
  xs<-seq(0.001,40.001,0.02)
  ys<-c(0,dexp(xs,rate=rate),0)
  xs<-c(0.001,xs,40.001)
  polygon(xs,ys,col='blue')  
}

#----------------------------------------------------------------#
#----------------------------------------------------------------#

