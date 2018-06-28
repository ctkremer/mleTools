
#----------------------------------------------------------------#
#----------------------------------------------------------------#

#### Distributions for mle calculations ####

#' Mean-parameterized beta distribution
#'
#' @param x Calculate the probability density at these values
#' @param mu The mean of the beta distribution
#' @param phi Dispersion parameter 
#' @param log logical; return the ln of the probability density?
#'
#' @export
dbeta2<-function(x,mu,phi,log=FALSE){
  dbeta(x,mu*phi,(1-mu)*phi,log=log)
}


#' Mean-parameterized gamma distribution
#'
#' Note: parmeter s is not the variance of the distribution. 
#' Variance = mu + mu^2/s
#'
#' @param x Calculate the probability density at these values
#' @param mu The mean of the gamma distribution
#' @param s Dispersion parameter 
#' @param log logical; return the ln of the probability density?
#'
#' @export
dgamma2<-function(x,mu,s,log=F){
  dgamma(x,shape=gamma.shape(mu,s),
         scale=gamma.scale(mu,s),log=log)
}

# conversion functions for gamma reparameterization
#gamma.shape<-function(mu,s) (mu^2)/s
#gamma.scale<-function(mu,s) s/mu

#### Mixture distributions: ####

#' Zero-altered beta distribution
#' 	
#' Code for vectorized zero altered beta distribution that avoids
#' errors in dbeta, and can handle log values.  Follows mean and
#' precision parameterization given in Ramalho et al. 2009
#' 
#' @param x 
#' @param z
#' @param mu
#' @param phi
#' @param log
#' 
#' @export
dzabeta<-function(x,z,mu,phi,log=FALSE){
  if(log==FALSE){
    ifelse(x==0,z,(1-z)*dbeta2(x,mu,phi))
  }
  else{
    ifelse(x==0,log(z),log((1-z)*dbeta2(x,mu,phi)))
  }
}

# zero inflated poisson
#dzipois<-function(x,z,lambda,log=FALSE){
#  if(log==FALSE){
#    ifelse(x==0,z+(1-z)*dpois(x,lambda),(1-z)*dpois(x,lambda))
#  }
#  else{
#    ifelse(x==0,log(z+(1-z)*dpois(x,lambda)),log((1-z)*dpois(x,lambda)))
#  }
#}

# zero inflated exponential distribution
#dziexp<-function(x,z,mean,log=FALSE){
#  if(log==FALSE){
#    ifelse(x==0,z+(1-z)*dexp(x,rate=1/mean),(1-z)*dexp(x,rate=1/mean))
#  }
#  else{
#    ifelse(x==0,log(z+(1-z)*dexp(x,rate=1/mean)),log((1-z)*dexp(x,rate=1/mean)))
#  }
#}

# zero inflated binomial distribution
# NOTE: currently hard-wired for an n of 5...
# updated this when I have time!
#dzibinom<-function(x,z,p,log=FALSE){
#  print("Warning in dzinbinom! Function currently hard-wired to use an n of 5!")
#  if(log==FALSE){
#    ifelse(x==0, (1-z)+(z)*dbinom(0,5,p), (z)*dbinom(x,5,p))
#  }
#  else{
#    ifelse(x==0, log((1-z)+(z)*dbinom(0,5,p)), log((z)*dbinom(x,5,p)))
#  }
#}


# mixture of 2 negative binomial distributions, weighted by parameter z.
#d2nbinom<-function(x,z,mu1,size1,mu2,size2,log=FALSE){
#  if(log==FALSE){
#    (z*dnbinom(x,mu=mu1,size=size1)+
#       (1-z)*dnbinom(x,mu=mu2,size=size2))
#  }
#  else{
#    log(
#      (z*dnbinom(x,mu=mu1,size=size1)+
#         (1-z)*dnbinom(x,mu=mu2,size=size2))
#    )
#  }
#}

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#### Zero inflated utilities for MLE fits ####

# set up zero/nonzero vector for a given data vector
#flatten<-function(x){ifelse(x==0,0,1)}

# initialize various functions mapping Reals to (0,1) 
#expit<-function(x){exp(x)/(1+exp(x))}
#logit<-function(x){log(x/(1-x))}	
#loglog<-function(x){exp(-exp(-x))}
#cloglog<-function(x){1-exp(-exp(-x))}
#cauchy<-function(x){0.5+(1/pi)*atan(x)}