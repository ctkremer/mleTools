


#----------------------------------------------------------------#
#----------------------------------------------------------------#

# Function designed to help visualize regression fits to binary data.  Requires:
#	 y, a vector containing presence absence values as (0,1)
#	 x, a vector containing the covariate being modeled, same length as y
#	 n, an integer specifying the number of intervals to estimate 
#		  probabilities for.
# utility for visualizing the quality of a logistic regression fit
#regplot<-function(y,x,n,xlab='x',ylab='Presence/Absence'){
#  minx<-min(x,na.rm=T)
#  maxx<-max(x,na.rm=T)

#  plot(y~x,xlab=xlab,ylab=ylab)

#  bds<-seq(minx,maxx,(maxx-minx)/n)
#  zest<-c()
#  Test<-c()
#  for(i in 1:(length(bds)-1)){
#    res<-mean(y[(x>bds[i])&(x<bds[i+1])],na.rm=T)
#    zest<-append(zest,res)
#    Test<-append(Test,mean(bds[i:(i+1)]))
#  }	
#  points(Test,zest,col='red',pch=2,cex=0.75)
#}


