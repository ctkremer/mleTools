
#----------------------------------------------------------------#
#----------------------------------------------------------------#
#
#	 To do:
#		- improved passing of control parameters, etc to mle2 and optim (see functions match.call() and use of call in mle2 code.)
#		- clean up structure of function output
#   - output format is still a bit messy - learn more about nested lists.
#
#----------------------------------------------------------------#
#----------------------------------------------------------------#


#'	Maximum likelihood model fitting using a grid of starting guesses
#'	
#'	Designed to perform mle2 fits for a grid of initial parameter
#'	guesses, to aid situations where good starting values are either
#'	difficult to obtain, or when a large number of fits need to be
#'	conducted in an automated fasion. A large number of fits are 
#'	performed; from the sbuset of resulting models that successfully
#'	converge, the best model is selecte based on AIC comparison.
#'	
#'	This function is essentially a wrapper for mle2() from 
#'	Ben Bolker's bbmle package.
#'	
#'	@param minuslogl A negative log likelihood function; passed to mle2
#'	@param grids A list of multiple starting guesses for parameters found in minuslogl
#'	@param start A list of single starting guesses for parameters in minuslogl
#'	@param data A data frame containing the data used in the minuslogl function
#'	
#'	@return res.mat A matrix of coefficient estimates, model AIC values, and convergence status
#'	@return res.model An indexed list of mle2 objects, one for each fit, ordered as in res.mat
#'	@return res.best The best model selected from list of res.model
#'	
#' @examples See examples in grid.mle2_wexamples.R
#'
#' @export
#' @import bbmle
grid.mle2<-function(minuslogl,grids,start,data,...){
  
  #	print(list(...))
  
  if(length(grids)>=1){	# if grids have been supplied,
    
    # all combinations of grid variables and values
    grid.starts<-as.matrix(expand.grid(grids))
    ncombos<-dim(grid.starts)[[1]]
    
    # cycle through each combo
    res.mat<-matrix(NA,nrow=ncombos,ncol=I(length(start)+4))
    res.mod<-list()
    for(i in 1:dim(grid.starts)[[1]]){
      
      # some how need to match grid parameters to start lists.
      mod.start<-as.list(grid.starts[i,])	
      new.start<-start
      new.start[names(start) %in% names(mod.start)]<-mod.start
      
      pscale<-as.numeric(new.start)
      names(pscale)<-names(new.start)
      
      #			res.fit<-try(mle2(minuslogl=minuslogl,start=new.start,data=data),silent=T)
      
      res.fit<-try(mle2(minuslogl=minuslogl,start=new.start,data=data,...),silent=T)
      
      #			res.fit<-try(mle2(minuslogl=minuslogl,start=new.start,control=list(parscale=pscale),method="BFGS",data=data,...),silent=T)	
      
      if(class(res.fit)=="try-error"){
        # then fit failed; return NA values plus error message.
        res.mat[i,]<-c(i,unlist(new.start),NA,1,res.fit[1])
        res.mod[[i]]<-res.fit		# not 100% sure this is a good idea				
      }else{
        res.mat[i,]<-c(i,coef(res.fit),AIC(res.fit),convergeQ(res.fit),"")		
        res.mod[[i]]<-res.fit
      }			
    }
    colnames(res.mat)<-c("Index",names(new.start),"AIC","convergence","error.message")
  }else{	# otherwise, no grids; perform mle2 fit as usual
    #CAUTION!
    print("grid.mle2 does not currently support fits w/out at least one grids parameter")
    
    # pscale<-as.numeric(start)
    # names(pscale)<-names(start)
    # res.fit<-try(mle2(minuslogl=minuslogl,start=start,control=list(parscale=pscale),data=data,...))
    
    # if(class(res.fit)=="try-error"){
    # # then fit failed; return NA values plus error message.
    # res.mat[i,]<-c(i,rep(NA,length(new.start)),NA,1,res.fit[1])
    # res.mod[[i]]<-res.fit		# not 100% sure this is a good idea				
    # }else{
    # res.mat[i,]<-c(i,coef(res.fit),AIC(res.fit),convergeQ(res.fit),"")		
    # res.mod[[i]]<-res.fit			
    # }
    
    # res.mat<-c(coef(res.fit),AIC(res.fit))		
    # res.mod[[1]]<-res.fit
    # names(res.mat)<-c(names(coef(res.fit)),"AIC")
  }
  
  # select best AIC value?
  res.mat<-as.data.frame(res.mat,stringsAsFactors=F)
  res.mat$AIC<-as.numeric(res.mat$AIC)
  listAIC<-res.mat$AIC[res.mat$convergence==0]
  if(length(listAIC)==0){
    print("warning! Unable to select best model")
    res<-list(res.mat=res.mat,res.mod=as.list(res.mod),res.best=NA)
  }else{
    minAIC<-min(listAIC)
    best.mod<-as.numeric(res.mat$Index[which(res.mat$AIC==minAIC)])[1]
    res<-list(res.mat=res.mat,res.mod=as.list(res.mod),res.best=res.mod[[best.mod]])
  }
  #	print(res.mod[[best.mod]])
  res
}


#' Convergence test
#' 
#' Check whether a particular mle2 model converged during fitting
#' 
#' @param x An mle2 mode object
#' @return The convergence status of the model (1 or 0)
#' 
convergeQ<-function(x){
  if(class(x)!="mle2"){print("warning! convergeQ() not supported for non mle2 classes")}
  x@details$convergence
}
