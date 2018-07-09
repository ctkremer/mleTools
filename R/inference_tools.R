#----------------------------------------------------------------#
#----------------------------------------------------------------#

# Tools for Maximum Likelihood Estimation,
#
# Developed by Colin T. Kremer
# Initially for ELME course in 2015, 2017

#----------------------------------------------------------------#
#----------------------------------------------------------------#

# To-do:
# - add grid.mle2 to this package
# - add new methods for confidence bands around mle2 regressions using the delta method

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#' Fisher information confidence intervals
#'
#' Calculate 95 percent confidence intervals for model parameters, based
#' on Fisher Information.
#'
#' @param model An mle2 model fit
#' 
#' @return A 95 percent confidence interval
#' 
#' @export
confint.FI<-function(model){
  cfs<-coef(model)
  ses<-sqrt(diag(vcov(model)))	# standard errors
  lw<-cfs-1.96*ses
  up<-cfs+1.96*ses
  res<-cbind(lw,up)
  dimnames(res)<-list(names(cfs),c("2.5 %","97.5 %"))	
  res
}

#' Generalized R2 estimation, based on Cox & Snell 1989
#'
#' Comparisons across models invoking different error distributions
#' is not recommended at present.
#'
#' @param m1 An intercept-only model
#' @param m2 A more complex model
#' 
#' @return Generalized R2 value
#' 
#' @export
gen.R2<-function(m1,m2){
  n<-length(m1@data[[1]])
  1 - (exp(logLik(m1)[[1]]-logLik(m2)[[1]]))^(2/n)
}


