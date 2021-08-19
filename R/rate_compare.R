#' Unstratified and Stratified  Miettinen and Nurminen Test
#' @param formula a symbolic description of the model to be fitted, which has the form \code{y ~ x} for unstratified MN test, and the form \code{y ~ x + strata(s)} for stratified MN test, where \code{y} is the numeric vector with values of 0 or 1, \code{x} is the group information, and \code{s} is the stratum information.
#' @param data an optional data frame, list or environment containing the variables in the model. If not found in data, the variables are taken from \code{environment (formula)}, typically the environment from which \code{rate_compare} is called.
#' @param delta a numeric value to set the difference of two group under the null.
#' @param weight_schema weighting schema used in stratified MN method. \code{"equal"} for equal weighting, \code{"ss"} for sample size weighting, \code{"cmh"} for Cochran Mantel-Haenszel's weights. Default is "ss". 
#' @param test a character string specifying the side of p-value, must be one of \code{"one.sided"}, or \code{"two.sided"}.
#' @param bisection the number of sections in the interval used in Bisection Method. Default is 100.
#' @param eps the level of precision. Default is eps=1e-06.
#' @param alpha pre-difined alpha level for Confidence Interval
#' @references Miettinen, O. and Nurminen, M, \emph{Comparative Analysis of Two Rates}. STATISTICS IN MEDICINE, 4:213-226, 1985.
#' @examples
#' ##To conduct the stratified MN analysis with sample size weights:
#' treatment <- c(rep('pbo',100),rep('exp',100))
#' response <- c(rep(0,80),rep(1,20),rep(0,40),rep(1,60))
#' stratum <- c(rep(1:4,12),1,3,3,1,rep(1:4,12),rep(1:4,25))
#' rate_compare(formula=response~factor(treatment,levels=c('pbo','exp'))+strata(stratum),delta = 0, weight_schema='ss',test = 'one.sided',alpha=0.05)
#' @export

rate_compare <- function(formula, data,delta = 0, weight_schema=c('ss','equal','cmh'),
                         test = c('one.sided','two.sided'),bisection= 100, eps=1e-06,alpha=0.05)
{
  test <- match.arg(test)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf <- model.frame(mf, drop.unused.levels=TRUE)
  response <- model.response(mf, "numeric")
  stratum <- mf[,attr(terms(formula,specials="strata"),"specials")$strata]
  
  # Count the event
  if (length(stratum)==0){
    treatment <- mf[,2L]
    strt <- sapply(split(response,treatment),sum)
    ntrt <- sapply(split(response,treatment),length)
    rtrt <- strt/ntrt
    trt <- names(ntrt)
  }
  if (length(stratum)!=0) { 
    treatment <- mf[,-attr(terms(formula,specials="strata"),"specials")$strata][,2L]
    strt <- sapply(split(response,paste(t(stratum),treatment,sep="_")),sum)
    ntrt <- sapply(split(response,paste(t(stratum),treatment,sep="_")),length)
    str <- sapply(strsplit(names(ntrt),"_"),"[",1)
    trt <- sapply(strsplit(names(ntrt),"_"),"[",2)
  }
  n0 <- ntrt[trt==trt[1]]
  n1 <- ntrt[trt==trt[2]]
  s0 <- strt[trt==trt[1]]
  s1 <- strt[trt==trt[2]]
  n <- n0+n1
  c <- s0+s1
  r1 <- s1/n1
  r0 <- s0/n0
  
  # start the analysis
  l3 <- n
  l2 <- (n1+2*n0)*delta-n-c
  l1 <- (n0*delta-n-2*s0)*delta+c
  l0 <- s0*delta*(1-delta)
  
  q  <- (l2/(3*l3))^3-l1*l2/(6*l3^2)+l0/(2*l3)
  sign <- ifelse(q>0,1, -1)
  p  <- sqrt((l2/(3*l3))^2 - l1/(3*l3))*sign
  
  # calcualte R tilter
  temp <-  q/(p^3)
  # to limit this temo within (-1,1)
  temp <- pmax(pmin(temp, 1),-1)
  a  <- (pi+acos(temp))/3
  
  # Start to calculate R tilter
  r0t  <- 2*p*cos(a)-l2/(3*l3)
  r1t  <- r0t + delta
  vart <- (r1t*(1-r1t)/n1+ r0t*(1-r0t)/n0)*(n/(n-1))
  
  if (length(stratum)==0){
    r_diff <- (r1 - r0)
    z_score <- (r_diff - delta) / sqrt(vart)
    pval <- switch(test,one.sided=ifelse(delta<=0, 1 - pnorm(z_score), pnorm(z_score)),two.sided=1 - pchisq(z_score^2,1))
  }
  if (length(stratum)!=0) {
    # Start to calculate the chi-square
    w <- switch (weight_schema,equal=rep(1,length(str)),ss=n/sum(n),cmh=(n0*n1/n)/sum(n0*n1/n))
    r1_w <- r1*w
    r0_w <- r0*w
    var_w <- w^2*vart
    r_diff <- (sum(r1_w) - sum(r0_w))
    z_score <- (r_diff - delta)/sqrt(sum(var_w))
    pval <- switch(test,one.sided=ifelse(delta<=0, 1 - pnorm(z_score), pnorm(z_score)),two.sided=1 - pchisq(z_score^2,1))
  }
  
  # bisection function to find the roots
  # f is the function for which the root is sought, a and b are minimum and maximum of the interval which contains the root from Bisection Method
  biroot<- function (f, a, b)
  {
    h = abs(b - a)/bisection
    i = 0
    j = 0
    a1 = b1 = 0
    roots<-c()
    while (i <= bisection) {
      a1 = a + i * h
      b1 = a1 + h
      if (f(a1) * f(b1) < 0) {
        repeat {
          if (abs(b1 - a1) < eps) 
            break
          x <- (a1 + b1)/2
          if (f(a1) * f(x) < 0) 
            b1 <- x
          else a1 <- x
        }
        j = j + 1
        roots[j]<-(a1 + b1)/2
      }
      i = i + 1
    }
    if (j == 0) 
      print(paste("After", bisection, "loops lower or uppder limit was not found: change initial lower or upper bound."))
    else return(roots)
  }
  
  # # Start to calculate the Confidence Interval
  func_d <- function(d) {
    l3= n
    l2 = (n1+2*n0)*d-n-c
    l1 = (n0*d-n-2*s0)*d+c
    l0 = s0*d*(1-d)
    
    q  = (l2/(3*l3))^3-l1*l2/(6*l3^2)+l0/(2*l3)
    sign = ifelse(q>0,1, -1)
    p  = sqrt((l2/(3*l3))^2 - l1/(3*l3))*sign
    # Adust p
    p = ifelse(p>(-1e-20) & p<0,
               p-1e-16,
               ifelse(
                 p>=0 & p<(1e-20),
                 p+1e-16,
                 p
               )
    )
    # calculate R tilter
    temp =  q/(p^3)
    # to limit this temp within (-1,1)
    temp = pmax(pmin(temp, 1),-1)
    a  = (pi+acos(temp))/3
    # Start to calculate R tilter
    r0t  = 2*p*cos(a)-l2/(3*l3)
    r1t  = r0t + d
    vart = (r1t*(1-r1t)/n1+ r0t*(1-r0t)/n0)*(n/(n-1))
    if (length(stratum)==0){        
      r_diff = (s1/n1 - s0/n0)
      chisq_obs = (r_diff - d)^2/vart
    }
    if (length(stratum)!=0){
      # Start to calculate the chi-square
      r1_w = r1*w
      r0_w = r0*w
      var_w = w^2*vart
      vs  =sum(var_w)
      
      r_diff = sum(r1_w) - sum(r0_w)
      chisq_obs = (r_diff - d)^2/vs
    }
    return(chisq_obs-qchisq(1-alpha,1))
  }
  
  ci <- biroot(f=func_d,a=-0.999,b=0.999)
  ci <- ci[(abs(ci)<1)]
  
  z <- data.frame(r_diff,z_score, pval, ci[1],ci[2])
  names(z)[c(4,5)] <- c("lower.limit","upper.limit")
  z
}


