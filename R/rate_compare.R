#' Unstratified and Stratified  Miettinen and Nurminen Test
#' 
#' @param formula a symbolic description of the model to be fitted, which has the form \code{response ~ treament}, where \code{response} is the numeric vector with values of 0 or 1 and \code{treatment} is the group information.
#' @param data an optional data frame, list or environment containing the variables in the model. If not found in data, the variables are taken from \code{environment (formula)}, typically the environment from which \code{rate_compare} is called.
#' @param delta a numeric value to set the difference of two group under the null.
#' @param weight_schema weighting schema used in stratified MN method. \code{"equal"} for equal weighting, \code{"ss"} for sample size weighting, \code{"cmh"} for Cochran Mantel-Haenszel's weights. Default is "ss". 
#' @param test a character string specifying the side of p-value, must be one of \code{"one.sided"}, or \code{"two.sided"}.
#' @param bisection the number of sections in the interval used in Bisection Method. Default is 100.
#' @param eps the level of precision. Default is eps=1e-06.
#' @param alpha pre-difined alpha level for Confidence Interval
#' @references Miettinen, O. and Nurminen, M, \emph{Comparative Analysis of Two Rates}. STATISTICS IN MEDICINE, 4:213-226, 1985.
#' 
#' @examples
#' ##To conduct the stratified MN analysis with sample size weights:
#' ana <- data.frame(
#'   treatment = c(rep(0,100),rep(1,100)),
#'   response  = c(rep(0,80),rep(1,20),rep(0,40),rep(1,60)),
#'   stratum   = c(rep(1:4,12),1,3,3,1,rep(1:4,12),rep(1:4,25))
#' )
#' rate_compare(response~treatment, data = ana)
#' @export

rate_compare <- function(formula, 
                         data,
                         delta = 0, 
                         weight_schema=c('ss','equal','cmh'),
                         test = c('one.sided','two.sided'),
                         bisection= 100, 
                         eps=1e-06,
                         alpha=0.05)
{
  test <- match.arg(test)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  response <- model.response(mf, "numeric")
  stratum_var <- untangle.specials(terms(formula,specials="strata"),"strata",1)$vars
  treatment <- mf[,2L]
  
  # Count the event
  if (identical(stratum_var,character(0))){
    rs_data <- cbind.data.frame(treatment,response)
    count_data_long <- rs_data %>%dplyr::group_by(treatment) %>%
      dplyr::summarise(N = n(),
                       S = sum(response,na.rm =TRUE),.groups='drop') 
    # Reconstruct the data
    count_data <- count_data_long %>% select(-S) %>%
      pivot_wider(values_from = N, names_from = treatment,names_glue="N{c(0,1)}")  %>%
      cbind(
        count_data_long %>% select(-N) %>%
          pivot_wider(values_from = S,names_from = treatment,names_glue="S{c(0,1)}")
      ) %>% ungroup() %>%
      dplyr::mutate(N = N1+N0,
                    C = S1+S0,
                    R1 = S1/N1,
                    R0 = S0/N0
      )
  }else { 
    stratum <- mf[stratum_var]
    names(stratum)<-"stratum"
    rs_data <- cbind.data.frame(treatment,response,stratum)
    weight_schema <- match.arg(weight_schema)
    count_data_long <- rs_data %>% dplyr::group_by(stratum, treatment) %>%
      dplyr::summarise(N = n(),
                       S = sum(response,na.rm=TRUE),.groups='drop') 
    # Reconstruct the data by strata
    count_data <- count_data_long %>% select(-S) %>%
      pivot_wider(values_from = N, names_from = treatment,names_glue="N{c(0,1)}") %>%
      full_join(
        count_data_long %>% select(-N) %>%
          pivot_wider(values_from = S, names_from = treatment,names_glue="S{c(0,1)}"),by='stratum'
      ) %>% ungroup() %>%
      dplyr::mutate(N = N1+N0,
                    C = S1+S0,
                    w = ifelse(rep(weight_schema=='equal',length(stratum)),1,ifelse(rep(weight_schema=='ss',length(stratum)),N/sum(N),(N0*N1/N)/sum(N0*N1/N))),
                    R1 = S1/N1,
                    R0 = S0/N0
      )
  }
  
  
  # start the analysis
  if (identical(stratum_var,character(0))){
    test_stat <- count_data %>%       
      dplyr::mutate(
        L3 = N,
        L2 = (N1+2*N0)*delta-N-C,
        L1 = (N0*delta-N-2*S0)*delta+C,
        L0 = S0*delta*(1-delta),
        
        q  = (L2/(3*L3))^3-L1*L2/(6*L3^2)+L0/(2*L3),
        sign = ifelse(q>0,1, -1),
        p  = sqrt((L2/(3*L3))^2 - L1/(3*L3))*sign,
        
        # calcualte R tilter
        temp =  q/(p^3),
        # to limit this temo within (-1,1)
        temp = pmax(pmin(temp, 1),-1),
        a  = (pi+acos(temp))/3,
        
        # Start to calculate R tilter
        R0t  = 2*p*cos(a)-L2/(3*L3),
        R1t  = R0t + delta,
        Vart = (R1t*(1-R1t)/N1+ R0t*(1-R0t)/N0)*(N/(N-1)),
      )%>% dplyr::summarise(
        R_diff = (S1/N1 - S0/N0),
        Z_score = (R_diff - delta) / sqrt(Vart)
      ) %>% {
        if(test == 'one.sided' ){
          dplyr::mutate(., Pval = ifelse(delta<=0, 1 - pnorm(Z_score), pnorm(Z_score)))
        } else if(test == "two.sided"){
          dplyr::mutate(., Pval = 1 - pchisq(Z_score^2,1))
        }
      }
  }else{
    test_stat <- count_data %>%  dplyr::mutate(
      L3 = N,
      L2 = (N1+2*N0)*delta-N-C,
      L1 = (N0*delta-N-2*S0)*delta+C,
      L0 = S0*delta*(1-delta),
      
      q  = (L2/(3*L3))^3-L1*L2/(6*L3^2)+L0/(2*L3),
      sign = ifelse(q>0,1, -1),
      p  = sqrt((L2/(3*L3))^2 - L1/(3*L3))*sign,
      
      # calcualte R tilter
      temp =  q/(p^3),
      # to limit this temo within (-1,1)
      temp = pmax(pmin(temp, 1),-1),
      a  = (pi+acos(temp))/3,
      
      # Start to calculate R tilter
      R0t  = 2*p*cos(a)-L2/(3*L3),
      R1t  = R0t + delta,
      Vart = (R1t*(1-R1t)/N1+ R0t*(1-R0t)/N0)*(N/(N-1)),
      # Start to calculate the chi-square
      R1_w = R1*w,
      R0_w = R0*w,
      Var_w = w^2*Vart
    ) %>% dplyr::summarise(
      # R1S = sum(R1_w),
      # R2S = sum(R0_w),
      R_diff = (sum(R1_w) - sum(R0_w)),
      Z_score = (R_diff - delta)/sqrt(sum(Var_w))
    ) %>% {
      if(test == 'one.sided'){
        dplyr::mutate(., Pval =ifelse(delta<=0, 1 - pnorm(Z_score),1-pnorm(Z_score)))
      } else if(test == "two.sided"){
        dplyr::mutate(., Pval = 1 - pchisq(Z_score^2,1))
      }
    }
  }
  # bisection function to find the roots
  BIroot<- function (f, a, b)
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
  if (identical(stratum_var,character(0))){
    func_D<-function(D) {test<-count_data %>%
      dplyr::mutate(
        L3= N,
        L2 = (N1+2*N0)*D-N-C,
        L1 = (N0*D-N-2*S0)*D+C,
        L0 = S0*D*(1-D),
        
        q  = (L2/(3*L3))^3-L1*L2/(6*L3^2)+L0/(2*L3),
        sign = ifelse(q>0,1, -1),
        p  = sqrt((L2/(3*L3))^2 - L1/(3*L3))*sign,
        # Adjust p
        p = ifelse(p>(-1e-20) & p<0,
                   p-1e-16,
                   ifelse(
                     p>=0 & p<(1e-20),
                     p+1e-16,
                     p
                   )
        ),
        # calculate R tilter
        temp =  q/(p^3),
        # to limit this temo within (-1,1)
        temp = pmax(pmin(temp, 1),-1),
        a  = (pi+acos(temp))/3,
        # Start to calculate R tilter
        R0t  = 2*p*cos(a)-L2/(3*L3),
        R1t  = R0t + D,
        Vart = (R1t*(1-R1t)/N1+ R0t*(1-R0t)/N0)*(N/(N-1)),
      ) %>%
      dplyr::summarise(
        R_diff = (S1/N1 - S0/N0),
        Chisq_obs = (R_diff - D)^2/Vart
      ) 
    return(test$Chisq_obs-qchisq(1-alpha,1))}
    
    CI<-BIroot(f=func_D,a=-0.99,b=0.99)
    CI<-CI[(abs(CI)<1)]
  }else{
    func_D<-function(D) {test<-count_data %>%
      dplyr::mutate(
        L3= N,
        L2 = (N1+2*N0)*D-N-C,
        L1 = (N0*D-N-2*S0)*D+C,
        L0 = S0*D*(1-D),
        
        q  = (L2/(3*L3))^3-L1*L2/(6*L3^2)+L0/(2*L3),
        sign = ifelse(q>0,1, -1),
        p  = sqrt((L2/(3*L3))^2 - L1/(3*L3))*sign,
        # Adust p
        p = ifelse(p>(-1e-20) & p<0,
                   p-1e-16,
                   ifelse(
                     p>=0 & p<(1e-20),
                     p+1e-16,
                     p
                   )
        ),
        # calculate R tilter
        temp =  q/(p^3),
        # to limit this temp within (-1,1)
        temp = pmax(pmin(temp, 1),-1),
        a  = (pi+acos(temp))/3,
        # Start to calculate R tilter
        R0t  = 2*p*cos(a)-L2/(3*L3),
        R1t  = R0t + D,
        Vart = (R1t*(1-R1t)/N1+ R0t*(1-R0t)/N0)*(N/(N-1)),
        # Start to calculate the chi-square
        R1_w = R1*w,
        R0_w = R0*w,
        Var_w = w^2*Vart
      ) %>%
      dplyr::summarise(
        R1S = sum(R1_w),
        R2S = sum(R0_w),
        Vs  =sum(Var_w)
      ) %>%  dplyr::mutate(
        R_diff = R1S - R2S,
        Chisq_obs = (R_diff - D)^2/Vs
      )
    return(test$Chisq_obs-qchisq(1-alpha,1))}
    
    CI<-BIroot(f=func_D,a=-0.99,b=0.99)
    CI<-CI[(abs(CI)<1)]
  }
  
  res <- as.data.frame(test_stat%>% dplyr::mutate(lower=CI[1],upper=CI[2]))
  names(res) <- tolower(res)
  
  res
}



