#' Unstratified and Stratified  Miettinen and Nurminen Test
#' @param Treatment treament indicator
#' @param Response response indicator, 1 for responsder, 0 for non-responder
#' @param TRT_ord treatment order. The first one is control arm and the second one is experiment arm. Default is NULL
#' @param stratified Default is NULL for unstratified MN. If TRUE, stratified MN will be conducted
#' @param Stratum stratum information
#' @param delta delta under the null
#' @param weight_schema weighting schema used in stratified MN method. "equal" for equal weighting, "ss" for sample size weighting, "cmh" for Cochran Mantel-Haenszel's weights. Default is "ss". 
#' @param test a character string specifying the side of p-value, must be one of "right" (default), "left" or "two.sided".
#' @param num the number of sections in the interval used in Bisection Method. Default is 100.
#' @param eps the level of precision. Default is eps=1e-06.
#' @param alpha pre-difined alpha level for Confidence Interval
#' @export
# library(haven)
# adrs<-read_sas("//bardsar-test/mk7902-nsclc/prot008-ia1-dmc/dataanalysis/adrs.sas7bdat")
# adsl<-read_sas("//bardsar-test/mk7902-nsclc/prot008-ia1-dmc/dataanalysis/adsl.sas7bdat")
# adsl<-adsl[adsl$IA01FL=='Y'&adsl$TRT01PN !=7,c("USUBJID","TRT01P","STRATP")]
# adrs<-adrs[adrs$PARAMCD=='BORCFIRC',]
# adrs$response<-ifelse(adrs$AVALC %in% c('CR', 'PR'),1,0)
# adrs<-adrs[,c("USUBJID","response")]
# ana<-merge(adrs,adsl,by='USUBJID',all.y=TRUE)
# 
# 
# Treatment<-ana$TRT01P
# Response<-ana$response
# Stratum<-ana$STRATP
# library(dplyr)
# library(tidyr)
rate0compare <- function(Treatment, Response, Stratum, stratified=NULL, TRT_ord = c("Pembrolizumab+ Placebo","Pembrolizumab+ Ipilimumab"),
                         delta = 0, num= 100, eps=1e-06,weight_schema=c('ss','equal','cmh'),
                         test = c('right','two.sided','left'),alpha=0.05)
{
  test <- match.arg(test)
  response_data <- cbind.data.frame(Treatment,Response,Stratum)
  rs_data <- response_data %>%
  {
    if(is.null(TRT_ord)){
      .
    }else{
      dplyr::mutate(.,Treatment = factor(Treatment, levels = TRT_ord))
    }
  } 
  # Count the event
  if (is.null(stratified)){
    count_data_long <- rs_data %>%dplyr::group_by_at("Treatment") %>%
      dplyr::summarise(N = n(),
                S = sum(Response,na.rm =TRUE),.groups='drop') 
    # Reconstruct the data
    count_data <- count_data_long %>% select(-S) %>%
      spread(key = Treatment, value = N) %>% rename(N1 =as.character(TRT_ord[2]), N0 = as.character(TRT_ord[1])) %>%
      cbind(
        count_data_long %>% select(-N) %>%
          spread(key = Treatment, value = S) %>% rename(S1 =  as.character(TRT_ord[2]), S0 = as.character(TRT_ord[1]))
      ) %>% ungroup() %>%
      dplyr::mutate(N = N1+N0,
             C = S1+S0,
             R1 = S1/N1,
             R0 = S0/N0
      )
  }else { 
    weight_schema <- match.arg(weight_schema)
    count_data_long <- rs_data %>% dplyr::group_by_at(vars(one_of(c("Stratum", "Treatment")))) %>%
      dplyr::summarise(N = n(),
              S = sum(Response,na.rm=TRUE),.groups='drop') 
    # Reconstruct the data by strata
    count_data <- count_data_long %>% select(-S) %>%
      spread(key = Treatment, value = N) %>% rename(N1 = as.character(TRT_ord[2]), N0 = as.character(TRT_ord[1])) %>%
      full_join(
        count_data_long %>% select(-N) %>%
          spread(key = Treatment, value = S) %>% rename(S1 =as.character(TRT_ord[2]), S0 = as.character(TRT_ord[1])),by='Stratum'
      ) %>% ungroup() %>%
      dplyr::mutate(N = N1+N0,
             C = S1+S0,
             w = ifelse(rep(weight_schema=='equal',length(Stratum)),1,ifelse(rep(weight_schema=='ss',length(Stratum)),N/sum(N),(N0*N1/N)/sum(N0*N1/N))),
             R1 = S1/N1,
             R0 = S0/N0
      )}
  

  # start the analysis
  PI <- 3.14159265
 
  if (is.null(stratified)){
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
      if(test == 'right' | test == 'left'){
        dplyr::mutate(., Pval = 1 - pnorm(Z_score))
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
      if(test == 'right' | test == 'left'){
        dplyr::mutate(., Pval =ifelse(delta<=0, 1 - pnorm(Z_score),1-pnorm(Z_score)))
      } else if(test == "two.sided"){
        dplyr::mutate(., Pval = 1 - pchisq(Z_score^2,1))
      }
    }
  }
  # bisection function to find the roots
  BIroot<- function (f, a, b)
  {
    h = abs(b - a)/num
    i = 0
    j = 0
    a1 = b1 = 0
    roots<-c()
    while (i <= num) {
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
      print(paste("After", num, "loops lower or uppder limit was not found: change initial lower or upper bound."))
    else return(roots)
  }
  
  # # Start to calculate the Confidence Interval
if (is.null(stratified)){
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
      # calcualte R tilter
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
      # calcualte R tilter
      temp =  q/(p^3),
      # to limit this temo within (-1,1)
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

as.data.frame(test_stat%>% dplyr::mutate(lower=CI[1],upper=CI[2]))
}
