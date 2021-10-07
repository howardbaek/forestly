#' Specific Adverse Events Table
#'
#' The function creates specific adverse events tables by AE term or grouped by AE SOC. The user can select whether displaying 
#' Total, confidence interval, or P-value columns.
#'
#' @param population_from A data frame to obtain population level variables. Typically an 'adsl' dataset.
#' @param observation_from A data frame to obtain observation level variables.
#' @param population_where A character string to define the criteria to select analysis population.
#' @param observation_where A character string to define the criteria to select analysis observation.
#' @param treatment_var A character string to define the variable of new column called "treatment".
#' @param treatment_order A character vector to define the treatment display order and label.
#' @param ae_var A character string to define the variable of new column called aedecod.
#' @param ae_grp A character string to define the variable of new column called aebodsys, which is used to group AE terms. Default is NULL, for which no AE grouping is performed.
#' @param display_total A logic value indicating whether displaying the total column. Default is FALSE.
#' @param display_ci A logic value indicating whether displaying the risk difference and its corresponding confidence interval. Default is FALSE.
#' @param display_pval A logic value indicating whether displaying p-value for the risk difference. Default is FALSE.
#' @param stratum_var A character string to define the variable of baseline stratum in 'population_from'. Only one 'stratum_var' is allowed.
#' Default is NULL, for which nonstratified MN analysis will be conducted. If specified, stratified MN analysis will be conducted.
#' @param title_text1 The part of the text string to appear in the title row.
#' @param subtitle_text A vector of text strings to appear in the subtitle row(s).
#' @param end_notes A vector of text strings to appear in the footnote.
#' @param output_name A text string to output the table, can be specified with the path together. If no path specified, the default path will be used to save the table.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' specific_ae(population_from=adsl %>% rename(TRTA=TRT01A),
#' observation_from=adae,
#' population_where = "ITTFL=='Y'",
#' observation_where = "TRTEMFL=='Y'",
#' treatment_var = "TRTA",
#' treatment_order =c("MK9999"="Xanomeline High Dose", "Placebo"="Placebo"),
#' ae_var = "AEDECOD",
#' ae_grp="AEBODSYS",
#' display_ci = FALSE,
#' display_total = TRUE,
#' display_pval = FALSE,
#' stratum_var = NULL,
#' title_text1 = "Participants With Adverse Events",
#' subtitle_text = c("(Incidence > 0% in More or More Treatment Group)","(APaT Population)"),
#' end_notes=c("Every subject is counted a single time for each applicable row and column.","Database Cutoff Date: 01SEP2021"),
#' output_name='s01specific0ae.rtf')
#' 



specific_ae <- function(population_from,
                        observation_from,
                        population_where ,
                        observation_where ,
                        treatment_var ,
                        treatment_order,
                        ae_var,
                        ae_grp=NULL,
                        display_total = FALSE,
                        display_ci = FALSE,
                        display_pval = FALSE,
                        stratum_var = NULL,
                        title_text1 ,
                        subtitle_text  ,
                        end_notes,
                        output_name){
  
  if (display_ci==TRUE&display_total==TRUE) stop('Cannot display difference estimates and total columns together.')
  if (display_total==FALSE&display_ci==FALSE&display_pval==TRUE) stop('Cannot display p-values without difference estimates.')
  
  if (!is.null(ae_grp)) title_text<-paste0(title_text1," by System Organ Class and Preferred Term")
  if (is.null(ae_grp)) title_text<-title_text1
  
  # Population Level Tidy Data
  pop <- tidy_population(population_from  = population_from,
                         population_where = population_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order,
                         stratum_var      = stratum_var,
                         baseline_var     = NULL)
  pop[["trt_label"]]<-ifelse(pop$treatment==names(treatment_order)[1],"exp","pbo")
  pop[["trt_label"]]<-factor(pop[["trt_label"]],levels=c("exp","pbo"))
  
  # Select the Desired Observation
  db <- tidy_observation(observation_from = observation_from,
                         observation_where = observation_where,
                         treatment_var    = treatment_var,
                         treatment_order  = treatment_order,
                         stratum_var = stratum_var)
  db[["trt_label"]]<-ifelse(db$treatment==names(treatment_order)[1],"exp","pbo")
  db[["trt_label"]]<-factor(db[["trt_label"]],levels=c("exp","pbo"))
  
  # Title Case the cell values
  db[["aedecod"]] <- tools::toTitleCase(tolower(db[[ae_var]])) 
  if (!is.null(ae_grp)) {db[["aebodsys"]] <- tools::toTitleCase(tolower(db[[ae_grp]]))}
  if (is.null(ae_grp)) {db[["aebodsys"]] <- "All"}
  
  # select the overlap pop(adsl) and db(adae)
  db <- subset(db, USUBJID %in% pop$USUBJID)
  
  # count the sample size of each arm and total
  pop_n <-  sapply(split(pop$trt_label, pop$trt_label), length)
  
  db_n <-   sapply(split(unique(db[,c("USUBJID","trt_label")])$trt_label, unique(db[,c("USUBJID","trt_label")])$trt_label), length)
  db_prop <- data.frame(t(format(round(db_n/pop_n*100,1),nsmall=1)))
  names(db_prop)<- paste(names(db_n),"_CI",sep="")
  db_out <- data.frame(data.frame(t(db_n)),db_prop)
  
  db_nonn <- pop_n - db_n
  db_nonprop<-data.frame(t(format(round(db_nonn/pop_n*100,1),nsmall=1)))
  names(db_nonprop)<- paste(names(db_nonn),"_CI",sep="")
  db_nonout <- data.frame(data.frame(t(db_nonn)),db_nonprop)
  
  t_pop <-  dplyr::bind_rows(pop_n,db_out,db_nonout)
  t_pop$aedecod <- c("Participants in population","with one or more adverse events","with no adverse events")
  t_pop$aebodsys <- "pop"
  
  pop_db <- merge(pop,db,by=c('USUBJID',"trt_label","stratum","treatment"))
  pop_n_str <- sapply(split(pop$trt_label, paste(pop$trt_label,pop$stratum,sep="_")), length)
  pop_db$trt_str <- factor(paste(pop_db$trt_label,pop_db$stratum,sep="_"),levels=names(pop_n_str))
  strata_level<-sub(".*_", "", names(pop_n_str))[1:length(unique(pop$stratum))]     
  uni <- unique(pop_db[,c("USUBJID","trt_label","stratum","trt_str")])
  db_n_str <- sapply(split(uni$trt_label, uni$trt_str), length)
  db_nonn_str <- pop_n_str - db_n_str
  
  pop_n0 <- pop_n_str[grepl(levels(pop$trt_label)[2],names(pop_n_str))]
  pop_n1 <- pop_n_str[grepl(levels(pop$trt_label)[1],names(pop_n_str))]
  db_s0 <- db_n_str[grepl(levels(pop$trt_label)[2],names(db_n_str))]
  db_s1 <- db_n_str[grepl(levels(pop$trt_label)[1],names(db_n_str))]
  db_non_s0 <- db_nonn_str[grepl(levels(pop$trt_label)[2],names(db_nonn_str))]
  db_non_s1 <- db_nonn_str[grepl(levels(pop$trt_label)[1],names(db_nonn_str))]
  
  output <- rbind(rate_compare_sum(n0=pop_n0,n1=pop_n1,x0=db_s0,x1=db_s1, strata=strata_level, delta = 0,weight = "ss",test = "one.sided",alpha = 0.05),
                  rate_compare_sum(n0=pop_n0,n1=pop_n1,x0=db_non_s0,x1=db_non_s1, strata=strata_level,delta = 0,weight = "ss",test = "one.sided",alpha = 0.05))
  estimate<-unlist(output[,1])
  pval<-format(round(unlist(output[,3]),3),nsmall=3)
  lower<- unlist(output[,4])
  upper<-unlist(output[,5])
  est<-paste(format(round(estimate*100,1),nsmall=1)," (",format(round(lower*100,1),nsmall=1),",",format(round(upper*100,1),nsmall=1),")",sep="")
  t_pop[["est"]] <- c(NA,est)
  t_pop[["pval"]] <- c(NA,pval)
  t_pop[["total"]] <- t_pop[["exp"]]+t_pop[["pbo"]]
  
  t_pop$total_CI <- c(NA,format(round(t_pop$total[-1]/t_pop$total[1]*100),nsmall=1))
  
  # count the ae count 
  ##count the soc
  if (!is.null(ae_grp)){
    soc_unique <- unique(pop_db[,c("USUBJID","trt_label","aebodsys")])
    soc_n <- data.frame(unclass(table(soc_unique$aebodsys,soc_unique$trt_label)))
    soc_prop <- data.frame(format(round(soc_n/pop_n*100,1),nsmall=1))
    names(soc_prop) <- paste(names(soc_prop),"_CI",sep="")
    soc <- data.frame(soc_n,soc_prop)
    soc$aedecod <- row.names(soc_n)
    soc$aebodsys <- row.names(soc_n)
    rownames(soc) <- NULL
    soc$order <- 0
  }
  if (is.null(ae_grp)){
    soc<- NULL
  }
  
  ##count the specific ae 
  ae_unique <- unique(db[,c("USUBJID","trt_label","aedecod","aebodsys")])
  ae_n <- data.frame(unclass(table(ae_unique$aedecod, ae_unique$trt_label)))
  ae_prop <- data.frame(format(round(ae_n/pop_n*100,1),nsmall=1))
  names(ae_prop) <- paste(names(ae_prop),"_CI",sep="")
  ae <- data.frame(ae_n,ae_prop)
  ae$aedecod <- row.names(ae)
  rownames(ae) <- NULL
  ae <- merge(unique(db[,c("aedecod","aebodsys")]),ae,by="aedecod")
  ae$order <- 1
  
  ##merge soc and specific ae
  soc_ae <- rbind(soc,ae)
  soc_ae <- subset(soc_ae[order(soc_ae$aebodsys,soc_ae$order,soc_ae$aedecod),],select=-order)
  
  ##calculate TOTAL
  soc_ae$total <- soc_ae[["exp"]]+soc_ae[["pbo"]]
  soc_ae$total_CI<- format(round(soc_ae$total/t_pop$total[1]*100),nsmall=1)
  
  ##calculate CI with or with stratified
  if (is.null(stratum_var)){
    output<-sapply(1:(dim(soc_ae)[1]), function(i) rate_compare_sum(n0=pop_n[["pbo"]],n1=pop_n[["exp"]],x0=soc_ae[["pbo"]][i],x1=soc_ae[["exp"]][i],strata=strata_level, delta = 0,weight = "ss",test = "one.sided",alpha = 0.05))
  }
  if (!is.null(stratum_var)){
    ##count the soc by stratum
    soc_unique <- unique(pop_db[,c("USUBJID","trt_label","aebodsys","stratum","trt_str")])
    soc_n <- data.frame(unclass(table(soc_unique$aebodsys, soc_unique$trt_str)))
    soc_n$aedecod <- row.names(soc_n)
    soc_n$aebodsys <- row.names(soc_n)
    rownames(soc_n) <- NULL
    soc_n$order <- 0
    
    ##count the specific ae by stratum
    ae_unique <- unique(pop_db[,c("USUBJID","trt_label","aedecod","aebodsys","stratum","trt_str")])
    ae_n <- data.frame(unclass(table(ae_unique$aedecod, ae_unique$trt_str)))
    ae_n$aedecod <- row.names(ae_n)
    rownames(ae_n) <- NULL
    ae_n <- merge(unique(pop_db[,c("aedecod","aebodsys")]),ae_n,by="aedecod")
    ae_n$order <- 1
    
    soc_ae_str <- rbind(soc_n,ae_n)
    soc_ae_str <- subset(soc_ae_str[order(soc_ae_str$aebodsys,soc_ae_str$order,soc_ae_str$aedecod),],select=-order)
    
    ae_s0 <- soc_ae_str[grepl(levels(pop_db$trt_label)[2],names(soc_ae_str))]
    ae_s1 <- soc_ae_str[grepl(levels(pop_db$trt_label)[1],names(soc_ae_str))]
    
    output<-sapply(1:(dim(soc_ae)[1]), function(i) rate_compare_sum(n0=pop_n0,n1=pop_n1,x0=unlist(ae_s0[i,]),x1=unlist(ae_s1[i,]),strata=strata_level, delta = 0,weight = "ss",test = "one.sided",alpha = 0.05))
    
  }
  estimate<-unlist(output[1,])
  pval<-format(round(unlist(output[3,]),3),nsmall=3)
  lower<- unlist(output[4,])
  upper<-unlist(output[5,])
  est<-paste(format(round(estimate*100,1),nsmall=1)," (",format(round(lower*100,1),nsmall=1),",",format(round(upper*100,1),nsmall=1),")",sep="")
  soc_ae$est <- est
  soc_ae$pval <- pval
  
  tbl_ae_spec <-  dplyr::bind_rows(t_pop,
                                   data.frame(aebodsys = "pop"), 
                                   soc_ae) %>% 
    dplyr::mutate(aedecod = ifelse(aedecod=="Participants in population"|aedecod == aebodsys , 
                                   aedecod, paste0("  ", aedecod)))

  #prepare reporting data
  if (display_total==FALSE&display_ci==FALSE){
    tbl_ae_spec <- tbl_ae_spec[,c(6,5,1,3,2,4)]
  }
  if (display_total==TRUE){
    tbl_ae_spec <- tbl_ae_spec[,c(6,5,1,3,2,4,9,10)]
  }
  if (display_ci==TRUE){
    if (display_pval==TRUE) tbl_ae_spec <- tbl_ae_spec[,c(6,5,1,3,2,4,7,8)]
    if (display_pval==FALSE) tbl_ae_spec <- tbl_ae_spec[,c(6,5,1,3,2,4,7)]  }

  
  #display bold text for group headers 
  n_row <- nrow(tbl_ae_spec)
  n_col <- ncol(tbl_ae_spec)
  id <- tbl_ae_spec$aebodsys == tbl_ae_spec$aedecod
  id <- ifelse(is.na(id), FALSE, id)
  
  text_format   <- ifelse(id, "b", "")
  
  #format output
  if (display_total==FALSE&display_ci==FALSE){
    if (!is.null(ae_grp)){
      tbl_ae_spec %>% 
        r2rtf::rtf_title(title_text, subtitle_text) %>%
        
        r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," "),
                      col_rel_width = c(3, rep(2, length(unique(pop$treatment))))
        ) %>%
        r2rtf::rtf_colheader(" | n | (%) | n | (%) ",
                      border_top = c("",rep("single",2*length(unique(pop$treatment)))),
                      border_bottom = "single",
                      border_left = c("single", rep(c("single", ""), length(unique(pop$treatment)))),
                      col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))))
        ) %>%
        
        r2rtf::rtf_body(
          col_rel_width = c(1, 3,  rep(1, 2*length(unique(pop$treatment)))),
          border_left = c("single","single",rep(c("single",""),length(unique(pop$treatment)))),
          text_justification = c("l","l", rep("c", 2*length(unique(pop$treatment)))),
          text_format = matrix(text_format, nrow = n_row, ncol = n_col), 
          page_by = "aebodsys", 
          pageby_row = "first_row") %>% 
        r2rtf::rtf_footnote(end_notes) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(output_name)
    }
    if (is.null(ae_grp)){
      tbl_ae_spec  %>% dplyr::select(-aebodsys)%>%
        r2rtf::rtf_title(title_text, subtitle_text) %>%
        
        r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," "),
                      col_rel_width = c(3, rep(2, length(unique(pop$treatment))))
        ) %>%
        r2rtf::rtf_colheader(" | n | (%) | n | (%) ",
                      border_top = c("",rep("single",2*length(unique(pop$treatment)))),
                      border_bottom = "single",
                      border_left = c("single", rep(c("single", ""), length(unique(pop$treatment)))),
                      col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))))
        ) %>%
        
        r2rtf::rtf_body(
          col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment)))),
          border_left = c("single",rep(c("single",""),length(unique(pop$treatment)))),
          text_justification = c("l", rep("c", 2*length(unique(pop$treatment))))) %>% 
        
        r2rtf::rtf_footnote(end_notes) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(output_name)
    }
  }
  if (display_ci==TRUE){
    if (display_pval==TRUE){
      if (!is.null(ae_grp)){
        tbl_ae_spec %>% 
          r2rtf::rtf_title(title_text, 
                    subtitle_text) %>%
          
          r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Difference in % vs ",levels(pop$treatment)[2]," "),
                        col_rel_width = c(3, rep(2, length(unique(pop$treatment))),3)
          ) %>%
          r2rtf::rtf_colheader(" | n | (%) | n | (%) | Estimate (95% CI) | p-value",
                        border_top = c("",rep("single",3*length(unique(pop$treatment)))),
                        border_bottom = "single",
                        border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single","single"),
                        col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),2,1)
          ) %>%
          
          r2rtf::rtf_body(
            col_rel_width = c(1, 3,  rep(1, 2*length(unique(pop$treatment))),2,1),
            border_left = c("single","single",rep(c("single",""),length(unique(pop$treatment))),"single","single"),
            text_justification = c("l","l", rep("c", 3*length(unique(pop$treatment)))),
            text_format = matrix(text_format, nrow = n_row, ncol = n_col), 
            page_by = "aebodsys", 
            pageby_row = "first_row") %>% 
          
          r2rtf::rtf_footnote(end_notes) %>%
          r2rtf::rtf_encode() %>%
          r2rtf::write_rtf(output_name)
      }
      if (is.null(ae_grp)){
        tbl_ae_spec %>% dplyr::select(-aebodsys)%>%
          r2rtf::rtf_title(title_text, 
                    subtitle_text) %>%
          
          r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Difference in % vs ",levels(pop$treatment)[2]," "),
                        col_rel_width = c(3, rep(2, length(unique(pop$treatment))),3)
          ) %>%
          r2rtf::rtf_colheader(" | n | (%) | n | (%) | Estimate (95% CI) | p-value",
                        border_top = c("",rep("single",3*length(unique(pop$treatment)))),
                        border_bottom = "single",
                        border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single","single"),
                        col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),2,1)
          ) %>%
          
          r2rtf::rtf_body(
            col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment))),2,1),
            border_left = c("single",rep(c("single",""),length(unique(pop$treatment))),"single","single"),
            text_justification = c("l", rep("c", 3*length(unique(pop$treatment))))) %>% 
          
          r2rtf::rtf_footnote(end_notes) %>%
          r2rtf::rtf_encode() %>%
          r2rtf::write_rtf(output_name)
      }
    }
    if (display_pval==FALSE){
      if (!is.null(ae_grp)){
        tbl_ae_spec %>% 
          r2rtf::rtf_title(title_text, 
                    subtitle_text) %>%
          
          r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Difference in % vs ",levels(pop$treatment)[2]," "),
                        col_rel_width = c(3, rep(2, length(unique(pop$treatment))),2)
          ) %>%
          r2rtf::rtf_colheader(" | n | (%) | n | (%) | Estimate (95% CI) ",
                        border_top = c("",rep("single",2*length(unique(pop$treatment))),"single"),
                        border_bottom = "single",
                        border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single"),
                        col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),2)
          ) %>%
          
          r2rtf::rtf_body(
            col_rel_width = c(1, 3,  rep(1, 2*length(unique(pop$treatment))),2),
            border_left = c("single","single",rep(c("single",""),length(unique(pop$treatment))),"single"),
            text_justification = c("l","l", rep("c", 2*length(unique(pop$treatment))),"c"),
            text_format = matrix(text_format, nrow = n_row, ncol = n_col), 
            page_by = "aebodsys", 
            pageby_row = "first_row") %>% 
          
          r2rtf::rtf_footnote(end_notes) %>%
          r2rtf::rtf_encode() %>%
          r2rtf::write_rtf(output_name)
      }
      if (is.null(ae_grp)){
        tbl_ae_spec %>% dplyr::select(-aebodsys)%>%
          r2rtf::rtf_title(title_text, 
                    subtitle_text) %>%
          
          r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Difference in % vs ",levels(pop$treatment)[2]," "),
                        col_rel_width = c(3, rep(2, length(unique(pop$treatment))),2)
          ) %>%
          r2rtf::rtf_colheader(" | n | (%) | n | (%) | Estimate (95% CI) ",
                        border_top = c("",rep("single",2*length(unique(pop$treatment))),"single"),
                        border_bottom = "single",
                        border_left = c("single", rep(c("single", ""), length(unique(pop$treatment))),"single"),
                        col_rel_width = c(3, rep(1, 2*length(unique(pop$treatment))),2)
          ) %>%
          
          r2rtf::rtf_body(
            col_rel_width = c(3,  rep(1, 2*length(unique(pop$treatment))),2),
            border_left = c("single",rep(c("single",""),length(unique(pop$treatment))),"single"),
            text_justification = c("l", rep("c", 2*length(unique(pop$treatment))),"c")) %>% 
          
          r2rtf::rtf_footnote(end_notes) %>%
          r2rtf::rtf_encode() %>%
          r2rtf::write_rtf(output_name)
      }
    }
  }
  if (display_total==TRUE){
    if (!is.null(ae_grp)){
      tbl_ae_spec %>% 
        r2rtf::rtf_title(title_text, subtitle_text) %>%
        
        r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Total"),
                      col_rel_width = c(3, rep(2, 1+length(unique(pop$treatment))))
        ) %>%
        r2rtf::rtf_colheader(" | n | (%) | n | (%) | n | (%) ",
                      border_top = c("",rep("single",2+2*length(unique(pop$treatment)))),
                      border_bottom = "single",
                      border_left = c("single", rep(c("single", ""), 1+length(unique(pop$treatment)))),
                      col_rel_width = c(3, rep(1, 2+2*length(unique(pop$treatment))))
        ) %>%
        
        r2rtf::rtf_body(
          col_rel_width = c(1, 3,  rep(1, 2+2*length(unique(pop$treatment)))),
          border_left = c("single","single",rep(c("single",""),1+length(unique(pop$treatment)))),
          text_justification = c("l","l", rep("c", 2+2*length(unique(pop$treatment)))),
          text_format = matrix(text_format, nrow = n_row, ncol = n_col), 
          page_by = "aebodsys", 
          pageby_row = "first_row") %>% 
        
        r2rtf::rtf_footnote(end_notes) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(output_name)
    }
    if (is.null(ae_grp)){
      tbl_ae_spec %>% dplyr::select(-aebodsys) %>%
        r2rtf::rtf_title(title_text, subtitle_text) %>%
        
        r2rtf::rtf_colheader(paste0(" | ", paste(levels(pop$treatment),collapse=" | ")," | Total"),
                      col_rel_width = c(3, rep(2, 1+length(unique(pop$treatment))))
        ) %>%
        r2rtf::rtf_colheader(" | n | (%) | n | (%) | n | (%) ",
                      border_top = c("",rep("single",2+2*length(unique(pop$treatment)))),
                      border_bottom = "single",
                      border_left = c("single", rep(c("single", ""), 1+length(unique(pop$treatment)))),
                      col_rel_width = c(3, rep(1, 2+2*length(unique(pop$treatment))))
        ) %>%
        
        r2rtf::rtf_body(
          col_rel_width = c(3,  rep(1, 2+2*length(unique(pop$treatment)))),
          border_left = c("single",rep(c("single",""),1+length(unique(pop$treatment)))),
          text_justification = c("l", rep("c", 2+2*length(unique(pop$treatment))))) %>% 
        
        r2rtf::rtf_footnote(end_notes) %>%
        r2rtf::rtf_encode() %>%
        r2rtf::write_rtf(output_name)
    }
  }
}

