
# A1 ----------------------------------------------------------------------

return_responsible_indicators_a1<-function(score_matrix){
  eq4_df<-score_matrix %>% filter(!is.na(eq4))
  eq3_df<-score_matrix %>% filter(!is.na(eq3))
  eq2_df<-score_matrix %>% filter(!is.na(eq2))
  eq1_df<-score_matrix %>% filter(!is.na(eq1))
  gte_2<-score_matrix %>% filter(!is.na(eq2)|!is.na(eq3)|!is.na(eq4))
  gte_3<-score_matrix %>% filter(!is.na(eq3)|!is.na(eq4))
  eq4_length<-score_matrix %>% filter(!is.na(eq4)) %>% nrow()
  eq3_length<-score_matrix %>% filter(!is.na(eq3)) %>% nrow()
  eq2_length<-score_matrix %>% filter(!is.na(eq2)) %>% nrow()
  eq1_length<-score_matrix %>% filter(!is.na(eq1)) %>% nrow()
  gte_2_length<-gte_2 %>% nrow()
  gte_3_length<-gte_3 %>% nrow()
  if(eq3_length>=2){
    eq3_2x<-eq3_df
  }
  if(eq2_length>=3){
    eq2_3x<-eq2_df
  }
  if(eq2_length>=1){
    eq2_1x<-eq2_df
  }
  if(eq3_length==1){
    eq3_1x<-eq3_df
  }
  if(eq3_length==3){
    eq3_3x<-eq3_df
  }
  if(eq4_length>=2){
    eq4_2x<-eq4_df
  }
  e3_2_and_eq2_3_binded<-bind_rows(get0("eq3_2"),get0("eq2_3"))

  if(eq4_length>=3){
    responsible_indicator_matrix<-eq4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }

  if((eq4_length %in% c(2)| gte_3_length>=3)& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("gte_3")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(((eq4_length==1& gte_2_length>=1)| (gte_3_length>=2|gte_2_length>=3))& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("eq2_df"), get0("gte_3"),get0("gte_2")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)

  }
  if(eq1_length>=3&exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-eq1_df %>% mutate(responsible_indicators=indicator_label,
                                                    sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(eq1_length<3 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_group="A.1",responsible_indicators=NA, indicator_value=NA, county= county_temp)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
  return(responsible_indicator_matrix)
}


# A.2 & A.3 ---------------------------------------------------------------


return_responsible_indicators_a2_3b<-function(score_matrix, sub_pillar_group_symb, indicator_col){
  # score_matrix
  gte4_df<-score_matrix %>% filter(!!sym(indicator_col)>=4)
  gte3_df<-score_matrix %>% filter(!!sym(indicator_col)>=3)
  gte2_df<-score_matrix %>% filter(!!sym(indicator_col)>=2)
  gte1_df<-score_matrix %>% filter(!!sym(indicator_col)>=1)


  gte4_length<-gte4_df %>% nrow()
  gte3_length<-gte3_df %>% nrow()
  gte2_length<-gte2_df %>% nrow()
  gte1_length<-gte1_df %>%  nrow()

  gte4_gte2_df<-bind_rows(get0("gte4_df"),get0("gte2_df"))

  if(gte4_length>=2){
    responsible_indicator_matrix<-gte4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if((gte4_length>=1 |gte2_length>=2)  &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte2_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, sub_pillar_score=NA)}
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix)
  return(responsible_indicator_matrix)}



return_responsible_indicators_a2_3<-function(score_matrix, sub_pillar_group_symb="A.2", county){
  eq4_df<-score_matrix %>% filter(!is.na(eq4))
  eq3_df<-score_matrix %>% filter(!is.na(eq3))
  eq2_df<-score_matrix %>% filter(!is.na(eq2))
  eq1_df<-score_matrix %>% filter(!is.na(eq1))
  gte_1df<-score_matrix %>% filter(indicator_value>=1)
  gte_2<-score_matrix %>% filter(!is.na(eq2)|!is.na(eq3)|!is.na(eq4))
  gte_3<-score_matrix %>% filter(!is.na(eq3)|!is.na(eq4))
  eq4_length<-score_matrix %>% filter(!is.na(eq4)) %>% nrow()
  eq3_length<-score_matrix %>% filter(!is.na(eq3)) %>% nrow()
  eq2_length<-score_matrix %>% filter(!is.na(eq2)) %>% nrow()
  eq1_length<-score_matrix %>% filter(!is.na(eq1)) %>% nrow()
  gte_1_length<-gte_1df %>% nrow()
  gte_2_length<-gte_2 %>% nrow()
  gte_3_length<-gte_3 %>% nrow()
  if(eq3_length>=2){
    eq3_2x<-eq3_df
  }
  if(eq2_length>=3){
    eq2_3x<-eq2_df
  }
  if(eq2_length>=1){
    eq2_1x<-eq2_df
  }
  if(eq3_length==1){
    eq3_1x<-eq3_df
  }
  if(eq3_length==3){
    eq3_3x<-eq3_df
  }
  if(eq4_length>=2){
    eq4_2x<-eq4_df
  }
  e3_2_and_eq2_3_binded<-bind_rows(get0("eq3_2"),get0("eq2_3"))

  if(eq4_length>=2){
    responsible_indicator_matrix<-eq4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }

  if( gte_3_length>=2& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("gte_3")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if((eq4_length==1| gte_2_length>=2)& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("eq2_df"), get0("gte_3"),get0("gte_2")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)

  }
  if(gte_1_length>=2&exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte_1df %>% mutate(responsible_indicators=indicator_label,
                                                     sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, county)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
  return(responsible_indicator_matrix)
}




# A.4 ---------------------------------------------------------------------



return_responsible_indicators_a4<-function(score_matrix,sub_pillar_group_symb="A.4"){
  eq4_df<-score_matrix %>% filter(!is.na(eq4))
  eq3_df<-score_matrix %>% filter(!is.na(eq3))
  eq2_df<-score_matrix %>% filter(!is.na(eq2))
  eq1_df<-score_matrix %>% filter(!is.na(eq1))
  gte_2<-score_matrix %>% filter(!is.na(eq2)|!is.na(eq3)|!is.na(eq4))
  gte_3<-score_matrix %>% filter(!is.na(eq3)|!is.na(eq4))
  eq4_length<-score_matrix %>% filter(!is.na(eq4)) %>% nrow()
  eq3_length<-score_matrix %>% filter(!is.na(eq3)) %>% nrow()
  eq2_length<-score_matrix %>% filter(!is.na(eq2)) %>% nrow()
  eq1_length<-score_matrix %>% filter(!is.na(eq1)) %>% nrow()
  gte_2_length<-gte_2 %>% nrow()
  gte_3_length<-gte_3 %>% nrow()
  if(eq3_length>=2){
    eq3_2x<-eq3_df
  }
  if(eq2_length>=3){
    eq2_3x<-eq2_df
  }
  if(eq2_length>=1){
    eq2_1x<-eq2_df
  }
  if(eq3_length==1){
    eq3_1x<-eq3_df
  }
  if(eq3_length==3){
    eq3_3x<-eq3_df
  }
  if(eq4_length>=2){
    eq4_2x<-eq4_df
  }
  e3_2_and_eq2_3_binded<-bind_rows(get0("eq3_2"),get0("eq2_3"))

  if(eq4_length>=2){
    responsible_indicator_matrix<-eq4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }

  if((eq4_length>=1 | gte_3_length>=2)& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("gte_3")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if((eq4_length==1| gte_2_length>=2)& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("eq2_df"), get0("gte_3"),get0("gte_2")))
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)

  }
  if(eq1_length>=2&exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-eq1_df %>% mutate(responsible_indicators=indicator_label,
                                                    sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(eq1_length<3 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, county= county_temp)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
  return(responsible_indicator_matrix)
}

# A.5 ---------------------------------------------------------------------


return_responsible_indicators_a5<-function(score_matrix,sub_pillar_group_symb="A.5"){
  score_matrix<-score_matrix %>% mutate(mean_a5=mean(!!sym(indicator_col),na.rm = TRUE))
  eq4_df<-score_matrix %>% filter(!is.na(eq4))
  eq3_df<-score_matrix %>% filter(!is.na(eq3))
  eq2_df<-score_matrix %>% filter(!is.na(eq2))
  eq1_df<-score_matrix %>% filter(!is.na(eq1))
  gte_2<-score_matrix %>% filter(!is.na(eq2)|!is.na(eq3)|!is.na(eq4))
  gte_3<-score_matrix %>% filter(!is.na(eq3)|!is.na(eq4))
  eq4_length<-score_matrix %>% filter(!is.na(eq4)) %>% nrow()
  eq3_length<-score_matrix %>% filter(!is.na(eq3)) %>% nrow()
  eq2_length<-score_matrix %>% filter(!is.na(eq2)) %>% nrow()
  eq1_length<-score_matrix %>% filter(!is.na(eq1)) %>% nrow()
  gte_2_length<-gte_2 %>% nrow()
  gte_3_length<-gte_3 %>% nrow()
  if(eq3_length>=2){
    eq3_2x<-eq3_df
  }
  if(eq2_length>=3){
    eq2_3x<-eq2_df
  }
  if(eq2_length>=1){
    eq2_1x<-eq2_df
  }
  if(eq3_length==1){
    eq3_1x<-eq3_df
  }
  if(eq3_length==3){
    eq3_3x<-eq3_df
  }
  if(eq4_length>=2){
    eq4_2x<-eq4_df
  }
  e3_2_and_eq2_3_binded<-bind_rows(get0("eq3_2"),get0("eq2_3"))

  if(unique(score_matrix$mean_a5)>=3.5){
    responsible_indicator_matrix<-score_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }

  if(unique(score_matrix$mean_a5)>=2.5& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-score_matrix
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(unique(score_matrix$mean_a5)>=1.5& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-score_matrix
    responsible_indicator_matrix<-responsible_indicator_matrix %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)

  }
  if(unique(score_matrix$mean_a5)>=1 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-score_matrix %>% mutate(responsible_indicators=indicator_label,
                                                          sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, county= county_temp)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
  return(responsible_indicator_matrix)
}



# FSL ---------------------------------------------------------------------


return_responsible_indicators_FSL<-function(score_matrix){
  # score_matrix
  gte4_df<-score_matrix %>% filter(sub_pillar_score>=4)
  gte3_df<-score_matrix %>% filter(sub_pillar_score>=3)
  gte2_df<-score_matrix %>% filter(sub_pillar_score>=2)
  gte1_df<-score_matrix %>% filter(sub_pillar_score>=1)
  # bind_rows(get0("gte1_df"),get0("gte2_df"),get0("gte3_df"),get0("gte4_df"))

  gte4_length<-gte4_df %>% nrow()
  gte3_length<-gte3_df %>% nrow()
  gte2_length<-gte2_df %>% nrow()
  gte1_length<-gte1_df %>%  nrow()

  if(gte4_length>=2){
    responsible_indicator_matrix<-gte4_df %>%
      mutate(
        fsl_score=4) %>%
      tidyr::pivot_longer(3:(ncol(gte4_df)-2)) %>%
      filter(!is.na(value))
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(
        fsl_score=3) %>%
      tidyr::pivot_longer(3:(ncol(gte3_df)-2)) %>%
      filter(!is.na(value))
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte2_df %>%
      mutate(
        fsl_score=2) %>%
      tidyr::pivot_longer(3:(ncol(gte2_df)-2)) %>%
      filter(!is.na(value))
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(
        fsl_score=1) %>%
      tidyr::pivot_longer(3:(ncol(gte1_df)-2)) %>%
      filter(!is.na(value))
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix)
  return(responsible_indicator_matrix)}












score_matrix<-fsl_score_pre_matrix %>% filter(county=="YirolWest")
debugonce(return_responsible_indicators_FSL)
return_responsible_indicators_FSL(score_matrix = score_matrix)
return_responsible_indicators_FSL<-function(score_matrix){
  gte4_df<-score_matrix %>% filter(num_gte4>0)
  gte3_df<-score_matrix %>% filter(num_gte3>0)
  gte2_df<-score_matrix %>% filter(num_gte2>0)
  gte1_df<-score_matrix %>% filter(num_gte1>0)
  gte4_length<-score_matrix %>% filter(num_gte4>0) %>% nrow()
  gte3_length<-score_matrix %>% filter(num_gte3>0) %>% nrow()
  gte2_length<-score_matrix %>% filter(num_gte2>0) %>% nrow()
  gte1_length<-score_matrix %>% filter(num_gte1>0) %>% nrow()

  if(gte4_length>=2){
    responsible_indicator_matrix<-gte4_df %>%
      mutate(
        fsl_score=4) %>%
      select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(
        fsl_score=3) %>%
      select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte2_df %>%
      mutate(
        fsl_score=2) %>%
      select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(
        fsl_score=1) %>%
      select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
  return(responsible_indicator_matrix)}

int_db$A.fsl.score <-
  ifelse(rowSums(A.fsl >= 4, na.rm = TRUE) >= 2, 4,
         ifelse(rowSums(A.fsl >= 3, na.rm = TRUE) >= 2, 3,
                ifelse(rowSums(A.fsl >= 2, na.rm = TRUE) >= 2, 2,
                       ifelse(rowSums(A.fsl >= 1, na.rm = TRUE) >= 2, 1, NA))))


# return_responsible_indicators_a4<-function(score_matrix){
#   eq4_df<-score_matrix %>% filter(!is.na(eq4))
#   eq3_df<-score_matrix %>% filter(!is.na(eq3))
#   eq2_df<-score_matrix %>% filter(!is.na(eq2))
#   eq1_df<-score_matrix %>% filter(!is.na(eq1))
#   gte_2<-score_matrix %>% filter(!is.na(eq2)|!is.na(eq3)|!is.na(eq4))
#   gte_3<-score_matrix %>% filter(!is.na(eq3)|!is.na(eq4))
#   eq4_length<-score_matrix %>% filter(!is.na(eq4)) %>% nrow()
#   eq3_length<-score_matrix %>% filter(!is.na(eq3)) %>% nrow()
#   eq2_length<-score_matrix %>% filter(!is.na(eq2)) %>% nrow()
#   eq1_length<-score_matrix %>% filter(!is.na(eq1)) %>% nrow()
#   gte_2_length<-gte_2 %>% nrow()
#   gte_3_length<-gte_3 %>% nrow()
#   if(eq3_length>=2){
#     eq3_2x<-eq3_df
#   }
#   if(eq2_length>=3){
#     eq2_3x<-eq2_df
#   }
#   if(eq2_length>=1){
#     eq2_1x<-eq2_df
#   }
#   if(eq3_length==1){
#     eq3_1x<-eq3_df
#   }
#   if(eq3_length==3){
#     eq3_3x<-eq3_df
#   }
#   if(eq4_length>=2){
#     eq4_2x<-eq4_df
#   }
#   e3_2_and_eq2_3_binded<-bind_rows(get0("eq3_2"),get0("eq2_3"))
#
#   if(eq4_length>=2){
#     responsible_indicator_matrix<-eq4_df %>%
#       mutate(responsible_indicators=indicator_label,
#              sub_pillar_score=4) %>%
#       select(responsible_indicators,indicator_value, sub_pillar_score)
#   }
#
#   if( (eq4_length>=1 | gte_3_length>=2 )& exists("responsible_indicator_matrix")==FALSE){
#     responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("gte_3")))
#     responsible_indicator_matrix<-responsible_indicator_matrix %>%
#       mutate(responsible_indicators=indicator_label,
#              sub_pillar_score=3) %>%
#       select(responsible_indicators,indicator_value, sub_pillar_score)
#   }
#   if((gte_2_length>=2| gte_2_length>=2)& exists("responsible_indicator_matrix")==FALSE){
#     responsible_indicator_matrix<-bind_rows(list(get0("eq4_df"),get0("eq2_df"), get0("gte_3"),get0("gte_2")))
#     responsible_indicator_matrix<-responsible_indicator_matrix %>%
#       mutate(responsible_indicators=indicator_label,
#              sub_pillar_score=2) %>%
#       select(responsible_indicators,indicator_value, sub_pillar_score)
#
#   }
#   if(eq1_length>=2&exists("responsible_indicator_matrix")==FALSE){
#     responsible_indicator_matrix<-eq1_df %>% mutate(responsible_indicators=indicator_label,
#                                                     sub_pillar_score=1) %>%
#       select(responsible_indicators,indicator_value, sub_pillar_score)
#   }
#   else{
#     responsible_indicator_matrix=  data.frame(sub_pillar_group="A.4",responsible_indicators=NA, indicator_value=NA, county= county_temp)}
#   responsible_indicator_matrix<-distinct(responsible_indicator_matrix,responsible_indicators, .keep_all= TRUE)
#   return(responsible_indicator_matrix)
# }

