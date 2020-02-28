# THIS IS FIRST ATTEMPT TO MAKE THE DRIVER EXTRACTION FUNCTIONS INTO ONE GENERALIZABLE FUNCTION
# SO FAR UNSUCCESSFUL


# A.1 ---------------------------------------------------------------------
hdm_23<-function(score_matrix, condition1= "gte4>=2"){
  # score_matrix
  gte4_df<-score_matrix %>% filter(indicator_value>=4)

  gte4<-gte4_df %>% nrow()

  eval(parse(text = condition1))
}



extract_drivers<-function(score_matrix,
                sub_pillar_group_symb,
                indicator_col,
                condition1= "gte4>=3",
                condition2="gte4>=2|gte3>=3",
                condition3= "(gte4>=1 & gte2>=1)|(gte3>=2|gte2>=3)",
                condition4= "gte1>=3"
                ){
  # score_matrix
  gte4_df<-score_matrix %>% filter(!!sym(indicator_col)>=4)
  gte3_df<-score_matrix %>% filter(!!sym(indicator_col)>=3)
  gte2_df<-score_matrix %>% filter(!!sym(indicator_col)>=2)
  gte1_df<-score_matrix %>% filter(!!sym(indicator_col)>=1)

  gte4<-gte4_df %>% nrow()
  gte3<-gte3_df %>% nrow()
  gte2<-gte2_df %>% nrow()
  gte1<-gte1_df %>%  nrow()
  vctrs::vec_cast.character()
  gte_all_df<-data.frame(gte= c("gte4","gte3","gte2", "gte1"), stringsAsFactors=FALSE)
  gte_all_df$gte=="gte3"

  gte4_gte2_df<-bind_rows(get0("gte4_df"),get0("gte2_df"))
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))

  gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", condition3, perl=T) %>% length()
  gsub(".*\\((.*)\\).", "\\1", condition3)
  cond1_combo<-paste0(paste0(gte_all_df$gte[stringr::str_detect (gte_all_df$gte,string = condition1)],collapse = "_"),"_df")
  cond2_combo<-paste0(paste0(gte_all_df$gte[stringr::str_detect (gte_all_df$gte,string = condition2)],collapse = "_"),"_df")

  conditions_args<-data.frame(
    condition1 =unlist(butteR::strsplit_on_multiple(condition1,separator = c("(",">=","(","|", "&"))) %>% trimws(),
    condition2 =unlist(butteR::strsplit_on_multiple(condition2,separator = c("(",">=","(","|", "&"))) %>% trimws(),
    condition3 =unlist(butteR::strsplit_on_multiple(condition3,separator = c("(",">=","(","|", "&"))) %>% trimws(),
    condition4 =unlist(butteR::strsplit_on_multiple(condition4,separator = c("(",">=","(","|", "&"))) %>% trimws(),
                              stringsAsFactors = FALSE)


  paste0paste0(args_from_condition3$gte[args_from_condition3$gte %in%  gte_all_df$gte],collapse="_")

  glue::glue_collapse(c(args_from_condition3$gte[args_from_condition3$gte %in%  gte_all_df$gte],"df"),sep = "_")

  cond3_combo<-paste0(paste0(asdf$gte[stringr::str_detect (asdf$gte,string = gte_all_df$gte)],collapse = "_"),"_df")
  stringr::str_detect (gte_all_df$gte,string = condition3)

  cond3_combo<-paste0(paste0(gte_all_df$gte[stringr::str_detect (gte_all_df$gte,string = condition3)],collapse = "_"),"_df")



  if(eval(parse(condition1))){
    responsible_indicator_matrix<-!!sym(cond1_combo) %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if((eval(parse(condition2))) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-!!sym(cond2_combo) %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }

  if((eval(parse(condition3)))  &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte2_gte3_gte2_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if((eval(parse(condition4))) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, sub_pillar_score=NA)}
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix)
  return(responsible_indicator_matrix)
  }



# A.2 & A.3 ---------------------------------------------------------------


hdm_23<-function(score_matrix, sub_pillar_group_symb, indicator_col, null_strata=NULL){
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
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))

  if(is.null(null_strata)==FALSE & county_temp %in% null_strata){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, sub_pillar_score=NA)}

  if(gte4_length>=2 &exists("responsible_indicator_matrix")==FALSE){
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
  return(responsible_indicator_matrix)
}


# A.4 ---------------------------------------------------------------------


hdm_4<-function(score_matrix, sub_pillar_group_symb, indicator_col){
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
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))

  if(gte4_length>=2){
    responsible_indicator_matrix<-gte4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if((gte4_length>=1|gte3_length>=2) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(gte2_length>=2  &exists("responsible_indicator_matrix")==FALSE){
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


# A.5 ---------------------------------------------------------------------

hdm_5<-function(score_matrix, sub_pillar_group_symb, indicator_col){
  # score_matrix
  score_matrix<-score_matrix %>% mutate(mean_a5=mean(!!sym(indicator_col),na.rm = TRUE))
  gte4_df<-score_matrix %>% filter(!!sym(indicator_col)>=4)
  gte3_df<-score_matrix %>% filter(!!sym(indicator_col)>=3)
  gte2_df<-score_matrix %>% filter(!!sym(indicator_col)>=2)
  gte1_df<-score_matrix %>% filter(!!sym(indicator_col)>=1)


  gte4_length<-gte4_df %>% nrow()
  gte3_length<-gte3_df %>% nrow()
  gte2_length<-gte2_df %>% nrow()
  gte1_length<-gte1_df %>%  nrow()

  gte4_gte2_df<-bind_rows(get0("gte4_df"),get0("gte2_df"))
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))

  if(unique(score_matrix$mean_a5)>=3.5){
    responsible_indicator_matrix<-gte4_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=4) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(unique(score_matrix$mean_a5)>=2.5& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=3) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(unique(score_matrix$mean_a5)>=1.5& exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte2_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=2) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(unique(score_matrix$mean_a5)>=1 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(responsible_indicators=indicator_label,
             sub_pillar_score=1) %>%
      select(responsible_indicators,indicator_value, sub_pillar_score)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=sub_pillar_group_symb,responsible_indicators=NA, indicator_value=NA, sub_pillar_score=NA)}
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix)
  return(responsible_indicator_matrix)}


# FSL AGGREGATE SCORE -----------------------------------------------------

hdm_fsl2<-function(score_matrix,score_type= "sector_score",
                   sector= "FSL"){
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
        !!score_type:=4) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(
        !!score_type:=3) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte2_df %>%
      mutate(
        !!score_type:=2) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(
        !!score_type:=1) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=NA,sub_pilar_score=NA, responsible_indicators=NA, indicator_value=NA) %>% mutate(!!score_type:=NA)
    }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix) %>% mutate(sector=sector)
  return(responsible_indicator_matrix)}




hdm_fsl<-function(score_matrix){
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
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "indicator",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(
        fsl_score=3) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "indicator",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte2_df %>%
      mutate(
        fsl_score=2) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "indicator",
                          values_to = "indicator_value") %>%
  filter(!is.na(indicator_value))
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(
        fsl_score=1) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "indicator",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=NA,sub_pilar_score=NA, fsl_score=NA, indicator=NA, indicator_value=NA)}
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix)
  return(responsible_indicator_matrix)}


# WASH --------------------------------------------------------------------
hdm_wash<-function(score_matrix,
                   score_type= "sector_score",
                   sector= "WASH",
                   indicator_col,
                   indicator_name_col= "indicator_name",
                   special_indicators= "BC.cholera.score"){
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
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))
  special_df<- score_matrix %>% filter(!!sym(indicator_name_col)%in% special_indicators)

  gte4_special_df<- special_df %>% filter(!!sym(indicator_col)>=4)
  gte4_special_length<- gte4_special_df %>% nrow()

  if((gte4_special_length>=1|gte4_length>=4)|(gte4_length>=2 & gte3_length>=2)){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=4) %>%
      select(responsible_indicators,indicator_value, score_type)
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if((gte4_length>=2|gte3_length>=2) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=3) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if((gte3_length>=1|gte2_length>=3) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_gte2_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=2) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if(gte1_length>=3 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=1) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-  data.frame(responsible_indicators=NA, indicator_value=NA) %>% mutate(!!score_type:=NA)
  }

  responsible_indicator_matrix<-distinct(responsible_indicator_matrix) %>% mutate(sector=sector)
  return(responsible_indicator_matrix)}


# HEALTH --------------------------------------------------------------------
hdm_health<-function(score_matrix,
                     score_type= "sector_score",
                     sector= "Health",
                     indicator_col,
                     indicator_name_col= "indicator_name",
                     special_indicators= c("BC.cholera.score","C.ebola.score")){
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
  gte4_gte3_df<-bind_rows(get0("gte4_df"),get0("gte3_df"))
  gte3_gte2_df<-bind_rows(get0("gte3_df"), get0("gte2_df"))
  gte4_gte2_gte3_gte2_df<-bind_rows(get0("gte4_gte2_df"),get0("gte3_gte2_df"))
  special_df<- score_matrix %>% filter(!!sym(indicator_name_col)%in% special_indicators)

  gte4_special_df<- special_df %>% filter(!!sym(indicator_col)>=4)
  gte4_special_length<- gte4_special_df %>% nrow()

  if((gte4_special_length>=1|gte4_length>=2)){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=4) %>%
      select(responsible_indicators,indicator_value, score_type)
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if((gte4_length>=1|gte3_length>=2) &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte4_gte3_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=3) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_gte2_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=2) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(responsible_indicators=!!sym(indicator_name_col),
             !!score_type:=1) %>%
      select(responsible_indicators,indicator_value, score_type)
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-  data.frame(responsible_indicators=NA, indicator_value=NA) %>% mutate(!!score_type:=NA)
  }

  responsible_indicator_matrix<-distinct(responsible_indicator_matrix) %>% mutate(sector=sector)
  return(responsible_indicator_matrix)
}

# INT AGGREGATE SCORE -----------------------------------------------------

hdm_int<-function(score_matrix,score_type= "int_score",
                  sector= "INT"){
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
        !!score_type:=4) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
    #select(sub_pillar_grouper,responsible_indicators,indicator_value, sub_pillar_score,fsl_score)
  }
  if(gte3_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte3_df %>%
      mutate(
        !!score_type:=3) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(gte2_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte2_df %>%
      mutate(
        !!score_type:=2) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(gte1_length>=2 &exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix<-gte1_df %>%
      mutate(
        !!score_type:=1) %>%
      tidyr::pivot_longer(3:(ncol(.)-2),
                          names_to = "responsible_indicators",
                          values_to = "indicator_value") %>%
      filter(!is.na(indicator_value))
  }
  if(exists("responsible_indicator_matrix")==FALSE){
    responsible_indicator_matrix=  data.frame(sub_pillar_grouper=NA,sub_pilar_score=NA, responsible_indicators=NA, indicator_value=NA) %>% mutate(!!score_type:=NA)
  }
  responsible_indicator_matrix<-distinct(responsible_indicator_matrix) %>% mutate(sector=sector)
  return(responsible_indicator_matrix)}






