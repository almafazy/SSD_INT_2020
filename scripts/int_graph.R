library(dplyr)

int<-readr::read_csv("inputs/INT_DB_20200127.csv")
int %>% select(contains("score")) %>% colnames()

int %>% select(intersect(starts_with("D"),  contains(".score"))) %>% colnames() %>% dput()

health_sub_score<-c("C.ebola.score", "C.measles.score", "C.ari.score", "C.health.score",
                   "county.score","BC.awd.score", "BC.cholera.score", "BC.malaria.score")

nutrition_sub_indicators<-c("D.whz_gam.score")

int$B.wash.score
wash_sub_indicators<- c("B.clean_water_no_access_aok.score", "B.open_defecation_aok.score",
                   "BC.awd.score", "BC.cholera.score", "BC.malaria.score")

fsl_sub_pillars<-c("A.1.food_access.score",
              "A.2.agriculture.score",
              "A.3.livestock.score",
              "A.4.markets.score",
              "A.5.climate.score")

month_of_interest<- "2019-12-01"
int<-int %>%
  mutate(date= paste0(year, "-", month,"-01"))
int_monthly<- int %>% filter(date== month_of_interest)
essential_cols<-c("date", "county")
int_pillars<-int %>% select(A.ipc_food_insecurity, B.wash.score, C.health.score,D.nutrition.score)
int_fsl_sub_pillars<- int %>% select(fsl_scores) %>% colnames()

int %>% select(intersect(starts_with("A."), ends_with("score"))) %>% colnames() %>% dput()


fsl_indicators_with_sub_pillars<-c("A.1.hunger_severity.score", "A.1.food_source_wild_emergency.score",
  "A.1.food_coping_children_only.score", "A.1.food_coping_skip_days.score",
  "A.1.food_source_unsustainable.score", "A.2.land_inputs_no_access.score",
  "A.2.crop_production.score", "A.2.fall_army_worm.score", "A.3.livestock_no_access_possession.score",
  "A.3.livestock_sell.score", "A.3.livestock_disease_aok.score",
  "A.3.food_now_milk_dairy.score", "A.4.market_no_access.score",
  "A.4.prices_sorghum.score", "A.4.prices_bean.score", "A.5.ndvi.score",
  "A.1.food_access.score", "A.2.agriculture.score", "A.3.livestock.score",
  "A.4.markets.score", "A.5.climate.score", "A.fsl.score")
fsl_indicators<-fsl_indicators_with_sub_pillars[fsl_indicators_with_sub_pillars %in% fsl_sub_pillars==FALSE]
fsl_indicators<- c(fsl_indicators[-17],"A.5.rainfall_score")


# A.1 responsible indicators ----------------------------------------------



int_monthly_indicators_long<- int_monthly %>%
  mutate(A.5.rainfall_score=AB.rainfall.score) %>%
  select(essential_cols, fsl_indicators) %>%
  tidyr::pivot_longer(c(A.1.hunger_severity.score:A.5.ndvi.score,"A.5.rainfall_score"),
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
         indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
           butteR::remove_kobo_grouper(.),
    eq4=ifelse(indicator_value>= 4, indicator_label, NA),
    eq3=ifelse(indicator_value== 3, indicator_label, NA),
    eq2=ifelse(indicator_value==2, indicator_label, NA),
    eq1=ifelse(indicator_value== 1, indicator_label, NA)

         )


int_monthly_indicators_long<- int_monthly %>%
  mutate(A.5.rainfall_score=AB.rainfall.score) %>%
  select(essential_cols, fsl_indicators) %>%
  tidyr::pivot_longer(c(A.1.hunger_severity.score:A.5.ndvi.score,"A.5.rainfall_score"),
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
    indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
      butteR::remove_kobo_grouper(.))


int_monthly_indicators_long_score_matrix<-int_monthly_indicators_long %>%
  filter(county=="Magwi") %>%
  filter(sub_pillar_grouper=="A.1") %>%
  group_by(sub_pillar_grouper) %>%
  mutate(number_eq4=sum(!is.na(eq4)),
         number_eq3=sum(!is.na(eq3)),
         number_eq2=sum(!is.na(eq2)),
         number_eq1=sum(!is.na(eq1)))


# A.1 ---------------------------------------------------------------------
#perfect

a1_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.1") %>%
    group_by(sub_pillar_grouper) %>%
    mutate(number_eq4=sum(!is.na(eq4)),
           number_eq3=sum(!is.na(eq3)),
           number_eq2=sum(!is.na(eq2)),
           number_eq1=sum(!is.na(eq1)))


  a1_drivers[[county_temp]]<-return_responsible_indicators_a1(df_temp) %>% mutate(county=county_temp)
}

a1_drivers_df<-bind_rows(a1_drivers)
a1_drivers_check<-a1_drivers_df %>% left_join(int_monthly %>% select(county, A.1.food_access.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.1.food_access.score)
a1_drivers_check %>% select(sub_pillar_score,A.1.food_access.score) %>% data.frame()
a1_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.2 ---------------------------------------------------------------------


a2_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.2") %>%
    group_by(sub_pillar_grouper)




  # debugonce(return_responsible_indicators_a2_3b)
  a2_drivers[[county_temp]]<-return_responsible_indicators_a2_3b(score_matrix =df_temp,
                                                                sub_pillar_group_symb = "A.2",
                                                                indicator_col="indicator_value"
  ) %>% mutate(county=county_temp)
}

a2_drivers_df<-bind_rows(a2_drivers)

a2_drivers_check<-a2_drivers_df %>% left_join(int_monthly %>% select(county, A.2.agriculture.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.2.agriculture.score)
a2_drivers_check %>% select(county,sub_pillar_score,A.2.agriculture.score) %>% data.frame()
a2_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


#still need to deal with differences in NA
a2_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.2") %>%
    group_by(sub_pillar_grouper) %>%
    mutate(number_eq4=sum(!is.na(eq4)),
           number_eq3=sum(!is.na(eq3)),
           number_eq2=sum(!is.na(eq2)),
           number_eq1=sum(!is.na(eq1))) %>% mutate(county=county_temp)





  a2_drivers[[county_temp]]<-return_responsible_indicators_a2_3(score_matrix =df_temp,
                                                                sub_pillar_group_symb = "A.2"
                                                                  ) %>% mutate(county=county_temp)
}

a2_drivers_df<-bind_rows(a2_drivers)

a2_drivers_check<-a2_drivers_df %>% left_join(int_monthly %>% select(county, A.2.agriculture.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.2.agriculture.score)
a2_drivers_check %>% select(county,sub_pillar_score,A.2.agriculture.score) %>% data.frame()
a2_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()



# A.3 ---------------------------------------------------------------------
# still need to fix issues with NAs

a3_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.3") %>%
    group_by(sub_pillar_grouper) %>%
    mutate(number_eq4=sum(!is.na(eq4)),
           number_eq3=sum(!is.na(eq3)),
           number_eq2=sum(!is.na(eq2)),
           number_eq1=sum(!is.na(eq1)))




  a3_drivers[[county_temp]]<-return_responsible_indicators_a2_3(score_matrix = df_temp,
                                                                sub_pillar_group_symb = "A.3",county = county_temp) %>%
    mutate(county=county_temp)
}



a3_drivers_df<-bind_rows(a3_drivers)
a3_drivers_check<-a3_drivers_df %>% left_join(int_monthly %>% select(county, A.3.livestock.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.3.livestock.score)
a3_drivers_check %>% select(county,sub_pillar_score,A.3.livestock.score) %>% data.frame()
a3_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.4  --------------------------------------------------------------------
# There is one case where there is an NA in maxs and not in mine....

a4_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.4") %>%
    group_by(sub_pillar_grouper) %>%
    mutate(number_eq4=sum(!is.na(eq4)),
           number_eq3=sum(!is.na(eq3)),
           number_eq2=sum(!is.na(eq2)),
           number_eq1=sum(!is.na(eq1)))


  a4_drivers[[county_temp]]<-return_responsible_indicators_a4(df_temp,sub_pillar_group_symb = "A.4") %>% mutate(county=county_temp)
}

a4_drivers_df<-bind_rows(a4_drivers)

a4_drivers_check<-a4_drivers_df %>% left_join(int_monthly %>% select(county, A.4.markets.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.4.markets.score)
a4_drivers_check %>% select(sub_pillar_score, A.4.markets.score)
a4_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()

# A.5 ---------------------------------------------------------------------
# PERFECT

a5_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.5") %>%
    group_by(sub_pillar_grouper) %>%
    mutate(number_eq4=sum(!is.na(eq4)),
           number_eq3=sum(!is.na(eq3)),
           number_eq2=sum(!is.na(eq2)),
           number_eq1=sum(!is.na(eq1)))

# debugonce(return_responsible_indicators_a5)
  a5_drivers[[county_temp]]<-return_responsible_indicators_a5(df_temp,sub_pillar_group_symb = "A.5") %>% mutate(county=county_temp)
}



a5_drivers_df<-bind_rows(a5_drivers)

a5_drivers_check<-a5_drivers_df %>% left_join(int_monthly %>% select(county, A.5.climate.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.5.climate.score)
a5_drivers_check %>%select(sub_pillar_score ,A.5.climate.score) %>%  data.frame()
a5_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()




# BIND SUB PILLARS --------------------------------------------------------


a1_2_3_4_5_drivers<-bind_rows(a1_drivers_df,a2_drivers_df,a3_drivers_df,a4_drivers_df,a5_drivers_df)
score_label_matrix<-distinct(int %>% select(score=A.fsl.score,label=A.fsl.label) %>%
                               arrange(score))

a1_2_3_4_5_drivers<-a1_2_3_4_5_drivers %>% left_join(score_label_matrix, by= c("indicator_value"= "score"))
a1_2_3_4_5_drivers<-a1_2_3_4_5_drivers %>%
  mutate(resp_indicator_label = paste0(stringr::str_replace_all(responsible_indicators,
                                                                ".score",""),": ", label))



a1_2_3_4_5_drivers %>% data.frame() %>% View()
windows();a1_2_3_4_5_drivers %>%
  filter(county=="AweilCentre") %>%
  group_by(sub_pillar_grouper) %>%
  mutate(normalized_indicator_score=(indicator_value/sum(indicator_value))*sub_pillar_score) %>%
  ggplot(aes(x=sub_pillar_grouper,
             y=normalized_indicator_score,
             fill=responsible_indicators,
             label=resp_indicator_label)) +
  geom_bar(stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))


# AGGREGATE TO FSL --------------------------------------------------------
#NAs still a problem and value always equaling 4 , but  i think its giving the right indicators.
a1_2_3_4_5_drivers %>% data.frame() %>% View()



fsl_drivers<-list()
for(i in 1:length(unique(a1_2_3_4_5_drivers$county))){
  print(i)
  county_temp<-unique(a1_2_3_4_5_drivers$county)[i]
  sub_pillar_group_score_unique<-distinct(a1_2_3_4_5_drivers %>% filter(county==county_temp) %>% select(sub_pillar_grouper, sub_pillar_score))
  df_temp<-a1_2_3_4_5_drivers %>%
    filter(county==county_temp) %>%
    group_by(sub_pillar_grouper) %>%
    select(sub_pillar_grouper,county, responsible_indicators, sub_pillar_score) %>%
    tidyr::pivot_wider(names_from = responsible_indicators,values_from = sub_pillar_score) %>% data.frame() %>%
    right_join(sub_pillar_group_score_unique, by= "sub_pillar_grouper")

# debugonce(return_responsible_indicators_FSL)
  fsl_drivers[[county_temp]]<-return_responsible_indicators_FSL(df_temp) %>% mutate(county=county_temp)
}


fsl_drivers_df<-bind_rows(fsl_drivers)
int$A.fsl.score
fsl_drivers_check<-fsl_drivers_df %>% left_join(int_monthly %>% select(county, A.fsl.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.fsl.score)
fsl_drivers_check %>%select(sub_pillar_score ,A.fsl.score) %>%  data.frame()
fsl_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()



























fsl_drivers

int$A.fsl.score

sub_pillar_group_score_unique<-distinct(a1_2_3_4_5_drivers %>% filter(county=="Magwi") %>% select(sub_pillar_grouper, sub_pillar_score))
a1_2_3_4_5_drivers$sub_pillar_score<-as.character(a1_2_3_4_5_drivers$sub_pillar_score)
df_temp<-a1_2_3_4_5_drivers %>%
  filter(county=="Magwi") %>%
  group_by(sub_pillar_grouper) %>%
  select(sub_pillar_grouper,county, responsible_indicators, sub_pillar_score) %>%
  tidyr::pivot_wider(names_from = responsible_indicators,values_from = sub_pillar_score) %>% data.frame() %>%
   right_join(sub_pillar_group_score_unique, by= "sub_pillar_grouper")


return_responsible_indicators_FSL(df_temp)

dplyr::join

  left_join(a1_2_3_4_5_drivers %>% filter(county=="Magwi") %>%group_by(sub_pillar_grouper) %>% filter(sub_pillar_score==unique(sub_pillar_score)) select(sub_pillar_grouper, sub_pillar_score)) %>% View()


fsl_score_pre_matrix<-a1_2_3_4_5_drivers %>%
  filter(!is.na(sub_pillar_grouper)) %>%
  # mutate(sub_pillar_county=paste0(sub_pillar_grouper,county)) %>%
  group_by(sub_pillar_grouper,county) %>%
  mutate(unique_val=unique(as.character(sub_pillar_score)),
         length_distinct= n_distinct(sub_pillar_score)
         ) %>%
  summarise(unique_val= unique(unique_val))%>%
  mutate(gte_4=unique_val>=4,
         gte_3=unique_val>=3,
         gte_2=unique_val>=2,
         gte_1=unique_val>=1) %>%
  ungroup() %>% group_by(county) %>%
  summarise(num_gte4=sum(gte_4),
            num_gte3=sum(gte_3),
            num_gte2=sum(gte_2),
            num_gte1=sum(gte_1)
            ) %>%
  right_join(a1_2_3_4_5_drivers, by="county")
fsl_score_pre_matrix %>% filter(county=="YirolWest")
FSL_drivers<-list()
for(i in 1:length(unique(fsl_score_pre_matrix$county))){
  print(i)
  county_temp<-unique(fsl_score_pre_matrix$county)[i]
  df_temp<-fsl_score_pre_matrix %>%
    filter(county==county_temp) %>% filter(!is.na(sub_pillar_score))


  # debugonce(return_responsible_indicators_FSL)
  FSL_drivers[[county_temp]]<-return_responsible_indicators_FSL(df_temp) %>% mutate(county=county_temp)
}



FSL_drivers_df<-bind_rows(FSL_drivers)

a5_drivers_check<-a5_drivers_df %>% left_join(int_monthly %>% select(county, A.5.climate.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.5.climate.score)
a5_drivers_check %>%select(sub_pillar_score ,A.5.climate.score) %>%  data.frame()
a5_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()





a1_2_3_4_5_drivers %>%
  filter(!is.na(sub_pillar_grouper)) %>%
  mutate(sub_pillar_county=paste0(sub_pillar_grouper,county)) %>%
  group_by(sub_pillar_county) %>%
  mutate(unique_val=unique(as.character(sub_pillar_score)),
         length_distinct= n_distinct(sub_pillar_score),
         gte_4=unique_val>=4,
         gte_3=unique_val>=3,
         gte_2=unique_val>=3,
         gte_1=unique_val>=1) %>% data.frame() %>% View()


eq4=ifelse(indicator_value>= 4, indicator_label, NA),
eq3=ifelse(indicator_value== 3, indicator_label, NA),
eq2=ifelse(indicator_value==2, indicator_label, NA),
eq1=ifelse(indicator_value== 1, indicator_label, NA)








a1_2_drivers %>%
  filter(county=="Magwi") %>%
  group_by(sub_pillar_grouper) %>%
  mutate(normalized_indicator_score=(indicator_value/sum(indicator_value))*sub_pillar_score) %>%
  ggplot(aes(x=sub_pillar_grouper, y=sub_pillar_score)) +
  stat_summary(geom = "bar", fun.y = 'identity')

  ggplot(aes(x=sub_pillar_grouper, y=sub_pillar_score))+geom_bar(stat="identity")

?ggplot2::geom_bar






int_monthly %>% filter(county=="Raja") %>% select(A.1.food_access.score)
A.1.food_access <- c(
  "A.1.hunger_severity.score",
  "A.1.food_source_wild_emergency.score",
  "A.1.food_coping_children_only.score",
  "A.1.food_coping_skip_days.score",
  "A.1.food_source_unsustainable.score")




int_monthly$A.1.food_access.score <-
  ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 3, 4,
         ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 2 |
                  rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 3, 3,
                ifelse((rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 1 &
                          rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 1) |
                         rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 2 |
                         rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 3, 2,
                       ifelse(rowSums(A.1.food_access >= 1, na.rm = TRUE) >= 3, 1, NA))))

int_db$A.1.food_access.score <-
  ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 3, 4,
         ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 2 |
                  rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 3, 3,
                ifelse((rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 1 &

                          rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 1) |
                         rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 2 |
                         rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 3, 2,
                       ifelse(rowSums(A.1.food_access >= 1, na.rm = TRUE) >= 3, 1, NA))))


rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 1 &
  rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 1) |
  rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 2 |
  rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 3, 2,



int_monthly_indicators_long %>% group_by(sub_pillar_grouper,county) %>%
  filter(indicator_value==4) %>% data.frame()






int_monthly_sub_pillars_long<- int_monthly %>%
  select(essential_cols, fsl_scores) %>%
  tidyr::pivot_longer(A.1.food_access.score:A.5.climate.score,
                      names_to = "sub_pillar_name",
                      values_to = "sub_pillar_value") %>%
  mutate(sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", sub_pillar_name),
         )
int$A.3.livestock.score

int_monthly_indicators_and_sub_pillars_fsl<-int_monthly_indicators_long %>% left_join(int_monthly_sub_pillars_long, by=c("sub_pillar_grouper", "county"))

library(ggplot2)
windows();int_monthly_indicators_and_sub_pillars_fsl %>% filter(county=="Akobo") %>%
  ggplot(aes(x=sub_pillar_name, y=indicator_value,label=indicator_label))+geom_bar(stat = "identity", aes(fill=factor(indicator_name)))+
  geom_text(size = 3, position = position_stack())+
  scale_y_continuous(breaks=seq(1,10,by=1))

int_monthly_indicators_and_sub_pillars_fsl %>% filter(county=="Abiemnhom") %>% data.frame()

int_monthly_indicators_and_sub_pillars_fsl$indicator_value %>% range(na.rm=TRUE)

int$county %>% unique()















msna<-read.csv("../../data_MSNA.csv",stringsAsFactors = FALSE)
sample<- sf::st_read("../..","sample")

msna_sf<- sf::st_as_sf(x = msna,coords=c("lon", "lat"), crs=4326)
library(sf)
butteR::closest_distance_rtree(msna_sf, sample)
