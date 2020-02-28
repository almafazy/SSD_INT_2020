library(ggplot2)
library(dplyr)
source("scripts/functions/hdm_drivers_extraction.R")

int<-readr::read_csv("inputs/INT_DB_20200127.csv")
int %>% select(contains("score")) %>% colnames()

int %>% select(intersect(starts_with("D"),  contains(".score"))) %>% colnames() %>% dput()

health_sub_score<-c("C.ebola.score", "C.measles.score", "C.ari.score", "C.health.score",
                    "county.score","BC.awd.score", "BC.cholera.score", "BC.malaria.score")

nutrition_sub_indicators<-c("D.whz_gam.score")


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
# int_fsl_sub_pillars<- int %>% select(fsl_scores) %>% colnames()

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



int_monthly_indicators_long<- int_monthly %>%
  mutate(A.5.rainfall_score=AB.rainfall.score) %>%
  select(essential_cols, fsl_indicators) %>%
  tidyr::pivot_longer(c(A.1.hunger_severity.score:A.5.ndvi.score,"A.5.rainfall_score"),
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
    # indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
      # butteR::remove_kobo_grouper(.),
    indicator_label=indicator_name


  )






# A.1 ---------------------------------------------------------------------
#perfect

a1_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.1") %>%
    group_by(sub_pillar_grouper)


  a1_drivers[[county_temp]]<-hdm_1(df_temp,
                                   sub_pillar_group_symb = "A.1",
                                   indicator_col="indicator_value") %>%
    mutate(county=county_temp)
}
int_mont

a1_drivers_df<-bind_rows(a1_drivers)
int_monthly %>% filter(county=="Fangak") %>% select(A.1.food_access.score)
int_monthly$A.4.markets.score
a1_drivers_check<-a1_drivers_df %>% left_join(int_monthly %>% select(county, A.1.food_access.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.1.food_access.score)
a1_drivers_check %>% select(sub_pillar_score,A.1.food_access.score) %>% data.frame()
a1_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.2 ---------------------------------------------------------------------
# I THINK THE ONLY REASON THE INT HAS SOME NAS THAT I DONT IS BECAUSE THERE IS A COUNTY
# BY COUNTY EXCLUSION BASED ON LIVELIHOOD ZONE.
livelihood_zone_agriculture_excluded <- c("Pibor", "KapoetaEast")
a2_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.2") %>%
    group_by(sub_pillar_grouper)




  # debugonce(return_responsible_indicators_a2_3b)
  a2_drivers[[county_temp]]<-hdm_23(score_matrix =df_temp,
                                    sub_pillar_group_symb = "A.2",
                                    indicator_col="indicator_value",
                                    null_strata = livelihood_zone_agriculture_excluded
  ) %>% mutate(county=county_temp)
}

a2_drivers_df<-bind_rows(a2_drivers)

a2_drivers_check<-a2_drivers_df %>% left_join(int_monthly %>% select(county, A.2.agriculture.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.2.agriculture.score)
a2_drivers_check %>% select(county,sub_pillar_score,A.2.agriculture.score) %>% data.frame()
a2_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.3 ---------------------------------------------------------------------

livelihood_zone_livestock_excluded <- c("Ezo", "Ibba", "Maridi", "MundriEast", "MundriWest", "Mvolo", "Nagero",
                                        "Nzara", "Tambura", "Yambio")
a3_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.3") %>%
    group_by(sub_pillar_grouper)


  # debugonce(return_responsible_indicators_a2_3b)
  a3_drivers[[county_temp]]<-hdm_23(score_matrix =df_temp,
                                    sub_pillar_group_symb = "A.3",
                                    indicator_col="indicator_value",
                                    null_strata = livelihood_zone_livestock_excluded
  ) %>% mutate(county=county_temp)
}



a3_drivers_df<-bind_rows(a3_drivers)
a3_drivers_check<-a3_drivers_df %>% left_join(int_monthly %>% select(county, A.3.livestock.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.3.livestock.score)
a3_drivers_check %>% select(county,sub_pillar_score,A.3.livestock.score) %>% data.frame()
a3_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.4 ---------------------------------------------------------------------

# need to include the markets_check... "CanalPigi"

a4_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  # county_temp<-"CanalPigi"
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.4") %>%
    group_by(sub_pillar_grouper)


  # debugonce(return_responsible_indicators_a2_3b)
  a4_drivers[[county_temp]]<-hdm_4(score_matrix =df_temp,
                                   sub_pillar_group_symb = "A.4",
                                   indicator_col="indicator_value"
  ) %>% mutate(county=county_temp)
}

a4_drivers_df<-bind_rows(a4_drivers)

a4_drivers_check<-a4_drivers_df %>% left_join(int_monthly %>% select(county, A.4.markets.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.4.markets.score)
a4_drivers_check %>% select(sub_pillar_score, A.4.markets.score)
a4_drivers_check%>% select(county,sub_pillar_score, A.4.markets.score,comparable_scores) #%>% filter(comparable_scores==FALSE)  %>% data.frame()


# A.5 ---------------------------------------------------------------------
int_monthly_indicators_long %>% filter(county=="Fangak") %>%  data.frame()
a5_drivers<-list()
for(i in 1:length(unique(int_monthly_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_indicators_long$county)[i]
  df_temp<-int_monthly_indicators_long %>%
    filter(county==county_temp) %>%
    filter(sub_pillar_grouper=="A.5") %>%
    group_by(sub_pillar_grouper)


  # debugonce(return_responsible_indicators_a2_3b)
  a5_drivers[[county_temp]]<-hdm_5(score_matrix =df_temp,
                                   sub_pillar_group_symb = "A.5",
                                   indicator_col="indicator_value"
  ) %>% mutate(county=county_temp)
}



a5_drivers_df<-bind_rows(a5_drivers)

a5_drivers_check<-a5_drivers_df %>% left_join(int_monthly %>% select(county, A.5.climate.score), by= "county") %>%
  mutate(comparable_scores= sub_pillar_score==A.5.climate.score)
a5_drivers_check %>%select(sub_pillar_score ,A.5.climate.score) %>%  data.frame()
a5_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()



# Combine Sub-Pillars -----------------------------------------------------
a1_2_3_4_5_drivers<-bind_rows(a1_drivers_df,a2_drivers_df,a3_drivers_df,a4_drivers_df,a5_drivers_df)

#GET SCORE 1-4 LABELS
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
#I THINK THE CONTRADICTIONS REMAINING ARE DUE TO NOT INCLUDING LIVELIHOOD EXCLUSION LIST.
a1_2_3_4_5_drivers %>% data.frame() %>% View()



fsl_drivers<-list()
for(i in 1:length(unique(a1_2_3_4_5_drivers$county))){
  print(i)
  county_temp<-unique(a1_2_3_4_5_drivers$county)[i]
  # county_temp<-"Fangak"
  sub_pillar_group_score_unique<-distinct(a1_2_3_4_5_drivers %>% filter(county==county_temp) %>% select(sub_pillar_grouper, sub_pillar_score))
  df_temp<-a1_2_3_4_5_drivers %>%
    filter(county==county_temp) %>%
    group_by(sub_pillar_grouper) %>%
    select(sub_pillar_grouper,county, responsible_indicators, sub_pillar_score) %>%
    tidyr::pivot_wider(names_from = responsible_indicators,values_from = sub_pillar_score) %>% data.frame() %>%
    right_join(sub_pillar_group_score_unique, by= "sub_pillar_grouper")

  # debugonce(hdm_fsl2)
  fsl_drivers[[county_temp]]<-hdm_fsl2(score_matrix = df_temp,score_type = "sector_score",sector = "FSL") %>% mutate(county=county_temp)
}
int_monthly$A.1.food_access.score
int_monthly %>% filter(county=="Fangak") %>% select(intersect(starts_with("A."),ends_with(".score")),A.fsl.score) %>% data.frame()
int_monthly %>% filter(county=="Fangak") %>% select(A.1.food_access.score)
fsl_drivers_df<-bind_rows(fsl_drivers)
fsl_drivers_df %>% head()
fsl_drivers_check<-fsl_drivers_df %>% left_join(int_monthly %>% select(county, A.fsl.score), by= "county") %>%
  mutate(comparable_scores= sector_score==A.fsl.score)
fsl_drivers_check %>%select(sector_score ,A.fsl.score) %>%  data.frame()
fsl_drivers_check %>%
  select(county,sector_score ,A.fsl.score,comparable_scores) %>%
  filter(comparable_scores==FALSE)  %>% data.frame()





# WASH --------------------------------------------------------------------


wash_indicators<-c("B.clean_water_no_access_aok.score", "B.open_defecation_aok.score",
                   "AB.rainfall.score", "BC.awd.score", "BC.cholera.score",
                   "BC.malaria.score")

int_monthly_wash_indicators_long<- int_monthly %>%
  select(essential_cols, wash_indicators) %>%
  tidyr::pivot_longer(c(wash_indicators),
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
    indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
      butteR::remove_kobo_grouper(.),


  )


wash_drivers<-list()
for(i in 1:length(unique(int_monthly_wash_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_wash_indicators_long$county)[i]
  df_temp<-int_monthly_wash_indicators_long %>%
    filter(county==county_temp )


  # debugonce(hdm_wash)
  wash_drivers[[county_temp]]<-hdm_wash(score_matrix =df_temp,
                                   indicator_col="indicator_value"
  ) %>% mutate(county=county_temp)
}

wash_drivers_df<-bind_rows(wash_drivers)

wash_drivers_check<-wash_drivers_df %>% left_join(int_monthly %>% select(county, B.wash.score), by= "county") %>%
  mutate(comparable_scores= sector_score==B.wash.score)
wash_drivers_check %>% select(sector_score, B.wash.score) %>% data.frame()
wash_drivers_check %>% filter(comparable_scores==FALSE)  %>% data.frame()
wash_drivers_df



# Health ------------------------------------------------------------------

health_indicators <- c( "C.ari.score",
                         "C.measles.score",
                         "C.ebola.score",
                         "BC.awd.score",
                         "BC.cholera.score",
                         "BC.malaria.score")

int_monthly_health_indicators_long<- int_monthly %>%
  select(essential_cols, health_indicators) %>%
  tidyr::pivot_longer(health_indicators,
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
    indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
      butteR::remove_kobo_grouper(.),
  )


health_drivers<-list()
for(i in 1:length(unique(int_monthly_health_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_health_indicators_long$county)[i]
  df_temp<-int_monthly_health_indicators_long %>%
    filter(county==county_temp )


  # debugonce(hdm_health)
  health_drivers[[county_temp]]<-hdm_health(score_matrix =df_temp,
                                        indicator_col="indicator_value"
  ) %>% mutate(county=county_temp)
}

health_drivers_df %>% filter( county=="Twic")
health_drivers_df<-bind_rows(health_drivers)

health_drivers_check<-health_drivers_df %>% left_join(int_monthly %>% select(county, C.health.score), by= "county") %>%
  mutate(comparable_scores= sector_score ==C.health.score)
health_drivers_check %>% select(sector_score , C.health.score) %>% data.frame()
health_drivers_check %>% filter(comparable_scores==FALSE)

# NUTRITION ---------------------------------------------------------------

nutrition_indicators <- c( "D.ipc_malnutrition",
                        "D.ipc_malnutrition_projection",
                        "D.ipc_malnutrition")

int_monthly_nutrition_indicators_long<- int_monthly %>%
  select(essential_cols, nutrition_indicators) %>%
  tidyr::pivot_longer(nutrition_indicators,
                      names_to = "indicator_name",
                      values_to = "indicator_value") %>%
  mutate(
    sub_pillar_grouper=sub( "(^[^.]+[.][^.]+)(.+$)", "\\1", indicator_name),
    indicator_label=butteR::remove_kobo_grouper( indicator_name) %>%
      butteR::remove_kobo_grouper(.),
  )
nutrition_drivers<-list()
for(i in 1:length(unique(int_monthly_nutrition_indicators_long$county))){
  print(i)
  county_temp<-unique(int_monthly_nutrition_indicators_long$county)[i]
  df_temp<-int_monthly_nutrition_indicators_long %>%
    filter(county==county_temp )


  # debugonce(hdm_health)
  nutrition_drivers[[county_temp]]<-df_temp %>% filter(!is.na(indicator_value))

}

nutrition_drivers_df<-bind_rows(nutrition_drivers)
int$D.nutrition.score
nutrition_drivers_check<-nutrition_drivers_df %>% left_join(int_monthly %>% select(county, D.nutrition.score), by= "county") %>%
  mutate(comparable_scores= indicator_value ==D.nutrition.score)

nutrition_drivers_check %>% select(indicator_value , D.nutrition.score) %>% data.frame()
nutrition_drivers_check %>% filter(comparable_scores==FALSE)


# BIND TOGETHER CONTRIBUTING FACTORS --------------------------------------

nutrition_drivers_df2<- nutrition_drivers_df %>% select(responsible_indicators=indicator_name, indicator_value, county) %>%
  mutate(sector_score=indicator_value,
         sector= "Nutrition")
colnames(nutrition_drivers_df2)
colnames(health_drivers_df)
fsl_wash_health_nutrition_drivers_list<-list(wash_drivers_df,health_drivers_df,fsl_drivers_df,nutrition_drivers_df2)
fsl_wash_health_nutrition_drivers_df<-bind_rows(fsl_wash_health_nutrition_drivers_list)

fsl_wash_health_nutrition_drivers_df %>% colnames()
score_label_matrix<-distinct(int %>% select(score=A.fsl.score,label=A.fsl.label) %>%
                               arrange(score))


fsl_wash_health_nutrition_drivers_df<-fsl_wash_health_nutrition_drivers_df %>%
  left_join(score_label_matrix, by= c("indicator_value"= "score"))
fsl_wash_health_nutrition_drivers_df %>% colnames()

fsl_wash_health_nutrition_drivers_df<-fsl_wash_health_nutrition_drivers_df %>%
  mutate(resp_indicator_shortened = stringr::str_replace_all(responsible_indicators,
                                                                ".score",""))


library(ggthemes)
reach_grey_scale<- c("#eff0f1ff",'#e4e6e6ff',"#c7c8c9ff", "#8f8f90ff")


# fsl_wash_health_nutrition_drivers_df %>% View()
fsl_wash_health_nutrition_drivers_df$responsible_indicators[fsl_wash_health_nutrition_drivers_df$responsible_indicators %in%
  (int %>% select(ends_with("score")) %>% colnames() )==FALSE]
fsl_wash_health_nutrition_drivers_df$label<- forcats::fct_expand(fsl_wash_health_nutrition_drivers_df$label,"Low","Moderate", "High", "Very High")



indicators_raw<-c(fsl_indicators,wash_indicators,health_indicators,nutrition_indicators)
# data.frame(indicator=indicators_raw, indicator_label= "") %>% write.csv("indicator_scores_INT_need_labels.csv")


# read in graph labels from jack - they are wrong
graph_labels<-read.csv("inputs/indicator_labels_for_int_graph_ZA.csv")
graph_labels<-read.csv("inputs/indicator_scores_INT_with_labels.csv")
graph_labels<-graph_labels %>% distinct()
#THESE ARE THE VALUES THAT I NEED THE LABELS FOR

fsl_wash_health_nutrition_drivers_df$responsible_indicators
fsl_wash_health_nutrition_drivers_df %>% filter(county=="Twic", sector=="Health")



fsl_wash_health_nutrition_drivers_df<-fsl_wash_health_nutrition_drivers_df %>%
  left_join(graph_labels, by= c("responsible_indicators"="indicator"))
fsl_wash_health_nutrition_drivers_df %>% filter(is.na(indicatorlabel)) %>% select(responsible_indicators)


# barplot examples --------------------------------------------------------
rgb(238,88,89,maxColorValue = 255)
asdf<- fsl_wash_health_nutrition_drivers_df%>%
  filter(county=="Magwi") %>%
  mutate(sector_score_label= factor(sector_score,levels=1:4,labels = c("low","medium","high","veryhigh"),ordered=T)
  )
asdf$sector_score_label %>% class()
magwi_barplot_example_dodge<-
  fsl_wash_health_nutrition_drivers_df %>%
  filter(county=="Magwi") %>%
  mutate(
    # sector_score_label= factor(sector_score,levels=1:4,labels = c("Low","Moderate","High","Very High"),ordered=T)
  ) %>%
  ggplot(aes(x=sector,
             y=sector_score
  )) +
  geom_bar(stat="identity", position="dodge", fill="#EE5859", colour="black")+
  scale_y_continuous(labels=c("Low","Moderate", "High", "Very High"), breaks=c(1:4),limits=c(0,4))+
  labs( y= "Sectoral INT Score")+
  theme(
    axis.text.x = element_text(angle=30, margin=margin(t=5,r=0, b=0, l=0), size=8),
    axis.text.y=element_text(angle=90, size=8),
    axis.title.y = element_text(size=8),
    axis.title.x = element_blank(),
    # text= element_text(size=6),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = 'white',colour=NA),
    legend.position = "none"
  )

magwi_barplot_example_dodge
# 134.2, 91.6
ggsave("magwi_example.pdf", width = 134.2, height = 91.6, units = "mm")
magwi_barplot_example_stacked<-fsl_wash_health_nutrition_drivers_df %>%
  filter(county=="Magwi") %>%
  group_by(sector) %>%
  mutate(normalized_indicator_score=(indicator_value/sum(indicator_value))*sector_score) %>%
  ggplot(aes(x=sector,
             y=normalized_indicator_score
             )) +

  geom_bar(stat="identity", colour="black", aes(fill=label))+

  # scale_fill_manual(values= reach_grey_scale)+
  scale_fill_grey(start=0.9, end=0.7)+
  geom_text(aes(label=paste0(responsible_indicators,":\n",label)), size = 2, position = position_stack(vjust = 0.5))+
  labs( y= "Sectoral INT Score")+
  # theme_clean()+
  theme(
    axis.text.x = element_text(angle=30, margin=margin(t=5,r=0, b=0, l=0), size=8),
    axis.text.y=element_text(angle=90, size=8),
    axis.title.y = element_text(size=8),
    axis.title.x = element_blank(),
    # text= element_text(size=6),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = 'white',colour=NA),
    legend.position = "none"
    # #legend.justification = c("right", "top"),
    # legend.background = element_rect(fill=alpha('white', 0.7),colour="white", size = 0.5),
    # legend.text = element_text(size=6),
    # legend.key=element_blank(),
    # legend.title = element_blank()
    # axis.text.x=element_text(margin=margin(t=-10,r=0, b=0, l=0))
  )

# ggsave("magwi_example.pdf", width = 135, height = 150, units = "mm")

for(i in 1: length(unique(fsl_wash_health_nutrition_drivers_df$county))){
  county_temp<-unique(fsl_wash_health_nutrition_drivers_df$county)[i]

  int_plot<-fsl_wash_health_nutrition_drivers_df %>%
    filter(county==county_temp) %>%
    ggplot(aes(x=sector,
               y=sector_score
    )) +
    geom_bar(stat="identity", position="dodge", fill="#EE5859", colour="black")+
    scale_y_continuous(labels=c("Low","Moderate", "High", "Very High"), breaks=c(1:4),limits=c(0,4))+
    labs( y= "Sectoral INT Score")+
    theme(
      axis.text.x = element_text(angle=30, margin=margin(t=5,r=0, b=0, l=0), size=8),
      axis.text.y=element_text(angle=90, size=8),
      axis.title.y = element_text(size=8),
      axis.title.x = element_blank(),
      # text= element_text(size=6),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background = element_rect(fill = 'white',colour=NA),
      legend.position = "none"
    )

      ggsave(filename = paste0("product_related/datamerge_inputs/int_driver_graphs/",county_temp,".pdf"),
                               width = 134.2, height = 91.6, units = "mm")

    }


#MAKE FOR DATA MERGE
int_drivers_dm<-fsl_wash_health_nutrition_drivers_df %>%
  mutate(
    label_with_score=paste0(label %>% toupper(),": ",indicatorlabel)
    ) %>%
  group_by(county, sector) %>%
  mutate(
    sector_indic_num=paste0(sector,"_",1:n())
         ) %>%
  ungroup() %>%
  select(county,label_with_score, sector_indic_num ) %>%
  tidyr::pivot_wider(values_from = label_with_score,names_from = sector_indic_num) %>%
  mutate(county_simple= butteR:::tolower_rm_special(county),
         `@int_graph`=paste0(".\\","datamerge_inputs\\int_driver_graphs\\",county,".pdf"),
         `@int_graph1`=paste0(".\\","int_driver_graphs\\",county,".pdf"))
int_drivers_dm$`@int_graph`

dm1<-readr::read_csv(file = "inputs/merge_me_use.csv")

dm2<- dm1 %>%
  mutate(
    county_simple= butteR:::tolower_rm_special(County_header),

    ) %>%
  left_join(int_drivers_dm)

# write.csv(dm2,"inputs/datamerge2_ZA.csv")
write.csv(dm2,"product_related/datamerge_inputs/datamerge_ZA.csv", na = "",row.names = F)




combination_table<-gtools::combinations(4, 4, 1:4, repeats.allowed = T) %>% data.frame()
combination_table$mean_rows= rowMeans(combination_table)
combination_table %>%
  mutate(
    threshold= case_when(
      mean_rows >=3.5 ~4,
      mean_rows>=2.5 ~3,
      mean_rows>=1.5~2,
      TRUE ~1
    )
  )

int_db$county.score <-
  ifelse(int_db$county.score >= 3.5, 4,
         ifelse(int_db$county.score >= 2.5, 3,
                ifelse(int_db$county.score >= 1.5, 2, 1)))

fsl_wash_health_nutrition_drivers_df
fsl_drivers<-list()
for(i in 1:length(unique(fsl_wash_health_nutrition_drivers_df$county))){
  print(i)
  county_temp<-unique(fsl_wash_health_nutrition_drivers_df$county)[1]
  sector_score_unique<-distinct(fsl_wash_health_nutrition_drivers_df %>% filter(county==county_temp) %>% select(sector, sector_score))
  df_temp<-fsl_wash_health_nutrition_drivers_df %>%
    filter(county==county_temp) %>%
    group_by(sector) %>%
    select(sector,county, responsible_indicators, sector_score) %>%
    tidyr::pivot_wider(names_from = responsible_indicators,values_from = sector_score) %>% data.frame() %>%
    right_join(sector_score_unique, by= "sector")

  # debugonce(hdm_fsl)
  fsl_drivers[[county_temp]]<-hdm_fsl2(df_temp) %>% mutate(county=county_temp)
}
