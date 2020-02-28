# library(here)

# int_db <- read.csv(here::here("..", "02_Data_Aggregated", "INT_DB_Values.csv"))
int_db<- read.csv("inputs/INT_DB_Values.csv")

livelihood_zone_agriculture_excluded <- c("Pibor", "KapoetaEast")
livelihood_zone_livestock_excluded <- c("Ezo", "Ibba", "Maridi", "MundriEast", "MundriWest", "Mvolo", "Nagero",
                                        "Nzara", "Tambura", "Yambio")
livelihood_zone_markets_excluded <- c()

# Creating the scoring for each indicator
indicator_label <- function(db, column) {
  score_column <- paste0(column, ".score")
  label_column <- paste0(column, ".label")
  db[[label_column]] <-
      ifelse(is.na(db[[score_column]]), "Insufficient Data",
      ifelse(db[[score_column]] >= 5, "Confirmed",   ##remove --------------------------
      ifelse(db[[score_column]] >= 4, "Very High",
      ifelse(db[[score_column]] >= 3, "High",
      ifelse(db[[score_column]] >= 2, "Moderate", "Low")))))
  return (db)
}

# ---------------------------------- //A. FSL ------------------------------------------
# A.1. FSL: Food Availability and Access

#  ifelse(int_db$A.1.hunger_worst + int_db$A.1.hunger_severe >= 0.60, 4,
int_db$A.1.hunger_severity.score <-
  ifelse(int_db$A.1.hunger_worst >= 0.20, 4,
  ifelse(int_db$A.1.hunger_worst + int_db$A.1.hunger_severe >= 0.60, 4,
  ifelse(int_db$A.1.hunger_worst + int_db$A.1.hunger_severe >= 0.40, 3,
  ifelse(int_db$A.1.hunger_worst + int_db$A.1.hunger_severe >= 0.20, 2, 1))))

## ** NEED to review the threshold -- maybe is calculated incorrectly in the AoK script as very small values
# Wild food making people sick and more than half the proportion
int_db$A.1.food_source_wild_emergency.score <-
  ifelse((int_db$A.1.food_wild_emergency + int_db$A.1.food_wild_proportion) / 2 >= 0.30, 4,
  ifelse((int_db$A.1.food_wild_emergency + int_db$A.1.food_wild_proportion) / 2 >= 0.20, 3,
  ifelse((int_db$A.1.food_wild_emergency + int_db$A.1.food_wild_proportion) / 2 >= 0.10, 2, 1)))
int_db$A.1.food_source_wild_emergency <- (int_db$A.1.food_wild_emergency + int_db$A.1.food_wild_proportion) / 2

#A.1.Coping Strategy - Adults Skipping Meals for Children to Eat
int_db$A.1.food_coping_children_only.score <-
  ifelse(int_db$A.1.food_coping_children_only >= 0.40, 4,
  ifelse(int_db$A.1.food_coping_children_only >= 0.20, 3,
  ifelse(int_db$A.1.food_coping_children_only >= 0.10, 2, 1)))

#A.1.Coping Strategy - All Day Without Eating
int_db$A.1.food_coping_skip_days.score <-
  ifelse(int_db$A.1.food_coping_skip_days >= 0.40, 4,
  ifelse(int_db$A.1.food_coping_skip_days >= 0.20, 3,
  ifelse(int_db$A.1.food_coping_skip_days >= 0.10, 2, 1)))

# A.1.Unsustainable Food Source
int_db$A.1.food_source_unsustainable.score <-
  ifelse(int_db$A.1.food_source_unsustainable >= 0.45, 4,
  ifelse(int_db$A.1.food_source_unsustainable >= 0.30, 3,
  ifelse(int_db$A.1.food_source_unsustainable >= 0.15, 2, 1)))

## // // A.2. FSL ~ Agriculture ----

# A.2. Access to Land & Inputs
int_db$A.2.land_inputs_no_access.score <-
  ifelse((int_db$A.2.agri_land_no_access + int_db$A.2.agri_seeds + int_db$A.2.agri_tools) / 3 >= 0.35, 4,
  ifelse((int_db$A.2.agri_land_no_access + int_db$A.2.agri_seeds + int_db$A.2.agri_tools) / 3 >= 0.25, 3,
  ifelse((int_db$A.2.agri_land_no_access + int_db$A.2.agri_seeds + int_db$A.2.agri_tools) / 3 >= 0.15, 2, 1)))
#to create label for Tableau
int_db$A.2.agri_land_inputs_no_access <- (int_db$A.2.agri_land_no_access + int_db$A.2.agri_seeds + int_db$A.2.agri_tools) / 3
int_db$A.2.agri_no_inputs <- (int_db$A.2.agri_seeds + int_db$A.2.agri_tools) / 2

# A.2.CFSAM Annual Crop production
int_db$A.2.crop_production.score <-
  ifelse(int_db$A.2.crop_production <= -0.30, 4,
  ifelse(int_db$A.2.crop_production <= -0.20, 3,
  ifelse(int_db$A.2.crop_production <= -0.10, 2, 1)))

# A.2.Fall Army Worm
int_db$A.2.fall_army_worm.score <-
  ifelse(int_db$A.2.fall_army_worm >= 0.30, 4,
  ifelse(int_db$A.2.fall_army_worm >= 0.20, 3,
  ifelse(int_db$A.2.fall_army_worm >= 0.10, 2, 1)))

## // // A.3. FSL ~ Livestock -----
int_db$A.3.livestock_no_access_possession.score <-
  ifelse(int_db$A.3.livestock_no_access_possession >= 0.60, 4,
  ifelse(int_db$A.3.livestock_no_access_possession >= 0.40, 3,
  ifelse(int_db$A.3.livestock_no_access_possession >= 0.20, 2, 1)))

# Coping Strategy - Selling Livestock
# Tyler adjusted thresholds
int_db$A.3.livestock_sell.score <-
  ifelse(int_db$A.3.livestock_sell >= 0.70, 4,
  ifelse(int_db$A.3.livestock_sell >= 0.50, 3,
  ifelse(int_db$A.3.livestock_sell >= 0.30, 2, 1)))

# Livestock Disease
int_db$A.3.livestock_disease_aok.score <-
  ifelse(int_db$A.3.livestock_disease_aok >= 0.60, 4,
  ifelse(int_db$A.3.livestock_disease_aok >= 0.40, 3,
  ifelse(int_db$A.3.livestock_disease_aok >= 0.20, 2, 1)))

#**Review threshold levels - this indicator is asking if "yes" do they access to milk and dairy?
## not sure I"ve done the thresholding correctly, does this now give a score of 0 a 5? and if so how to give that an n.a. value
int_db$A.3.food_now_milk_dairy.score <-
  ifelse(int_db$A.3.food_now_milk_dairy >= 0.80, 4,
  ifelse(int_db$A.3.food_now_milk_dairy >= 0.60, 3,
  ifelse(int_db$A.3.food_now_milk_dairy >= 0.40, 2, 1)))

## // // A.4. FSL ~ Markets -----
#need to combine this with the time
int_db$A.4.market_no_access.score <-
  ifelse(int_db$A.4.market_no_access >= 0.60, 4,
  ifelse(int_db$A.4.market_no_access >= 0.40, 3,
  ifelse(int_db$A.4.market_no_access >= 0.20, 2, 1)))

# Max - Are we supposed to be changing these two (Sorghum and Bean) to a three month average?
int_db$A.4.prices_sorghum.score <-
  ifelse(int_db$A.4.prices_sorghum >= 0.15, 4,
  ifelse(int_db$A.4.prices_sorghum >= 0.10, 3,
  ifelse(int_db$A.4.prices_sorghum >= 0.05, 2, 1)))

# Max
int_db$A.4.prices_bean.score <-
  ifelse(int_db$A.4.prices_bean >= 0.15, 4,
  ifelse(int_db$A.4.prices_bean >= 0.10, 3,
  ifelse(int_db$A.4.prices_bean >= 0.05, 2, 1)))

## // // A.5. FSL ~ Climate -----
int_db$A.5.ndvi.score <-
  ifelse(int_db$A.5.ndvi_anom <= 90, 4,
  ifelse(int_db$A.5.ndvi_anom <= 95, 3,
  ifelse(int_db$A.5.ndvi_anom <= 100, 2, 1)))

#do absolute values
int_db$AB.rainfall.score <-
  ifelse(abs(int_db$AB.rainfall_1month_anom - 100) >= 30, 4,
  ifelse(abs(int_db$AB.rainfall_1month_anom - 100) >= 20, 3,
  ifelse(abs(int_db$AB.rainfall_1month_anom - 100) >= 10, 2, 1)))


### -----------------------------------------------   // B. WASH    -----------------------------------------------

# NEW AOK MONTHLY SYSTEM NO ANCHOR - TT
int_db$B.clean_water_no_access_aok.score <-
  ifelse(int_db$B.clean_water_no_access_aok >= 0.70, 4,
  ifelse(int_db$B.clean_water_no_access_aok >= 0.40, 3,
  ifelse(int_db$B.clean_water_no_access_aok >= 0.20, 2, 1)))

int_db$B.open_defecation_aok.score <-
  ifelse(int_db$B.open_defecation_aok >= 0.90, 4,
  ifelse(int_db$B.open_defecation_aok >= 0.40, 3,
  ifelse(int_db$B.open_defecation_aok >= 0.20, 2, 1)))

# END OF NEW MONTHLY SYSTEM NO ANCHOR - TT

int_db$BC.awd.score <-
  ifelse(int_db$BC.awd >= int_db$BC.awd_nonile_7 & int_db$BC.awd > 0, 4,
  ifelse(int_db$BC.awd >= int_db$BC.awd_nonile_6 & int_db$BC.awd > 0, 3,
  ifelse(int_db$BC.awd >= int_db$BC.awd_nonile_5 & int_db$BC.awd > 0, 2, 1)))

int_db$BC.cholera.score <-
  ifelse(int_db$BC.cholera >= int_db$BC.cholera_nonile_7 & int_db$BC.cholera > 0, 4,
  ifelse(int_db$BC.cholera >= int_db$BC.cholera_nonile_6 & int_db$BC.cholera > 0, 3,
  ifelse(int_db$BC.cholera >= int_db$BC.cholera_nonile_5 & int_db$BC.cholera > 0, 2, 1)))

int_db$BC.malaria.score <-
  ifelse(int_db$BC.malaria >= int_db$BC.malaria_nonile_7 & int_db$BC.malaria > 0, 4,
  ifelse(int_db$BC.malaria >= int_db$BC.malaria_nonile_6 & int_db$BC.malaria > 0, 3,
  ifelse(int_db$BC.malaria >= int_db$BC.malaria_nonile_5 & int_db$BC.malaria > 0, 2, 1)))

### -----------------------------------------------   // C. Health    -----------------------------------------------
int_db$C.ebola.score <-
  ifelse(int_db$C.ebola >= 1, 4, 1)

int_db$C.measles.score <-
  ifelse(int_db$C.measles >= int_db$C.measles_nonile_7 & int_db$C.measles > 0, 4,
  ifelse(int_db$C.measles >= int_db$C.measles_nonile_6 & int_db$C.measles > 0, 3,
  ifelse(int_db$C.measles >= int_db$C.measles_nonile_5 & int_db$C.measles > 0, 2, 1)))

int_db$C.ari.score <-
  ifelse(int_db$C.ari >= int_db$C.ari_nonile_7 & int_db$C.ari > 0, 4,
  ifelse(int_db$C.ari >= int_db$C.ari_nonile_6 & int_db$C.ari > 0, 3,
  ifelse(int_db$C.ari >= int_db$C.ari_nonile_5 & int_db$C.ari > 0, 2, 1)))

### -----------------------------------------------   // D. Nutrition     -----------------------------------------------
int_db$D.whz_gam.score <-
  ifelse(int_db$D.whz_gam >= 0.15, 4,
  ifelse(int_db$D.whz_gam >= 0.10, 3,
  ifelse(int_db$D.whz_gam >= 0.05, 2, 1)))





# ==== Initial aggregating and scoring =====
## creating dataframe to hold relevant indicators within, then applying the selected scoring to that dataframe

# ---- // FSL -----
A.1.food_access <- data.frame(
  int_db$A.1.hunger_severity.score,
  int_db$A.1.food_source_wild_emergency.score,
  int_db$A.1.food_coping_children_only.score,
  int_db$A.1.food_coping_skip_days.score,
  int_db$A.1.food_source_unsustainable.score)

int_db$A.1.food_access.score <-
  ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 3, 4,
  ifelse(rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 2 |
         rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 3, 3,
  ifelse((rowSums(A.1.food_access >= 4, na.rm = TRUE) >= 1 &
         rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 1) |
         rowSums(A.1.food_access >= 3, na.rm = TRUE) >= 2 |
         rowSums(A.1.food_access >= 2, na.rm = TRUE) >= 3, 2,
  ifelse(rowSums(A.1.food_access >= 1, na.rm = TRUE) >= 3, 1, NA))))

A.2.agriculture <- data.frame(
  int_db$A.2.land_inputs_no_access.score,
  int_db$A.2.fall_army_worm.score,
  int_db$A.2.crop_production.score)

int_db$A.2.agriculture.score <-
  ifelse((int_db$county %in% livelihood_zone_agriculture_excluded), NA,
  ifelse(rowSums(A.2.agriculture >= 4, na.rm = TRUE) >= 2, 4,
  ifelse(rowSums(A.2.agriculture >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(A.2.agriculture >= 4, na.rm = TRUE) >= 1 |
         rowSums(A.2.agriculture >= 2, na.rm = TRUE) >= 2, 2,
  ifelse(rowSums(A.2.agriculture >= 1, na.rm = TRUE) >= 2, 1, NA)))))

A.3.livestock <- data.frame(
  int_db$A.3.livestock_no_access_possession.score,
  int_db$A.3.livestock_disease_aok.score,
  int_db$A.3.livestock_sell.score,
  int_db$A.3.food_now_milk_dairy.score)

int_db$A.3.livestock.score <-
  ifelse((int_db$county %in% livelihood_zone_livestock_excluded), NA,
  ifelse(rowSums(A.3.livestock >= 4, na.rm = TRUE) >= 2, 4,
  ifelse(rowSums(A.3.livestock >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(A.3.livestock >= 4, na.rm = TRUE) >= 1 |
         rowSums(A.3.livestock >= 2, na.rm = TRUE) >= 2, 2,
  ifelse(rowSums(A.3.livestock >= 1, na.rm = TRUE) >= 2, 1, NA)))))

A.4.markets <- data.frame(
  int_db$A.4.market_no_access.score,
  int_db$A.4.prices_sorghum.score,
  int_db$A.4.prices_bean.score)

A.4.markets_check <- data.frame(
  int_db$A.4.prices_sorghum.score,
  int_db$A.4.prices_bean.score)

int_db$A.4.markets.score <-
  ifelse((int_db$county %in% livelihood_zone_markets_excluded), NA,
  ifelse(rowSums(A.4.markets_check >= 1, na.rm = TRUE) == 0, NA,
  ifelse(rowSums(A.4.markets >= 4, na.rm = TRUE) >= 2, 4,
  ifelse(rowSums(A.4.markets >= 4, na.rm = TRUE) >= 1 |
         rowSums(A.4.markets >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(A.4.markets >= 2, na.rm = TRUE) >= 2, 2,
  ifelse(rowSums(A.4.markets >= 1, na.rm = TRUE) >= 2, 1, NA))))))

A.5.climate <- data.frame(
  int_db$A.5.ndvi.score,
  int_db$AB.rainfall.score)

int_db$A.5.climate.score <- rowMeans(A.5.climate, na.rm=TRUE)

int_db$A.5.climate.score <-
  ifelse(int_db$A.5.climate.score >= 3.5, 4,
  ifelse(int_db$A.5.climate.score >= 2.5, 3,
  ifelse(int_db$A.5.climate.score >= 1.5, 2,
  ifelse(int_db$A.5.climate.score >= 1, 1, NA))))

A.fsl <- data.frame(
  int_db$A.1.food_access.score,
  int_db$A.2.agriculture.score,
  int_db$A.3.livestock.score,
  int_db$A.4.markets.score,
  int_db$A.5.climate.score)


int_db$A.fsl.score <-
  ifelse(rowSums(A.fsl >= 4, na.rm = TRUE) >= 2, 4,
  ifelse(rowSums(A.fsl >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(A.fsl >= 2, na.rm = TRUE) >= 2, 2,
  ifelse(rowSums(A.fsl >= 1, na.rm = TRUE) >= 2, 1, NA))))

# THIS OVERWRITES FSL WITH IPC DATA
## First this writes if IPC is na (which it is for 10 months a year) then read FSL based from AoK
## Then if FSL_AoK is na, then read IPC_projection which is done to fill gaps where AoK doesn"t have coverage

int_db$A.fsl.score <- ifelse(
  is.na(int_db$A.ipc_food_insecurity),
  int_db$A.fsl.score,
  int_db$A.ipc_food_insecurity)

int_db$A.fsl.score <- ifelse(
  is.na(int_db$A.fsl.score),
  int_db$A.ipc_food_insecurity_projection,
  int_db$A.fsl.score)


# ---- // WASH -----
B.wash.indicators <- data.frame(
  int_db$B.clean_water_no_access_aok.score,
  int_db$B.open_defecation_aok.score,
  int_db$AB.rainfall.score,
  int_db$BC.awd.score,
  int_db$BC.cholera.score,
  int_db$BC.malaria.score)

B.wash_special.indicators <- data.frame(
  int_db$BC.cholera.score)

int_db$B.wash.score <-
  ifelse(rowSums(B.wash_special.indicators >= 4, na.rm = TRUE) >= 1 |
         rowSums(B.wash.indicators >= 4, na.rm = TRUE) >= 4 |
        (rowSums(B.wash.indicators >= 4, na.rm = TRUE) >= 2 &
         rowSums(B.wash.indicators >= 3, na.rm = TRUE) >= 2), 4,
  ifelse(rowSums(B.wash.indicators >= 4, na.rm = TRUE) >= 2 |
         rowSums(B.wash.indicators >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(B.wash.indicators >= 3, na.rm = TRUE) >= 1 |
         rowSums(B.wash.indicators >= 2, na.rm = TRUE) >= 3, 2,
  ifelse(rowSums(B.wash.indicators >= 1, na.rm = TRUE) >= 3, 1, NA))))

#---- // Health ----
C.health.indicators <- data.frame(
  int_db$C.ari.score,
  int_db$C.measles.score,
  int_db$C.ebola.score,
  int_db$BC.awd.score,
  int_db$BC.cholera.score,
  int_db$BC.malaria.score)

C.health_special.indicators <- data.frame(
  int_db$BC.cholera.score,
  int_db$C.ebola.score)

int_db$C.health.score <-
  ifelse(rowSums(C.health_special.indicators >= 4, na.rm = TRUE) >= 1 |
         rowSums(C.health.indicators >= 4, na.rm = TRUE) >= 2, 4,
  ifelse(rowSums(C.health.indicators >= 4, na.rm = TRUE) >= 1 |
         rowSums(C.health.indicators >= 3, na.rm = TRUE) >= 2, 3,
  ifelse(rowSums(C.health.indicators >= 2, na.rm = TRUE) >= 2, 2,
  ifelse(rowSums(C.health.indicators >= 1, na.rm = TRUE) >= 2, 1, NA))))



# ---- // Nutrition ----
int_db$D.nutrition.score <- ifelse(is.na(int_db$D.ipc_malnutrition), int_db$D.ipc_malnutrition_projection, int_db$D.ipc_malnutrition)

# ======== Final scoring system =========
nawg_triggers <- data.frame(
  int_db$N.disease_outbreak,
  int_db$N.displacement,
  int_db$N.gam,
  int_db$N.ipc_population,
  int_db$N.mortality_increase)

county_indicators <- data.frame(
  int_db$A.fsl.score,
  int_db$B.wash.score,
  int_db$C.health.score,
  int_db$D.nutrition.score)

int_db$county.score <- rowMeans(county_indicators, na.rm=TRUE)





int_db$county.score <-
  ifelse(int_db$county.score >= 3.5, 4,
  ifelse(int_db$county.score >= 2.5, 3,
  ifelse(int_db$county.score >= 1.5, 2, 1)))

#int_db$county.score<-
  ifelse(rowSums(nawg_triggers == "Yes", na.rm = TRUE) >= 1, 5, int_db$county.score)

# Now label all of the scores

int_db <- indicator_label(int_db, "A.1.hunger_severity")
int_db <- indicator_label(int_db, "A.1.food_source_wild_emergency")
int_db <- indicator_label(int_db, "A.1.food_coping_children_only")
int_db <- indicator_label(int_db, "A.1.food_coping_skip_days")
int_db <- indicator_label(int_db, "A.1.food_source_unsustainable")
int_db <- indicator_label(int_db, "A.2.land_inputs_no_access")
int_db <- indicator_label(int_db, "A.2.crop_production")
int_db <- indicator_label(int_db, "A.2.fall_army_worm")
int_db <- indicator_label(int_db, "A.3.livestock_no_access_possession")
int_db <- indicator_label(int_db, "A.3.livestock_sell")
int_db <- indicator_label(int_db, "A.3.livestock_disease_aok")
int_db <- indicator_label(int_db, "A.3.food_now_milk_dairy")
int_db <- indicator_label(int_db, "A.4.market_no_access")
int_db <- indicator_label(int_db, "A.4.prices_sorghum")
int_db <- indicator_label(int_db, "A.4.prices_bean")
int_db <- indicator_label(int_db, "A.5.ndvi")
int_db <- indicator_label(int_db, "AB.rainfall")
int_db <- indicator_label(int_db, "B.clean_water_no_access_aok")
int_db <- indicator_label(int_db, "B.open_defecation_aok")
int_db <- indicator_label(int_db, "BC.awd")
int_db <- indicator_label(int_db, "BC.cholera")
int_db <- indicator_label(int_db, "BC.malaria")
int_db <- indicator_label(int_db, "C.ari")
int_db <- indicator_label(int_db, "C.measles")
int_db <- indicator_label(int_db, "C.ebola")
int_db <- indicator_label(int_db, "D.whz_gam")

int_db <- indicator_label(int_db, "A.1.food_access")
int_db <- indicator_label(int_db, "A.2.agriculture")
int_db <- indicator_label(int_db, "A.3.livestock")
int_db <- indicator_label(int_db, "A.4.markets")
int_db <- indicator_label(int_db, "A.5.climate")
int_db <- indicator_label(int_db, "A.fsl")
int_db <- indicator_label(int_db, "B.wash")
int_db <- indicator_label(int_db, "C.health")
int_db <- indicator_label(int_db, "D.nutrition")
int_db <- indicator_label(int_db, "county")




## ---------------- Calculations for Tableau/Factsheet ---------------

#Setting sources
int_db$A.nutrition.score.source <- ifelse(
  is.na(int_db$D.ipc_malnutrition)
  ,"IPC Projection",
  "IPC")

int_db$A.fsl.score.source <- ifelse(
  is.na(int_db$A.ipc_food_insecurity),
  "FSL Indicators",
  "IPC")


#Easier numbers to understand
int_db$AB.rainfall_1month_anom.percent <- int_db$AB.rainfall_1month_anom - 100
int_db$AB.rainfall_1month_anom.percent2 <- ifelse(int_db$AB.rainfall_1month_anom.percent > 0, "+", "")

int_db$A.5.ndvi_anom.percent <- int_db$A.5.ndvi_anom - 100
int_db$A.5.ndvi_anom.percent2 <- ifelse(int_db$A.5.ndvi_anom.percent > 0, "+", "")

int_db$A.2.crop_production.percent <- int_db$A.2.crop_production
int_db$A.2.crop_production.percent2 <- ifelse(int_db$A.2.crop_production.percent > 0, "+", "")



#Counting
# library(dplyr)
# library(tidyr)
# int_db$C.health.count <- int_db %>% mutate(
#   # HDDS=rowSums(sapply(hh_data[,food_types], function(x) ifelse(x==7, TRUE,FALSE))),
#   I.FSL.HDDS_score_raw=rowSums(.[food_types],na.rm=TRUE),
#   HDDS_over5=rowSums(sapply(hh_data[,food_types], function(x) ifelse(x>2, TRUE,FALSE))))
#
# C.health.indicators$C.health.indicators.count <- count_(C.health.indicators, )
#
# C.health.indicators %>%
#   count("C.health.indicators.count") %>%
#   filter(n >= "2")


write.csv(
  int_db,
  file = here::here("..", "01_Data_Final", "INT_DB.csv"),
  na = "",
  row.names = FALSE)



# write.csv(
#   int_db_longterm,
#   file = here::here("..", "01_Data_Final", "INT_DB_longterm.csv"),
#   na = "",
#   row.names = FALSE)

