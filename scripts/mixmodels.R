# Library
library(lme4)
library(MASS)
library(MuMIn)
library(sjPlot)
library(readr)
library(dplyr)
library(modelr)
library(Hmisc)


source("config.R")


# Create data set
covidp_long <- read.csv("outputs/covidp_long.csv")
covidy_long <- read.csv("outputs/covidy_long.csv")
family_ <- read.csv("outputs/family.csv") %>% dplyr::select(-eventname)
demo_baseline <- read.csv("outputs/demographics_baseline.csv")
p_factor_with_sui <- read_csv(paste0(p_factor_files_path, "ABCD_psychopathology_bifactor_scores_23March2021_WITH_SUICIDALITY.csv"))
site <- read.csv("../outputs/site.csv") %>% 
  dplyr::filter(eventname == "1_year_follow_up_y_arm_1") %>% 
  dplyr::select(src_subject_id, site_id_l_br)

demo_baseline[,c("interview_date" ,"interview_age" ,"eventname" )] <- NULL

p_factor_with_sui$src_subject_id <- paste0("NDAR_", p_factor_with_sui$ID)
p_factor_with_sui$ID <- NULL
colnames(p_factor_with_sui)[1:7] <- paste0(colnames(p_factor_with_sui)[1:7], "_with_sui")


dataset <- merge(covidp_long,covidy_long, all = T )
dataset <- merge(dataset,family_, all.x = T )
dataset <- merge(dataset,demo_baseline, all.x = T )


dataset$rel_family_id <- dataset$rel_family_id + 1
dataset$timepoint <- sub("cv", "" ,dataset$timepoint )

dataset <- merge(dataset, p_factor_with_sui, all.x = T)
dataset <- merge(dataset, site, all.x = T)

# Add columns
dataset$morning_bedtime_routine <- rowMeans(dataset[, grep("routine", colnames(dataset))], na.rm = T)
dataset <- add_residuals(dataset, lm(money_cv ~ worry_y_cv, data = dataset), var = "Residualized_money_cv")
dataset$su_total_cv_binary <- ifelse(dataset$su_total_cv == 0, 0, 1)


dataset <- dataset %>%
  dplyr::mutate(
    fam_under_poverty_line = case_when(
      # group 5: half above poverty line, half below --> NA
      household_income <= 4 ~ 1,
      household_income >= 6 ~ 0,
      TRUE ~ NA_real_
    )
  )

dataset <- dataset %>%
  dplyr::mutate(across(
    c(
      src_subject_id,
      rel_family_id,
      sex_br,
      race_white,
      race_black,
      race_asian,
      ethnicity_hisp,
      su_total_cv_binary,
      child_tested_pos_cv,
      fam_tested_pos_cv,
      child_separate_cv,
      school_close_spring_2020_cv,
      fam_wage_loss_cv,
      went_to_school_cv,
      enjoy_school_y_cv,
      child_tested_cv,
      fam_under_poverty_line,
      site_id_l_br
    ),
    as.factor
  )) %>%
  dplyr::mutate(across(
    c(
      timepoint,
      felt_sad_cv_raw_tot_bar_Z,
      household_income,
      fam_isolate_tot_cv,
      parent_monitor_tot_bar,
      increased_conflict_cv,
      morning_bedtime_routine,
      screentime_wknd_hr_cv,
      demo_exercise_cv
    ),
    as.numeric
  ))

################### NO INTERACTIONS ################### 
# OUTCOME 1: WAGE LOSS
## Model 1 (+ pre-covid SES)
model_1_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + 
                        age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu, 
                      ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                      data = dataset, verbose = FALSE)

## Model 2 (+ P-factor) [MAIN MODEL]
model_2_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + 
                        age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                      ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                      data = dataset, verbose = FALSE)

## Model 3 (+ exposome)
model_3_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + 
                        age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui +
                        morning_bedtime_routine + screentime_wknd_hr_cv + demo_exercise_cv + parent_monitor_tot_bar + increased_conflict_cv + child_tested_pos_cv + 
                        fam_isolate_tot_cv + child_separate_cv + school_close_spring_2020_cv, #went_to_school_cv #enjoy_school_y_cv not work
                      ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                      data = dataset, verbose = FALSE)

comb_1 <- tab_model(model_1_DV, model_2_DV, model_3_DV)

r.squaredGLMM(model_1_DV)
# R2m       R2c
# delta     0.05130253 0.6312130
# lognormal 0.05194201 0.6390809
# trigamma  0.05063511 0.6230011
r.squaredGLMM(model_2_DV)
# R2m       R2c
# delta     0.1211565 0.6281227
# lognormal 0.1226817 0.6360303
# trigamma  0.1195648 0.6198707
r.squaredGLMM(model_3_DV)
# R2m       R2c
# delta     0.1694682 0.7092143
# lognormal 0.1706556 0.7141835
# trigamma  0.1682393 0.7040714


# OUTCOME 2: FINANCIAL WORRIES
model_1_fw_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

model_2_fw_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

model_3_fw_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui +
                           morning_bedtime_routine + screentime_wknd_hr_cv + demo_exercise_cv + parent_monitor_tot_bar + increased_conflict_cv +
                           child_tested_pos_cv + fam_isolate_tot_cv + child_separate_cv + school_close_spring_2020_cv, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

model_4_fw_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + fam_wage_loss_cv +
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui +
                           morning_bedtime_routine + screentime_wknd_hr_cv + demo_exercise_cv + parent_monitor_tot_bar + increased_conflict_cv +
                           child_tested_pos_cv + fam_isolate_tot_cv + child_separate_cv + school_close_spring_2020_cv, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

comb_2 <- tab_model(model_1_fw_DV, model_2_fw_DV, model_3_fw_DV, model_4_fw_DV)
r.squaredGLMM(model_1_fw_DV)
# R2m       R2c
# delta     0.06933016 0.6229192
# lognormal 0.07021349 0.6308557
# trigamma  0.06840877 0.6146407
r.squaredGLMM(model_2_fw_DV)
# R2m       R2c
# delta     0.1362938 0.6235249
# lognormal 0.1380260 0.6314498
# trigamma  0.1344869 0.6152585
r.squaredGLMM(model_3_fw_DV)
# R2m       R2c
# delta     0.1934641 0.7077611
# lognormal 0.1948237 0.7127351
# trigamma  0.1920571 0.7026139
r.squaredGLMM(model_4_fw_DV)
# R2m       R2c
# delta     0.1925013 0.7075636
# lognormal 0.1938551 0.7125396
# trigamma  0.1911004 0.7024144



################### WITH INTERACTIONS ################### 

# OUTCOME 1: WAGE LOSS
## household_income*fam_wage_loss_cv
model_1_DV_int <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + household_income*fam_wage_loss_cv +
                            age + sex_br + race_white + race_black + ethnicity_hisp + parents_avg_edu + General_p_with_sui, 
                          ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                          data = dataset, verbose = FALSE)

comb_1_int <- tab_model(model_2_DV, model_1_DV_int)

r.squaredGLMM(model_1_DV_int)
# R2m       R2c
# delta     0.1213015 0.6280096
# lognormal 0.1228292 0.6359191
# trigamma  0.1197072 0.6197556


# OUTCOME 2: FINANCIAL WORRIES
## household_income * Residualized_money_cv
model_1_fw_DV_int <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + household_income*Residualized_money_cv +
                               age + sex_br + race_white + race_black + ethnicity_hisp + parents_avg_edu + General_p_with_sui, 
                             ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                             data = dataset, verbose = FALSE)

comb_2_int <- tab_model(model_2_fw_DV, model_1_fw_DV_int)
r.squaredGLMM(model_1_fw_DV_int)
# R2m       R2c
# delta     0.1366673 0.6239468
# lognormal 0.1384014 0.6318635
# trigamma  0.1348585 0.6156889

################### SENSITIVITIVE ANALYSIS ################### 

# OUTCOME 3: mental_health_cv
model_1_mh_DV <- glmmPQL(mental_health_cv ~ fam_wage_loss_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

model_2_mh_DV <- glmmPQL(mental_health_cv ~ Residualized_money_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

comb_3 <- tab_model(model_1_mh_DV, model_2_mh_DV)

r.squaredGLMM(model_1_mh_DV)
# R2m       R2c
# delta     0.03745182 0.3035925
# lognormal 0.03808355 0.3087134
# trigamma  0.03681091 0.2983971
r.squaredGLMM(model_2_mh_DV)
# R2m       R2c
# delta     0.03837507 0.2989958
# lognormal 0.03903511 0.3041384
# trigamma  0.03770548 0.2937787


# OUTCOME 4: su_total_cv
model_1_su_DV <- glmmPQL(su_total_cv ~ fam_wage_loss_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

model_2_su_DV <- glmmPQL(su_total_cv ~ Residualized_money_cv*timepoint + 
                           age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                         ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                         data = dataset, verbose = FALSE)

comb_4 <- tab_model(model_1_su_DV, model_2_su_DV)

r.squaredGLMM(model_1_su_DV)
# R2m       R2c
# delta     0.01928362 0.9958127
# lognormal 0.01928556 0.9959128
# trigamma  0.01928159 0.9957077
r.squaredGLMM(model_2_su_DV)
# R2m       R2c
# delta     0.01981825 0.9959171
# lognormal 0.01982023 0.9960163
# trigamma  0.01981618 0.9958128


# OUTCOME 5: pstr_cv_raw_tot_bar
model_1_pstr_DV <- glmmPQL(pstr_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + 
                             age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                           ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                           data = dataset, verbose = FALSE)

model_2_pstr_DV <- glmmPQL(pstr_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + 
                             age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                           ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                           data = dataset, verbose = FALSE)

comb_5 <- tab_model(model_1_pstr_DV, model_2_pstr_DV)

r.squaredGLMM(model_1_pstr_DV)
# R2m       R2c
# delta     0.08986857 0.5535629
# lognormal 0.09124681 0.5620524
# trigamma  0.08843705 0.5447451
r.squaredGLMM(model_2_pstr_DV)
# R2m       R2c
# delta     0.1025289 0.5447774
# lognormal 0.1041365 0.5533192
# trigamma  0.1008600 0.5359101



# EXPOSURES
model_4_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_exp_tot_cv*timepoint + 
                        age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                      ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                      data = dataset, verbose = FALSE)

model_5_DV <- glmmPQL(felt_sad_cv_raw_tot_bar ~ child_worried_about_cv*timepoint + 
                        age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui, 
                      ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                      data = dataset, verbose = FALSE)

comb_6 <- tab_model(model_4_DV, model_5_DV)

r.squaredGLMM(model_4_DV)
# R2m       R2c
# delta     0.1223449 0.6258192
# lognormal 0.1239019 0.6337837
# trigamma  0.1207200 0.6175075
r.squaredGLMM(model_5_DV)
# R2m       R2c
# delta     0.1275978 0.6184534
# lognormal 0.1292408 0.6264168
# trigamma  0.1258846 0.6101498




# ADD SITE AS A COVARIATE
################### NO INTERACTIONS - ADD SITE ################### 
# OUTCOME 1: WAGE LOSS + SITE

## Model 2 (+ P-factor) [MAIN MODEL] + SITE
model_2_DV_site <- glmmPQL(felt_sad_cv_raw_tot_bar ~ fam_wage_loss_cv*timepoint + 
                             age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui + site_id_l_br, 
                           ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                           data = dataset, verbose = FALSE)

comb_1_site <- tab_model(model_2_DV, model_2_DV_site)

r.squaredGLMM(model_2_DV_site)
# R2m       R2c
# delta     0.1324887 0.6280796
# lognormal 0.1341564 0.6359859
# trigamma  0.1307483 0.6198291


# OUTCOME: FINANCIAL WORRIES
model_2_fw_DV_site <- glmmPQL(felt_sad_cv_raw_tot_bar ~ Residualized_money_cv*timepoint + 
                                age + sex_br + race_white + race_black + ethnicity_hisp + household_income + parents_avg_edu + General_p_with_sui + site_id_l_br, 
                              ~ 1 | rel_family_id/src_subject_id, family = quasipoisson(),
                              data = dataset, verbose = FALSE)

comb_2_site <- tab_model(model_2_fw_DV, model_2_fw_DV_site)

r.squaredGLMM(model_2_fw_DV_site)
# R2m       R2c
# delta     0.1474327 0.6235586
# lognormal 0.1493061 0.6314820
# trigamma  0.1454785 0.6152937