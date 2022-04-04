# Library
library(lme4)
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
demographics_long <- read.csv("../outputs/demographics_long.csv")
exposome_sum_set <- read.csv("../outputs/exposome_sum_set.csv")
site <- read.csv("outputs/site.csv") %>% 
  dplyr::filter(eventname == "1_year_follow_up_y_arm_1") %>% 
  dplyr::select(src_subject_id, site_id_l_br)

p_factor_with_sui$src_subject_id <- paste0("NDAR_", p_factor_with_sui$ID)
p_factor_with_sui$ID <- NULL
colnames(p_factor_with_sui)[1:7] <- paste0(colnames(p_factor_with_sui)[1:7], "_with_sui")

IVs <- merge(demographics_long, exposome_sum_set, all = T)
IVs <- IVs[(IVs$eventname == "1_year_follow_up_y_arm_1"),]

colnames(IVs)[2] <- "interview_date_before_covid"
colnames(IVs)[3] <- "interview_age_before_covid"
IVs$eventname <- NULL

dataset <- merge(covidp_long, covidy_long, all = T )
dataset <- merge(dataset, family_, all.x = T )
dataset <- merge(dataset, IVs)

demo_baseline <- demo_baseline %>%
  dplyr::select(
    -sex,
    -household_income,
    -parents_avg_edu,
    -separated_or_divorced,
    -parents_married,
    -age,
    -sex_br,
    -gender,
    -demo_fam_poverty,
    -interview_date,
    -interview_age,
    -eventname
  )

dataset <- merge(dataset, demo_baseline, all.x = T)
dataset <- merge(dataset, p_factor_with_sui, all.x = T)
dataset <- merge(dataset, site, all.x = T)

dataset$rel_family_id <- dataset$rel_family_id + 1
dataset$timepoint <- sub("cv", "" ,dataset$timepoint )

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
      felt_sad_cv_raw_tot_bar,
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

# writexl::write_xlsx(dataset, "outputs/mixedmodels_dat_final.xlsx")

################### NO INTERACTIONS ################### 
# OUTCOME 1: WAGE LOSS
## Model 1 (+ pre-covid SES)
model_1_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                        scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) +
                      (1 | rel_family_id/src_subject_id), 
                      data = dataset, verbose = FALSE)

## Model 2 (+ P-factor) [MAIN MODEL]
model_2_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                        scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                      (1 | rel_family_id/src_subject_id), 
                      data = dataset, verbose = FALSE)

## Model 3 (+ exposome)
model_3_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                        scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                        scale(morning_bedtime_routine) + scale(screentime_wknd_hr_cv) + scale(demo_exercise_cv) + scale(parent_monitor_tot_bar) + scale(increased_conflict_cv) + child_tested_pos_cv + 
                        scale(fam_isolate_tot_cv) + child_separate_cv + school_close_spring_2020_cv + #went_to_school_cv #enjoy_school_y_cv not work
                      (1 | rel_family_id/src_subject_id), 
                      data = dataset, verbose = FALSE)

comb_1 <- tab_model(model_1_DV, model_2_DV, model_3_DV)
comb_1

# OUTCOME 2: FINANCIAL WORRIES
model_1_fw_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

model_2_fw_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

model_3_fw_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                           scale(morning_bedtime_routine) + scale(screentime_wknd_hr_cv) + scale(demo_exercise_cv) + scale(parent_monitor_tot_bar) + scale(increased_conflict_cv) +
                           child_tested_pos_cv + scale(fam_isolate_tot_cv) + child_separate_cv + school_close_spring_2020_cv +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

model_4_fw_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + fam_wage_loss_cv +
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                           scale(morning_bedtime_routine) + scale(screentime_wknd_hr_cv) + scale(demo_exercise_cv) + scale(parent_monitor_tot_bar) + scale(increased_conflict_cv) +
                           child_tested_pos_cv + scale(fam_isolate_tot_cv) + child_separate_cv + school_close_spring_2020_cv +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

comb_2 <- tab_model(model_1_fw_DV, model_2_fw_DV, model_3_fw_DV, model_4_fw_DV)
comb_2

################### WITH INTERACTIONS ################### 

# OUTCOME 1: WAGE LOSS
## scale(household_income)*fam_wage_loss_cv
model_1_DV_int <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + scale(household_income)*fam_wage_loss_cv +
                            scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(parents_avg_edu) + General_p_with_sui +
                          (1 | rel_family_id/src_subject_id), 
                          data = dataset, verbose = FALSE)

comb_1_int <- tab_model(model_2_DV, model_1_DV_int)
comb_1_int

# OUTCOME 2: FINANCIAL WORRIES
## scale(household_income) * Residualized_money_cv
model_1_fw_DV_int <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + scale(household_income)*Residualized_money_cv +
                               scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(parents_avg_edu) + General_p_with_sui +
                             (1 | rel_family_id/src_subject_id), 
                             data = dataset, verbose = FALSE)

comb_2_int <- tab_model(model_2_fw_DV, model_1_fw_DV_int)
comb_2_int

################### SENSITIVITIVE ANALYSIS ################### 

# OUTCOME 3: mental_health_cv
model_1_mh_DV <- lmer(scale(mental_health_cv) ~ fam_wage_loss_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

model_2_mh_DV <- lmer(scale(mental_health_cv) ~ Residualized_money_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

comb_3 <- tab_model(model_1_mh_DV, model_2_mh_DV)
comb_3

# OUTCOME 4: su_total_cv
model_1_su_DV <- lmer(scale(su_total_cv) ~ fam_wage_loss_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

model_2_su_DV <- lmer(scale(su_total_cv) ~ Residualized_money_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                         (1 | rel_family_id/src_subject_id), 
                         data = dataset, verbose = FALSE)

comb_4 <- tab_model(model_1_su_DV, model_2_su_DV)
comb_4

# OUTCOME 5: pstr_cv_raw_tot_bar
model_1_pstr_DV <- lmer(scale(pstr_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                             scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                           (1 | rel_family_id/src_subject_id), 
                           data = dataset, verbose = FALSE)

model_2_pstr_DV <- lmer(scale(pstr_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                             scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                           (1 | rel_family_id/src_subject_id), 
                           data = dataset, verbose = FALSE)

comb_5 <- tab_model(model_1_pstr_DV, model_2_pstr_DV)
comb_5

# EXPOSURES
model_4_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_exp_tot_cv + timepoint + 
                        scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                      (1 | rel_family_id/src_subject_id), 
                      data = dataset, verbose = FALSE)

model_5_DV <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ child_worried_about_cv + timepoint + 
                        scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + General_p_with_sui +
                      (1 | rel_family_id/src_subject_id), 
                      data = dataset, verbose = FALSE)

comb_6 <- tab_model(model_4_DV, model_5_DV)
comb_6

# ADD SITE AS A COVARIATE & AS A RANDOM EFFECTS
# OUTCOME 1: WAGE LOSS + SITE

## Model 2 (+ P-factor) [MAIN MODEL] + SITE
model_2_DV_site <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                             scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + 
                          General_p_with_sui + site_id_l_br + (1 | rel_family_id/src_subject_id), 
                           data = dataset, verbose = FALSE)

model_2_DV_rsite <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ fam_wage_loss_cv + timepoint + 
                           scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + 
                           General_p_with_sui + (1 | rel_family_id/src_subject_id) + (1 | site_id_l_br), 
                         data = dataset, verbose = FALSE)

comb_1_site <- tab_model(model_2_DV, model_2_DV_rsite, model_2_DV_site)
comb_1_site

# OUTCOME: FINANCIAL WORRIES
model_2_fw_DV_site <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                                scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + 
                             General_p_with_sui + site_id_l_br + (1 | rel_family_id/src_subject_id), 
                              data = dataset, verbose = FALSE)

model_2_fw_DV_rsite <- lmer(scale(felt_sad_cv_raw_tot_bar) ~ Residualized_money_cv + timepoint + 
                              scale(interview_age) + sex_br + race_white + race_black + ethnicity_hisp + scale(household_income) + scale(parents_avg_edu) + 
                              General_p_with_sui + (1 | rel_family_id/src_subject_id) + (1 | site_id_l_br), 
                            data = dataset, verbose = FALSE)

comb_2_site <- tab_model(model_2_fw_DV, model_2_fw_DV_rsite, model_2_fw_DV_site)
comb_2_site
