library(psych)
library(plyr)
library(psych)

source("config.R")
source("utility_fun.R")


covidy_r1 = load_instrument("yabcdcovid19questionnaire01",covid19_1_3_files_path)
covidy_r2 = load_instrument("yabcdcovid19questionnaire01",covid19_4_6_files_path)

covidy = rbind.fill(covidy_r1, covidy_r2)
covidy[covidy == 777 | covidy == 999] = NA

#  new variable to use in reshape from long to wide format
covidy$timepoint = regmatches(covidy$eventname, regexpr("cv[1-6]", covidy$eventname))


#### remove timepoint 6 for now
covidy = covidy[covidy$timepoint != "cv6",]

####################################
###### outputs
####################################

###### 1. Sadness Scale
sadness_scale = covidy[,grep("src_|_age|timepoint|felt|sad(.*)tot", colnames(covidy))]
sadness_scale$felt_angry_cv = NULL
sadness_scale$felt_nervous_cv = NULL
sadness_scale$felt_scared_cv = NULL

# sum only if there are 8 answers 
sadness_scale$felt_sad_cv_raw_tot_bar = rowSums(sadness_scale[,grep("felt", colnames(sadness_scale))])
sadness_scale$nih_sad_cv_raw_tot = NULL

# no missing data
sadness_scale = sadness_scale[rowSums(is.na(sadness_scale)) == 0,]

sadness_scale_wide = reshape(sadness_scale, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 2 time points
# sadness_scale_wide = sadness_scale_wide[rowSums(!is.na(sadness_scale_wide)) > 20,]

# write.csv(file = "outputs/sadness_scale_wide.csv",x = sadness_scale_wide ,row.names=F, na = "")

###### 2. Substance
substance = covidy[,grep("src_|_age|timepoint|su_(a|l|m)", colnames(covidy))]
substance$su_mj_edible_cv = NULL
substance$su_mj_oils_cv = NULL

# no missing data
# substance = substance[rowSums(is.na(substance)) == 0,]

# convert to binary feature 
substance$su_alcohol_binary = ifelse(substance$su_alcohol_cv > 1, 1, 0)
substance$su_alcohol_binary[is.na(substance$su_alcohol_cv)] = NA

substance$su_liquids_binary = ifelse(substance$su_liquids_cv > 1, 1, 0)
substance$su_liquids_binary[is.na(substance$su_liquids_cv)] = NA

substance$su_medication_binary = ifelse(substance$su_medication_cv > 1, 1, 0)
substance$su_medication_binary[is.na(substance$su_medication_cv)] = NA

substance$su_meth_binary = ifelse(substance$su_meth_cv > 1, 1, 0)
substance$su_meth_binary[is.na(substance$su_meth_cv )] = NA

substance$su_other_cv = substance$su_liquids_binary + substance$su_medication_binary + substance$su_meth_binary

# check if the variables can be combined
xcor <- polychoric(substance[ ,grep("binary|mj", colnames(substance), value = T) ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

substance$su_total_cv = rowSums(substance[ ,grep("binary|mj", colnames(substance), value = T) ])

substance_wide = reshape(substance, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 3 time points
# substance_wide = substance_wide[(rowSums(!is.na(substance_wide)) > 33),]

# write.csv(file = "outputs/substance_wide.csv",x = substance_wide ,row.names=F, na = "")


###### 3. Perceived Stress Scale
perceived_stress = covidy[,grep("src_|_age|timepoint|pstr", colnames(covidy))]
perceived_stress$pstr_cv_raw_tot_bar = 10 - perceived_stress$pstr_confidence_p_cv - perceived_stress$pstr_way_p_cv + perceived_stress$pstr_overcome_p_cv + perceived_stress$pstr_unable_control_cv
perceived_stress$pstr_cv_raw_tot = NULL
perceived_stress = perceived_stress[rowSums(is.na(perceived_stress)) == 0,]

perceived_stress_wide = reshape(perceived_stress, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 3 time points
# perceived_stress_wide = perceived_stress_wide[(rowSums(!is.na(perceived_stress_wide)) > 18),]

# write.csv(file = "outputs/perceived_stress_wide.csv",x = perceived_stress_wide ,row.names=F, na = "")


###### 4. Mental health
mental_health = covidy[,grep("src_|_age|timepoint|mental", colnames(covidy))]
mental_health = mental_health[rowSums(is.na(mental_health))  == 0,]

mental_health_wide = reshape(mental_health, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

# include kids with at least 2 time points
# mental_health_wide = mental_health_wide[(rowSums(!is.na(mental_health_wide)) > 4),]

# write.csv(file = "outputs/mental_health_wide.csv",x = mental_health_wide ,row.names=F, na = "")



####################################
###### exposures
####################################

exposures = covidy[,grep("src_|_age|timepoint|exercise|money|outdoor|parent_monitor|pstr|routine|screentime_wknd|strle|exp_rac|witness_rac|^worr(ied|y_y)_cv", colnames(covidy))]
exposures_wide = reshape(exposures, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

###### bedtime
bedtime = exposures_wide[,grep("src_|bedtime", colnames(exposures_wide))]
# bedtime = bedtime[(rowSums(!is.na(bedtime)) > 3),]
bedtime$bedtime_routine_cv_mean = rowMeans(bedtime[,grep("bedtime", colnames(bedtime))], na.rm = T)

###### exercise
exercise = exposures_wide[,grep("src_|exercise", colnames(exposures_wide))]
# exercise = exercise[(rowSums(!is.na(exercise)) > 2),]
exercise$demo_exercise_cv_mean = rowMeans(exercise[,grep("exercise", colnames(exercise))], na.rm = T)

###### Sadness Scale - cont'
sadness_scale_wide = sadness_scale_wide[,grep("src_|bar", colnames(sadness_scale_wide))]
sadness_scale_wide$sad_cv_raw_tot_bar_mean = rowMeans(sadness_scale_wide[,grep("bar", colnames(sadness_scale_wide))], na.rm = T)

###### Mental health - cont'
mental_health_wide = mental_health_wide[,grep("src_|mental", colnames(mental_health_wide))]
mental_health_wide$mental_health_cv_mean = rowMeans(mental_health_wide[,grep("mental", colnames(mental_health_wide))], na.rm = T)

###### money
money = exposures_wide[,grep("src_|money", colnames(exposures_wide))]
# money = money[(rowSums(!is.na(money)) > 3),]
money$money_cv_mean = rowMeans(money[,grep("money", colnames(money))], na.rm = T)

###### outdoor_act
outdoor_act = exposures_wide[,grep("src_|outdoor_act", colnames(exposures_wide))]
# outdoor_act = outdoor_act[(rowSums(!is.na(outdoor_act)) > 3),]
outdoor_act$outdoor_act_cv_mean = rowMeans(outdoor_act[,grep("outdoor_act", colnames(outdoor_act))], na.rm = T)

###### parents monitor
parents_monitor = exposures[,grep("src_|timepoint|parent_monitor", colnames(exposures))]

# check if the variables can be combined
xcor <- polychoric(parents_monitor[ ,grep("parent_monitor", colnames(parents_monitor), value = T) ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

parents_monitor$parent_monitor_tot_bar = rowSums(parents_monitor[,grep("parent_monitor", colnames(parents_monitor))])
parents_monitor_wide = reshape(parents_monitor, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
parents_monitor_wide = parents_monitor_wide[,grep("src|bar", colnames(parents_monitor_wide))]
# parents_monitor_wide = parents_monitor_wide[(rowSums(!is.na(parents_monitor_wide)) > 3),]
parents_monitor_wide$parent_monitor_tot_bar_mean = rowMeans(parents_monitor_wide[,grep("bar", colnames(parents_monitor_wide))], na.rm = T)
  
###### Perceived Stress Scale - cont'
perceived_stress_wide = perceived_stress_wide[,grep("src_|bar", colnames(perceived_stress_wide))]
perceived_stress_wide$pstr_cv_raw_tot_bar_mean = rowMeans(perceived_stress_wide[,grep("bar", colnames(perceived_stress_wide))], na.rm = T)

###### routine
routine = exposures_wide[,grep("src_|^routine", colnames(exposures_wide))]
# routine = routine[(rowSums(!is.na(routine)) > 3),]
routine$routine_cv_mean = rowMeans(routine[,grep("routine", colnames(routine))], na.rm = T)

###### screen time
screentime = exposures_wide[,grep("src_|screentime", colnames(exposures_wide))]
screentime = screentime[,-grep("typical", colnames(screentime))]
# screentime = screentime[(rowSums(!is.na(screentime)) > 3),]
screentime$screentime_wknd_hr_cv_mean = rowMeans(screentime[,grep("screentime", colnames(screentime))], na.rm = T)

###### stressful life event 
stressful_events = exposures[,grep("src_|timepoint|strle", colnames(exposures))]
stressful_events = stressful_events[(rowSums(!is.na(stressful_events)) > 9),]

# check if the variables can be combined
xcor <- polychoric(stressful_events[ ,grep("strle", colnames(stressful_events), value = T) ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

stressful_events$strle_tot_bar = rowSums(stressful_events[,grep("strle", colnames(stressful_events))])
stressful_events$timepoint = NULL

###### Substance - cont'
substance_wide = substance_wide[,grep("src_|mj|alcohol_b|total|other", colnames(substance_wide))]
substance_wide$su_mj_use_past_month_cv_mean = rowMeans(substance_wide[,grep("mj", colnames(substance_wide))], na.rm = T)
substance_wide$su_alcohol_binary_mean = rowMeans(substance_wide[,grep("alcohol", colnames(substance_wide))], na.rm = T)
substance_wide$su_total_cv_mean = rowMeans(substance_wide[,grep("total", colnames(substance_wide))], na.rm = T)
substance_wide$su_other_cv_mean = rowMeans(substance_wide[,grep("other", colnames(substance_wide))], na.rm = T)

###### racism
racism = exposures_wide[,grep("src_|_rac_", colnames(exposures_wide))]
racism$exp_rac_disc_cv_max = apply(racism[,grep("exp", colnames(racism))], 1, max, na.rm = T)
racism$witness_rac_disc_cv_max = apply(racism[,grep("witness", colnames(racism))], 1, max, na.rm = T)
racism[racism == -Inf] = NA

###### worry
worry = exposures_wide[,grep("src_|worr", colnames(exposures_wide))]
worry$worry_y_cv_mean = rowMeans(worry[,grep("worry", colnames(worry))], na.rm = T)
worry$worried_cv = worry$worried_cv_cv2





covidy_final = merge(bedtime,exercise, all = T)
covidy_final = merge(covidy_final,sadness_scale_wide, all = T)
covidy_final = merge(covidy_final,mental_health_wide, all = T)
covidy_final = merge(covidy_final,money, all = T)
covidy_final = merge(covidy_final,outdoor_act, all = T)
covidy_final = merge(covidy_final,parents_monitor_wide, all = T)
covidy_final = merge(covidy_final,perceived_stress_wide, all = T)
covidy_final = merge(covidy_final,routine, all = T)
covidy_final = merge(covidy_final,screentime, all = T)
covidy_final = merge(covidy_final,stressful_events, all = T)
covidy_final = merge(covidy_final,substance_wide, all = T)
covidy_final = merge(covidy_final,racism, all = T)
covidy_final = merge(covidy_final,worry, all = T)

covidy_final = covidy_final[,grepl("src|mean|max|strle_tot|worried_cv$", colnames(covidy_final))]
covidy_final = covidy_final[!rowSums(is.na(covidy_final)) > 14,]


write.csv(covidy_final, "outputs/covidy_final.csv", row.names=F, na = "")


####################################
###### positive affect scale
####################################
positive_affect_scale = covidy[,grep("^(src_|_age|timepoint|attentive|calm|con|deli|ease|en(e|t)|intere|nih_posaff)", colnames(covidy))]
positive_affect_scale$nih_posaff_cv_raw_tot[positive_affect_scale$nih_posaff_cv_raw_tot >45 ] = NA
positive_affect_scale = positive_affect_scale[rowSums(is.na(positive_affect_scale)) != 10,]
positive_affect_scale_wide = reshape(positive_affect_scale, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

write.csv(positive_affect_scale_wide, "outputs/covidy_positive_affect_scale.csv", row.names=F, na = "")

