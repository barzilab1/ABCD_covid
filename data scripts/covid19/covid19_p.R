library(psych)
library(plyr)

source("config.R")
source("utility_fun.R")


covidp_r1 = load_instrument("pabcdcovid19questionnaire01",covid19_1_3_files_path)
covidp_r2 = load_instrument("pabcdcovid19questionnaire01",covid19_4_6_files_path)

covidp = rbind.fill(covidp_r1, covidp_r2)
covidp[covidp == 777 | covidp == 999] = NA

#  new variable to use in reshape from long to wide format
covidp$timepoint = regmatches(covidp$eventnam, regexpr("cv[1-6]", covidp$eventnam))

#### remove timepoint 6 for now
covidp = covidp[covidp$timepoint != "cv6",]

covidp = covidp[,grep("src|timepoint|^fam_(a|w|dia|exp([1-7]|_rac))|increased_conflict|^child_(sep|tested|worried)|(to_|.?)school_(at|close_cv|cv)|work_ability", colnames(covidp), value = T)]
covidp_wide = reshape(covidp, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")

#### family actions
fam_actions = covidp[,grepl("src|timepoint|fam_a", colnames(covidp))]
fam_actions[,c("fam_actions_cv___14","fam_actions_cv___15")] = NULL
fam_actions$fam_isolate_tot_cv = rowSums(fam_actions[,grep("fam_", colnames(fam_actions))])
fam_actions_wide = reshape(fam_actions, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
fam_actions_wide = fam_actions_wide[,grep("src|tot", colnames(fam_actions_wide))]
# fam_actions_wide = fam_actions_wide[(rowSums(!is.na(fam_actions_wide)) > 3),]
fam_actions_wide$fam_isolate_tot_cv_mean = rowMeans(fam_actions_wide[,grep("tot", colnames(fam_actions_wide))], na.rm = T)

#### financial strain
financial_strain = covidp[,grepl("src|timepoint|fam_(w|exp[1-7])", colnames(covidp))]
financial_strain[,grep("fam_(w|exp[1-6])", colnames(financial_strain))] = financial_strain[,grep("fam_(w|exp[1-6])", colnames(financial_strain))] - 1
financial_strain$fam_exp_tot_cv = rowSums(financial_strain[,grep("exp[1-6]", colnames(financial_strain))])
financial_strain[,grep("exp[1-6]", colnames(financial_strain))] = NULL
financial_strain_wide = reshape(financial_strain, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
financial_strain_wide$fam_exp_tot_cv_mean = rowMeans(financial_strain_wide[,grep("tot", colnames(financial_strain_wide))], na.rm = T)
financial_strain_wide$fam_exp7_v2_cv_mean = rowMeans(financial_strain_wide[,grep("exp7", colnames(financial_strain_wide))], na.rm = T)
financial_strain_wide$fam_wage_loss_cv = Reduce("|",financial_strain_wide[,grep("wage", colnames(financial_strain_wide))])*1
financial_strain_wide$fam_wage_loss_cv = ifelse(is.na(financial_strain_wide$fam_wage_loss_cv), financial_strain_wide$fam_wage_loss_cv_cv5, financial_strain_wide$fam_wage_loss_cv)


#### increased_conflict
increased_conflict = covidp_wide[,grepl("src|conflict", colnames(covidp_wide))]
# increased_conflict = increased_conflict[(rowSums(!is.na(increased_conflict)) > 2),]
increased_conflict$increased_conflict_cv_mean = rowMeans(increased_conflict[,grep("increased_conflict", colnames(increased_conflict))], na.rm = T)

#### child_separate
child_separate = covidp_wide[,grepl("src|child_separate", colnames(covidp_wide))]
# child_separate = child_separate[(rowSums(!is.na(child_separate)) > 3),]
child_separate[,grep("child", colnames(child_separate))]  = child_separate[,grep("child", colnames(child_separate))] -1
child_separate$child_separate_cv_mean = rowMeans(child_separate[,grep("child", colnames(child_separate))], na.rm = T)

#### child test
child_test = covidp[,grepl("src|timepoint|test", colnames(covidp))]
child_test$child_tested_pos_cv = ifelse(child_test$child_tested_cv == 4, 1, 0)
child_test$child_tested_neg_cv = ifelse(child_test$child_tested_cv == 3, 1, 0)
child_test$child_tested_cv = NULL
child_test_wide = reshape(child_test, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
child_test_wide$child_tested_pos_cv = Reduce("|",child_test_wide[,grep("pos", colnames(child_test_wide))])*1
child_test_wide$child_tested_neg_cv = Reduce("|",child_test_wide[,grep("neg", colnames(child_test_wide))])*1
child_test_wide$child_tested_pos_cv = ifelse(is.na(child_test_wide$child_tested_pos_cv), child_test_wide$child_tested_pos_cv_cv5, child_test_wide$child_tested_pos_cv)
child_test_wide$child_tested_neg_cv = ifelse(is.na(child_test_wide$child_tested_neg_cv), child_test_wide$child_tested_neg_cv_cv5, child_test_wide$child_tested_neg_cv)

#### child worry finance 
child_worry = covidp_wide[,grepl("src|child_wor", colnames(covidp_wide))]
child_worry$child_worried_about_cv_mean = rowMeans(child_worry[,grep("child", colnames(child_worry))], na.rm = T)

#### family diagnosed with coronavirus
family_diagnosed = covidp_wide[,grepl("src|fam_d", colnames(covidp_wide))]
#remove kids with no data
family_diagnosed = family_diagnosed[rowSums(is.na(family_diagnosed)) != 5 ,]
family_diagnosed$fam_tested_pos_cv = ifelse(rowSums(family_diagnosed[,grep("fam", colnames(family_diagnosed))], na.rm = T) > 0, 1,0)

#### school
school = covidp_wide[,grepl("src|school", colnames(covidp_wide))]
school[school == 3] = NA
school$school_close_spring_2020_cv = school$school_close_cv_cv1 -1
school$school_close_fall_2020_cv = (school$school_at_home_cv_cv4 | school$school_at_home_cv_cv5)*1

school$went_to_school_cv = rowMeans(school[,grep("went", colnames(school))], na.rm = T) -1

#### work ability
work_ability = covidp_wide[,grepl("src|work", colnames(covidp_wide))]
work_ability$work_ability_cv_mean = rowMeans(work_ability[,grep("work", colnames(work_ability))], na.rm = T)

###### racism
racism = covidp_wide[,grepl("src|rac", colnames(covidp_wide))]
racism$fam_exp_racism_cv_mean = rowMeans(racism[,grep("rac", colnames(racism))], na.rm = T)




covidp_final = merge(fam_actions_wide,financial_strain_wide, all = T)
covidp_final = merge(covidp_final,increased_conflict , all = T)
covidp_final = merge(covidp_final,child_separate , all = T)
covidp_final = merge(covidp_final,child_test_wide, all = T)
covidp_final = merge(covidp_final,child_worry, all = T)
covidp_final = merge(covidp_final,family_diagnosed, all = T)
covidp_final = merge(covidp_final,school, all = T)
covidp_final = merge(covidp_final,work_ability, all = T)
covidp_final = merge(covidp_final,racism, all = T)

covidp_final = covidp_final[,-grep("cv[1-5]", colnames(covidp_final) )]

write.csv(covidp_final, "outputs/covidp_final.csv", row.names=F, na = "")

