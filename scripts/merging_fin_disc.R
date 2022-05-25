library(readr)
demographics_baseline <- read.csv("outputs/demographics_baseline.csv")
family <- read.csv("outputs/family.csv")
site <- read.csv("outputs/site.csv")


demographics_long <- read.csv("outputs/demographics_long.csv")

covidy_final <- read_csv("outputs/covidy_final.csv")
covidp_final <- read_csv("outputs/covidp_final.csv")


p_factor <-  read_csv("~/Box Sync/2. Barzi Lab - Restricted Access/2-ABCD Data Files/Projects/exposome/3.0/data/p factor scores/ABCD_psychopathology_bifactor_scores_23March2021.csv")
p_factor_with_sui <-  read_csv("~/Box Sync/2. Barzi Lab - Restricted Access/2-ABCD Data Files/Projects/exposome/3.0/data/p factor scores/ABCD_psychopathology_bifactor_scores_23March2021_WITH_SUICIDALITY.csv")

p_factor$src_subject_id = paste0("NDAR_", p_factor$ID)
p_factor$ID = NULL

p_factor_with_sui$src_subject_id = paste0("NDAR_", p_factor_with_sui$ID)
p_factor_with_sui$ID = NULL
colnames(p_factor_with_sui)[1:7] = paste0(colnames(p_factor_with_sui)[1:7], "_with_sui")



covars = merge(site[site$eventname == "1_year_follow_up_y_arm_1", c("src_subject_id","site_id_l_br")], 
               family[family$eventname == "baseline_year_1_arm_1", c("src_subject_id", "rel_family_id")])

covars = merge(covars, demographics_baseline[,grep("src_|race|hisp|born|year", colnames(demographics_baseline))])


demographics_long = demographics_long[,grep("src_|inter|sex|event|pare|divo|demo_f|inco|age", colnames(demographics_long))]

IVs = demographics_long
IVs = IVs[(IVs$eventname == "1_year_follow_up_y_arm_1") , ]

colnames(IVs)[2] = "interview_date_before_covid"
colnames(IVs)[3] = "interview_age_before_covid"
IVs$eventname = NULL


covid_final = merge(covidy_final,covidp_final, all =T)
covid_final = merge(covid_final, covars)
covid_final = merge(covid_final, IVs)
covid_final = merge(covid_final, p_factor, all.x = T)
covid_final = merge(covid_final, p_factor_with_sui, all.x = T)


write.csv(file = "outputs/covid_final.csv",x = covid_final, row.names=F, na = "")



library(data.table)
set.seed(131)
one_family_member = setDT(covid_final)[, sample(src_subject_id, 1) ,by = rel_family_id]

write.csv(file = "outputs/one_family_member.csv",x = one_family_member, row.names=F, na = "")



