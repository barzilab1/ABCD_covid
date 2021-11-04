library(plyr)


demographics_baseline <- read.csv("outputs/demographics_baseline.csv")
demographics_long <- read.csv("outputs/demographics_long.csv")
family <- read.csv("outputs/family.csv")
site <- read.csv("outputs/site.csv")
geo_data <- read.csv("outputs/geo_data.csv")

mental_health <- read.csv("outputs/mental_health_wide.csv")
perceived_stress <- read.csv("outputs/perceived_stress_wide.csv")
sadness_scale <- read.csv("outputs/sadness_scale_wide.csv")
substance <- read.csv("outputs/substance_wide.csv")

covars = merge(site[site$eventname == "baseline_year_1_arm_1", c("src_subject_id","site_id_l_br")], 
               family[family$eventname == "baseline_year_1_arm_1", c("src_subject_id", "rel_family_id")])

covars = merge(covars, demographics_baseline[,grep("src_|race|hisp|born|year", colnames(demographics_baseline))])

### take the most updated geo
geo_data$timepoint = regmatches(geo_data$eventname, regexpr("(.*)_year", geo_data$eventname))
geo_data = geo_data[,c("src_subject_id","timepoint","reshist_addr1_adi_perc")]
geo_data_w = reshape(geo_data, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "_")
geo_data_w$reshist_addr1_adi_perc_bar = ifelse(!(is.na(geo_data_w$reshist_addr1_adi_perc_2_year)), geo_data_w$reshist_addr1_adi_perc_2_year,
                                        ifelse(!(is.na(geo_data_w$reshist_addr1_adi_perc_1_year)), geo_data_w$reshist_addr1_adi_perc_1_year,
                                               geo_data_w$reshist_addr1_adi_perc_baseline_year ))

covars = merge(covars,geo_data_w[,c("src_subject_id", "reshist_addr1_adi_perc_bar")])

### get demographics 2 year follow up -> 1 year follow up -> baseline 
ids = union(mental_health$src_subject_id, perceived_stress$src_subject_id)
ids = union(ids, sadness_scale$src_subject_id)
ids = union(ids, substance$src_subject_id)

ids_2_year = ids[ (ids %in% demographics_long$src_subject_id[demographics_long$eventname == "2_year_follow_up_y_arm_1"])]
ids_1_year = ids[ !(ids %in% ids_2_year) &
  (ids %in% demographics_long$src_subject_id[demographics_long$eventname == "1_year_follow_up_y_arm_1"])]
ids_base = ids[ !(ids %in% ids_2_year) & !(ids %in% ids_1_year)]


demographics_long = demographics_long[,grep("src_|inter|sex|event|pare|divo|demo_f|inco|age", colnames(demographics_long))]

# get data from 2 year follow up
demographics_2_years = demographics_long[(demographics_long$src_subject_id %in% ids_2_year) & (demographics_long$eventname == "2_year_follow_up_y_arm_1"),]
# remove kids that dont have data in the required features and add them to 1 year ids
demographics_2_years = demographics_2_years[rowSums(is.na(demographics_2_years)) < 9,]
ids_missimg_2 = setdiff(ids_2_year,demographics_2_years$src_subject_id)
ids_1_year = union(ids_missimg_2,ids_1_year)
# parents_avg_edu is empty, household_income missing 279, parents status missing 20

demographics_2_years$parents_avg_edu = NULL

# get 2 year missing data from the 1 year data 
demographics_2_years_sup = demographics_long[(demographics_long$src_subject_id %in% ids_2_year) & (demographics_long$eventname == "1_year_follow_up_y_arm_1"), 
                                             c("src_subject_id", "household_income","parents_married","separated_or_divorced","parents_avg_edu")]
colnames(demographics_2_years_sup)[2:4] = paste0(colnames(demographics_2_years_sup)[2:4],"_sup")
demographics_2_years = merge(demographics_2_years,demographics_2_years_sup, all.x = T)
demographics_2_years$household_income =      ifelse(is.na(demographics_2_years$household_income),     demographics_2_years$household_income_sup,      demographics_2_years$household_income)
demographics_2_years$parents_married =       ifelse(is.na(demographics_2_years$parents_married),      demographics_2_years$parents_married_sup,       demographics_2_years$parents_married)
demographics_2_years$separated_or_divorced = ifelse(is.na(demographics_2_years$separated_or_divorced),demographics_2_years$separated_or_divorced_sup, demographics_2_years$separated_or_divorced)
demographics_2_years[,grep("_sup$",colnames(demographics_2_years))] = NULL
# parents_avg_edu missing 58, household_income missing 133, parents status missing 3

# get 2 year missing data from baseline 
demographics_2_years_sup = demographics_baseline[(demographics_baseline$src_subject_id %in% ids_2_year) , 
                                             c("src_subject_id", "household_income","parents_married","separated_or_divorced","parents_avg_edu")]
colnames(demographics_2_years_sup)[2:5] = paste0(colnames(demographics_2_years_sup)[2:5],"_sup")
demographics_2_years = merge(demographics_2_years,demographics_2_years_sup, all.x = T)
demographics_2_years$household_income =      ifelse(is.na(demographics_2_years$household_income),demographics_2_years$household_income_sup , demographics_2_years$household_income)
demographics_2_years$parents_married =       ifelse(is.na(demographics_2_years$parents_married),demographics_2_years$parents_married_sup , demographics_2_years$parents_married)
demographics_2_years$separated_or_divorced = ifelse(is.na(demographics_2_years$separated_or_divorced),demographics_2_years$separated_or_divorced_sup , demographics_2_years$separated_or_divorced)
demographics_2_years$parents_avg_edu =       ifelse(is.na(demographics_2_years$parents_avg_edu),demographics_2_years$parents_avg_edu_sup , demographics_2_years$parents_avg_edu)
demographics_2_years[,grep("_sup$",colnames(demographics_2_years))] = NULL
# parents_avg_edu missing 1, household_income missing 80, parents status missing 2

# get data from 1 year follow up
demographics_1_years = demographics_long[(demographics_long$src_subject_id %in% ids_1_year) & (demographics_long$eventname == "1_year_follow_up_y_arm_1"),]
demographics_1_years = demographics_1_years[rowSums(is.na(demographics_1_years)) < 9,]
ids_missimg_1 = setdiff(ids_1_year,demographics_1_years$src_subject_id)
ids_base = union(ids_missimg_1, ids_base)
# parents_avg_edu missing 5, household_income missing 217, parents status missing 11

# get 1 year missing data from baseline 
demographics_1_years_sup = demographics_baseline[(demographics_baseline$src_subject_id %in% ids_1_year) , 
                                                 c("src_subject_id", "household_income","parents_married","separated_or_divorced","parents_avg_edu")]
colnames(demographics_1_years_sup)[2:5] = paste0(colnames(demographics_1_years_sup)[2:5],"_sup")
demographics_1_years = merge(demographics_1_years,demographics_1_years_sup, all.x = T)
demographics_1_years$household_income = ifelse(is.na(demographics_1_years$household_income),demographics_1_years$household_income_sup , demographics_1_years$household_income)
demographics_1_years$parents_married = ifelse(is.na(demographics_1_years$parents_married),demographics_1_years$parents_married_sup , demographics_1_years$parents_married)
demographics_1_years$separated_or_divorced = ifelse(is.na(demographics_1_years$separated_or_divorced),demographics_1_years$separated_or_divorced_sup , demographics_1_years$separated_or_divorced)
demographics_1_years$parents_avg_edu = ifelse(is.na(demographics_1_years$parents_avg_edu),demographics_1_years$parents_avg_edu_sup , demographics_1_years$parents_avg_edu)
demographics_1_years[,grep("_sup$",colnames(demographics_1_years))] = NULL
# parents_avg_edu no missing data, household_income missing 98, parents status missing 2

# get data from baseline
demographics_base = demographics_baseline[(demographics_baseline$src_subject_id %in% ids_base) ,grep("src_|inter|sex|event|pare|divo|demo_f|inco|age", colnames(demographics_baseline)) ]

demographics = rbind.fill(demographics_2_years, demographics_1_years)
demographics = rbind.fill(demographics, demographics_base)
demographics[,c("sex","eventname","interview_age", "demo_years_us_v2",grep("fam_exp", colnames(demographics),value = T))] = NULL
colnames(demographics)[2] = "interview_date_before_covid"
colnames(demographics)[6] = "interview_age_before_covid"

covars = merge(covars,demographics)


mental_health = merge(mental_health, covars)
perceived_stress = merge(perceived_stress, covars)
sadness_scale = merge(sadness_scale, covars)
substance = merge(substance, covars)


write.csv(file = "outputs/mental_health.csv", x= mental_health, row.names=F, na = "")
write.csv(file = "outputs/perceived_stress.csv", x= perceived_stress, row.names=F, na = "")
write.csv(file = "outputs/sadness_scale.csv", x= sadness_scale, row.names=F, na = "")
write.csv(file = "outputs/substance.csv", x= substance, row.names=F, na = "")




psychopathology <- read.csv("outputs/psychopathology.csv")
psychopathology_sum <- read.csv("outputs/psychopathology_sum_scores.csv")
covidy_pas <- read.csv("outputs/covidy_positive_affect_scale.csv")

pre_covid_pas = merge(psychopathology, psychopathology_sum, all =T)
pre_covid_pas$timepoint = gsub("_follow_up(.*)", "", pre_covid_pas$eventname)
pre_covid_pas[,c("eventname","sex")] = NULL
pre_covid_pas_wide = reshape(pre_covid_pas, direction = "wide", idvar = "src_subject_id", timevar = "timepoint", sep = "__")


covidy_pas = merge(pre_covid_pas_wide,covidy_pas)
write.csv(file = "outputs/covidy_pas.csv", x= covidy_pas, row.names=F, na = "")



