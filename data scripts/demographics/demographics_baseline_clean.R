##############################################
#' most of the code below is adjusted from 
#' the abcd official github:
#' https://github.com/ABCD-STUDY/analysis-nda 
##############################################

library(data.table)

source("config.R")
source("utility_fun.R")

demographics_set = load_instrument("pdem02",demographics_files_path)

########### rearrange data ########### 
###1. convert variables names to be more readable  
###2. change outliers (777,999) to be NA
demographics_set = data.table(demographics_set)

########### sex
#convert the NIH sex at birth (equal to demo_sex_v2)
demographics_set[, sex_br := (sex == "F")*1]
demographics_set[,demo_sex_v2 := NULL]

########### age
#interview age will be used instead of age
demographics_set[,demo_brthdat_v2:=NULL] 
demographics_set[, age := interview_age]

########### gender
demographics_set[,gender := demo_gender_id_v2]
demographics_set[(gender %in%  c(777,999)) ,gender := NA] 
demographics_set[, gender:= gender-1]
demographics_set[, demo_gender_id_v2:= NULL]

########### ethnicity
demographics_set[demo_ethn_v2 == 1, ethnicity_hisp := 1]
demographics_set[demo_ethn_v2 == 2, ethnicity_hisp := 0]
demographics_set[(demo_ethn_v2 %in% c(777,999)), ethnicity_hisp:= NA]
demographics_set = demographics_set[, demo_ethn_v2 := NULL]


########### child race
#"refuse to answer" and "dont know" will be 0

# White
demographics_set[, race_white:= demo_race_a_p___10 ]

# Black
demographics_set[, race_black:= demo_race_a_p___11  ]

# Asian
demographics_set[, race_asian:= 0]
demographics_set[ (demo_race_a_p___18 == 1 | demo_race_a_p___19 == 1 | demo_race_a_p___20 == 1 |
        demo_race_a_p___21 == 1 | demo_race_a_p___22 == 1 | demo_race_a_p___23 == 1 |
        demo_race_a_p___24 ==1), race_asian:= 1 ]

# AIAN: American Indian and Alaska Native
demographics_set[, race_aian:= 0]
demographics_set[ (demo_race_a_p___12 == 1 | demo_race_a_p___13 == 1), race_aian:=1 ]


#NHPI: Native Hawaiian and Other Pacific
demographics_set[, race_nhpi:= 0]
demographics_set[ demo_race_a_p___14 == 1 | demo_race_a_p___15 == 1 | demo_race_a_p___16 == 1 |
       demo_race_a_p___17 == 1, race_nhpi:= 1 ]

# Other
demographics_set[, race_other:= 0 ]
demographics_set[ demo_race_a_p___25 == 1, race_other:= 1 ]

# Mixed
demographics_set[, race_mixed:= (race_white + race_black + race_asian + race_aian + race_nhpi + race_other)]
demographics_set[, table(race_mixed, useNA = "if")]
demographics_set[ race_mixed <= 1, race_mixed:= 0]
demographics_set[ race_mixed > 1, race_mixed:= 1]
demographics_set[, table(race_mixed, useNA = "if")]

demographics_set[, grep("^demo_race_a_p___",colnames(demographics_set), value = T) := NULL]


########### child's country of birth 
demographics_set[, born_in_usa := 0]
demographics_set[demo_origin_v2 %in% c(777,999), born_in_usa := NA]
demographics_set[demo_origin_v2 == 189, born_in_usa := 1]


########### parents education
demographics_set[(demo_prnt_ed_v2 %in%  c(777,999)), demo_prnt_ed_v2:= NA]
demographics_set[(demo_prtnr_ed_v2 %in%  c(777,999)), demo_prtnr_ed_v2:= NA]
demographics_set[, parents_avg_edu:= (demo_prnt_ed_v2 + demo_prtnr_ed_v2)/2]
#in case of edu is missing in one of the parents, it will be the edu the of other parent 
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= demo_prnt_ed_v2]
demographics_set[is.na(parents_avg_edu), parents_avg_edu:= demo_prtnr_ed_v2]

########### family income 
demographics_set[,household_income:= demo_comb_income_v2]
demographics_set[( household_income %in%  c(777,999)) ,household_income:= NA]
demographics_set[,demo_comb_income_v2 := NULL]

########### parents married status 
demographics_set[demo_prnt_marital_v2 == 777 , demo_prnt_marital_v2:= NA]

demographics_set[,separated_or_divorced := 0]
demographics_set[(demo_prnt_marital_v2 %in%  c(3,4)), separated_or_divorced := 1]
demographics_set[is.na(demo_prnt_marital_v2), separated_or_divorced := NA]

demographics_set[,parents_married := 0]
demographics_set[(demo_prnt_marital_v2 == 1), parents_married := 1]
demographics_set[is.na(demo_prnt_marital_v2), parents_married := NA]

demographics_set[,living_with_partenr_or_married := 0]
demographics_set[(demo_prnt_marital_v2 %in% c(1,6)), living_with_partenr := 1]
demographics_set[is.na(demo_prnt_marital_v2), living_with_partenr := NA]


########### economic hardship
economic_hardship_names = grep("demo_fam_exp", colnames(demographics_set),value = T)
for(name in economic_hardship_names){
  set(demographics_set,i = which(demographics_set[[name]] == 777), j= name, value = NA)
}


library("psych")
xcor <- polychoric(as.data.frame(demographics_set)[ ,economic_hardship_names ])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

demographics_set[, demo_fam_poverty := rowSums(.SD, na.rm = T), .SDcols = economic_hardship_names]
# demographics_set[ , View(.SD), .SDcols = c(economic_hardship_names, "demo_fam_poverty") ]
demographics_set[rowSums(is.na(demographics_set[,.SD,.SDcols = economic_hardship_names])) == 7 , "demo_fam_poverty" := NA]



demographics_set = droplevels(demographics_set)


#remove irrelevant columns
demographics_set [, c("demo_adopt_agex_v2_bl_dk","demo_years_us_v2_dk" ) := NULL]

#remove outliers of "number of household members" 
demographics_set[demo_roster_v2 %in% c(60,77), demo_roster_v2:= NA]



selected_features = c("src_subject_id", "eventname", "sex",
                      "race_white", "race_black", "race_aian", "race_nhpi", "race_asian", "race_other","race_mixed" ,"ethnicity_hisp",
                      "born_in_usa", "household_income", "age", "sex_br", "gender",
                      "parents_avg_edu", "separated_or_divorced","parents_married" ,"demo_years_us_v2",
                      economic_hardship_names, "demo_fam_poverty")

write.csv(file = "outputs/demographics_baseline.csv",x = demographics_set[,.SD,.SDcols = selected_features], row.names=F, na = "")








