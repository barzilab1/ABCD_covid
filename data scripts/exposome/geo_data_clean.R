
source("config.R")
source("utility_fun.R")


########### School Risk and Protective Factors ########### 
rhds01 = load_instrument("abcd_rhds01",exposome_files_path)

rhds01 = rhds01[, grepl("^(src|interview|event|sex)|addr1_adi_perc", colnames(rhds01))]



summary(droplevels(rhds01[rhds01$eventname == "1_year_follow_up_y_arm_1",]))
summary(droplevels(rhds01[rhds01$eventname == "baseline_year_1_arm_1",]))

write.csv(file = "outputs/geo_data.csv",x = rhds01, row.names = F, na = "")


