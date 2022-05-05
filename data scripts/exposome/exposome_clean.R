
source("config.R")
source("utility_fun.R")


########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",abcd_files_path)

acspsw03 = acspsw03[acspsw03$eventname == "baseline_year_1_arm_1",c("src_subject_id","eventname","rel_family_id")]

summary(acspsw03)

write.csv(acspsw03, "outputs/family.csv", row.names=F, na = "")

