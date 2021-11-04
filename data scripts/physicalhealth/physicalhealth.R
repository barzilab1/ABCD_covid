
source("config.R")
source("utility_fun.R")

########### ABCD Parent Medical History Questionnaire (MHX) ########### 

mx01 = load_instrument("abcd_mx01",physicalhealth_files_path)

#select variables
mx01 = mx01[,grepl("src|interview|event|sex|(2(a|b|d|g|m)|6(a|l))$",colnames(mx01))]


########### ABCD Parent Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ########### 

ppdms = load_instrument("abcd_ppdms01",physicalhealth_files_path)

#"Don't know" will be treated as NA
ppdms[ppdms == "999"] = NA

#remove empty col
ppdms = ppdms[, !colSums(is.na(ppdms)) == nrow(ppdms)]
ppdms$pds_select_language___1 = NULL

#fix scale 
ppdms$pds_f5b_p = as.numeric(as.character(ppdms$pds_f5b_p)) - 1
ppdms$pds_f5b_p[ppdms$pds_f5b_p == 3] = 1

ppdms$pds_f6_p[ppdms$pds_f6_p == "99.0"] = NA

summary(droplevels(ppdms))


########### ABCD Youth Pubertal Development Scale and Menstrual Cycle Survey History (PDMS) ########### 

ypdms = load_instrument("abcd_ypdms01",physicalhealth_files_path)

#"Don't know" and "Decline to answer" will be treated as NA
ypdms[ypdms == "777" | ypdms == "999"] = NA

#remove empty col
ypdms = ypdms[, !colSums(is.na(ypdms)) == nrow(ypdms)]


########### ABCD Youth Youth Risk Behavior Survey Exercise Physical Activity (YRB) ########### 

yrb = load_instrument("abcd_yrb01",physicalhealth_files_path)

#select variables
yrb = yrb[,grepl("src|interview|event|sex|physical_activity(1|2)_y",colnames(yrb))]

#change scale 
yrb$physical_activity2_y = as.numeric(as.character(yrb$physical_activity2_y)) - 1




# physicalhealth_1year = merge(ppdms,ypdms)
# write.csv(file = "outputs/physicalhealth_1year.csv",x = physicalhealth_1year, row.names = F, na = "")
# 
# physicalhealth_baseline = merge(mx01,yrb)
# write.csv(file = "outputs/physicalhealth_baseline.csv",x = physicalhealth_baseline, row.names = F, na = "")
# 

