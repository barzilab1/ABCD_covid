
source("config.R")
source("utility_fun.R")

########### Discrimination ########### 
ydmes01 = load_instrument("abcd_ydmes01",exposome_files_path)

ydmes01[ydmes01 == 777 | ydmes01 == 999] = NA
ydmes01 = droplevels(ydmes01)

# summary(ydmes01[ydmes01$eventname == "1_year_follow_up_y_arm_1",])

#check collinearity 
library("psych")
matrix_names = colnames(ydmes01[ ,grep("_matrix_", colnames(ydmes01)) ])
ydmes01[,matrix_names] = apply(ydmes01[,matrix_names], 2, function(x) {as.numeric(as.character(x))})
xcor <- polychoric(ydmes01[,matrix_names])$rho
VSS.scree(xcor)
eigen(xcor)$values[1]/eigen(xcor)$values[2]

ydmes01[,c("dim_yesno_q3", "dim_yesno_q4")] = NULL
write.csv(ydmes01, "outputs/discrimination.csv", row.names=F, na = "")



########### family relationship section ########### 
acspsw03 = load_instrument("acspsw03",exposome_files_path)

acspsw03 = acspsw03[,c("src_subject_id","eventname","rel_family_id")]

summary(acspsw03)

write.csv(acspsw03, "outputs/family.csv", ,row.names=F, na = "")

