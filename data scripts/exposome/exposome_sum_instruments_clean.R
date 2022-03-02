
source("config.R")
source("utility_fun.R")

########### Sum Scores Culture & Environment Youth ########### 
sscey01 = load_instrument("abcd_sscey01",abcd_files_path)

sscey01 = sscey01[, grepl("^(src|interview|event|sex|pmq|fes|srpf|dim)", colnames(sscey01))]

#remove nt (Number Total Questions) and nm (Number Missing Answers) and na (Number Answered)
sscey01 = sscey01[,!grepl("_(nm|nt|na|answered|pr)$",colnames(sscey01))] #pr


summary(sscey01)



########### Sum Scores Mobil Tech Youth ########### 
ssmty = load_instrument("abcd_ssmty01",abcd_files_path)

ssmty = ssmty[, grepl("(src|interview|event|sex)|_(weekend|weekday)$", colnames(ssmty))]
ssmty$stq_y_total_mean = rowMeans(ssmty[,grep("_ss_",colnames(ssmty))], na.rm = T)

summary(ssmty)





########### merge all tables
exposome_set = merge(ssmty,sscey01)


write.csv(file = "outputs/exposome_sum_set.csv",x = exposome_set, row.names = F, na = "")





