
source("config.R")
source("utility_fun.R")

########## ABCD Sum Scores Physical Health Parent ###########

ssphp01 = load_instrument("abcd_ssphp01", physicalhealth_files_path)

ssphp01 = ssphp01[,grepl("src|event|sex|inter|sds_p_ss_total$",colnames(ssphp01))]

write.csv(file = "outputs/physical_health.csv",x = ssphp01, row.names = F, na = "")


