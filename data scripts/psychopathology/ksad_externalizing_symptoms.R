
source("config.R")
source("utility_fun.R")

ksad_p = load_instrument("abcd_ksad01",abcd_files_path)

#555 and 888 will be treated as NA
ksad_p[ksad_p == "888" | ksad_p == "555"] = NA

ksad_p = droplevels(ksad_p)

ksad_p = ksad_p[,grepl("^(src|inter|event|sex|ksads_7_(299|300))", colnames(ksad_p))]
ksad_p$anxiety_p = (ksad_p$ksads_7_300_p | ksad_p$ksads_7_299_p)*1

write.csv(file = "outputs/ksad_anxiety.csv",x = ksad_p, row.names=F, na = "")
