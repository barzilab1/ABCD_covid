
source("config.R")
source("utility_fun.R")

ksad_p = load_instrument("abcd_ksad01",psychopathology_files_path)
ksad_y = load_instrument("abcd_ksad501",psychopathology_files_path)

#555 and 888 will be NA
ksad_p[ksad_p == "888" | ksad_p == "555"] = NA
ksad_y[ksad_y == "888" | ksad_y == "555"] = NA

ksad_p = droplevels(ksad_p)
ksad_y = droplevels(ksad_y)



##calculate suicidality
#if one of the items is 1, the result will be 1
#if one of the items is NA and the rest is 0, the result will be NA

#ideation
ksad_p$SI_current_p = apply(ksad_p[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_current_y = apply(ksad_y[,which(grepl("ksads_23_(946|947|948|949|950|951)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SI_past_p = apply(ksad_p[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SI_past_y = apply(ksad_y[,which(grepl("ksads_23_(957|958|959|960|961|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SI_p = (ksad_p$SI_current_p == 1 | ksad_p$SI_past_p == 1)*1
ksad_y$SI_y = (ksad_y$SI_current_y == 1 | ksad_y$SI_past_y == 1)*1


#attempt
ksad_p$SA_current_p = apply(ksad_p[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_current_y = apply(ksad_y[,which(grepl("ksads_23_(952|953|954)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SA_past_p = apply(ksad_p[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$SA_past_y = apply(ksad_y[,which(grepl("ksads_23_(963|964|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$SA_p = (ksad_p$SA_current_p == 1 | ksad_p$SA_past_p == 1)*1
ksad_y$SA_y = (ksad_y$SA_current_y == 1 | ksad_y$SA_past_y == 1)*1

#combine ideation and attempt
ksad_p$suicidality_p = (ksad_p$SI_p == 1 | ksad_p$SA_p == 1)*1
ksad_y$suicidality_y = (ksad_y$SI_y == 1 | ksad_y$SA_y == 1)*1
  
#nssi
ksad_p$nssi_current_p = apply(ksad_p[,which(grepl("ksads_23_(945|955)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$nssi_current_y = apply(ksad_y[,which(grepl("ksads_23_(945|955)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

ksad_p$nssi_past_p = apply(ksad_p[,which(grepl("ksads_23_(956|966)", colnames(ksad_p)))],1 ,function(x) {any(x == 1)*1})
ksad_y$nssi_past_y = apply(ksad_y[,which(grepl("ksads_23_(956|966)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})

##########################################
### for each past/current pair, create one feature 
### for youth only

ksad_y$symptom_self_injurious_behavior = apply(ksad_y[,which(grepl("ksads_23_(143|144)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_wishes_better_off_dead = apply(ksad_y[,which(grepl("ksads_23_(145|146)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_SI = apply(ksad_y[,which(grepl("ksads_23_(147|148)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$symptom_SA = apply(ksad_y[,which(grepl("ksads_23_(149|150)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})


ksad_y$diag_SelfInjuriousBehaviorwithoutsuicidalintent = apply(ksad_y[,which(grepl("ksads_23_(945|956)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationPassive = apply(ksad_y[,which(grepl("ksads_23_(946|957)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActivenonspecific = apply(ksad_y[,which(grepl("ksads_23_(947|958)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActivemethod = apply(ksad_y[,which(grepl("ksads_23_(948|959)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActiveintent = apply(ksad_y[,which(grepl("ksads_23_(949|960)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicidalideationActiveplan = apply(ksad_y[,which(grepl("ksads_23_(950|961)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_PreparatoryActionstowardimminentSuicidalbehavior = apply(ksad_y[,which(grepl("ksads_23_(951|962)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_InterruptedAttempt = apply(ksad_y[,which(grepl("ksads_23_(952|963)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_AbortedAttempt = apply(ksad_y[,which(grepl("ksads_23_(953|964)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_SuicideAttempt = apply(ksad_y[,which(grepl("ksads_23_(954|965)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})
ksad_y$diag_Nosuicidalideationorbehavior = apply(ksad_y[,which(grepl("ksads_23_(955|966)", colnames(ksad_y)))],1 ,function(x) {any(x == 1)*1})



##########################################


#create separte variables for baseline and 1 year follow up

suicide_set = merge(ksad_y,ksad_p)
suicide_set_baseline = suicide_set[which(grepl("baseline", suicide_set$eventname)),which(grepl("ksads_23_|src|inter|event|sex|SI|SA|sui", colnames(suicide_set)))]
# suicide_set_1_year_follow = suicide_set[which(grepl("1_year_follow_up", suicide_set$eventname)),which(grepl("ksads_23_|src|inter|event|gender|SI|SA|sui", colnames(suicide_set)))]

write.csv(file = "outputs/suicide_baseline.csv",x = suicide_set_baseline,row.names=F, na = "")
# write.csv(file = "suicide/outputs/suicide_1_year.csv",x = suicide_set_1_year_follow,row.names=F, na = "")

suicide_set_1_year_follow = suicide_set[which(grepl("1_year_follow_up", suicide_set$eventname)),which(grepl("src|interview_a|sex|SI_|SA_|sui", colnames(suicide_set)))]

suicide_set_1_year_follow$suicidality_current_1_year_p = (suicide_set_1_year_follow$SI_current_p == 1 | suicide_set_1_year_follow$SA_current_p == 1)*1
suicide_set_1_year_follow$suicidality_current_1_year_y = (suicide_set_1_year_follow$SI_current_y == 1 | suicide_set_1_year_follow$SA_current_y == 1)*1
suicide_set_1_year_follow$suicidality_past_1_year_y = (suicide_set_1_year_follow$SI_past_y == 1 | suicide_set_1_year_follow$SA_past_y == 1)*1

#remove parent report, they all empty 
suicide_set_1_year_follow = suicide_set_1_year_follow[,which(colSums(is.na(suicide_set_1_year_follow)) < 11234)]

#update names
colnames(suicide_set_1_year_follow)[2] = "interview_age_1_year"
colnames(suicide_set_1_year_follow)[4] = "SI_current_1_year_y"
colnames(suicide_set_1_year_follow)[5] = "SI_past_1_year_y"
colnames(suicide_set_1_year_follow)[6] = "SI_1_year_y"
colnames(suicide_set_1_year_follow)[7] = "SA_current_1_year_y"
colnames(suicide_set_1_year_follow)[8] = "SA_past_1_year_y"
colnames(suicide_set_1_year_follow)[9] = "SA_1_year_y"
colnames(suicide_set_1_year_follow)[10] = "suicidality_1_year_y"


suicide_firstyear_ontopof_baseline = merge(suicide_set_baseline, suicide_set_1_year_follow , all.x = T)
write.csv(file = "outputs/suicide_firstyear_ontopof_baseline.csv",x = suicide_firstyear_ontopof_baseline,row.names=F, na = "")
# write.csv(file = "suicide/outputs/suicide_firstyear_ontopof_baseline_grant.csv",x = suicide_firstyear_ontopof_baseline,row.names=F, na = "")


