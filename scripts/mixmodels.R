library(lme4)
library(lmerTest)
library(MuMIn)

covidp_long = read.csv("outputs/covidp_long.csv")
covidy_long = read.csv("outputs/covidy_long.csv")
family_ = read.csv("outputs/family.csv")
demo_baseline = read.csv("outputs/demographics_baseline.csv")


dataset = merge(covidp_long,covidy_long, all = T )
dataset = merge(dataset,family_, all.x = T )


demo_baseline[,c("interview_date" ,"interview_age" ,"eventname" )] = NULL
dataset = merge(dataset,demo_baseline, all.x = T )


dataset$felt_sad_cv_raw_tot_bar_Z = scale(dataset$felt_sad_cv_raw_tot_bar)
dataset$rel_family_id = dataset$rel_family_id + 1
dataset$timepoint = sub("cv", "" ,dataset$timepoint )




mod = lmer(felt_sad_cv_raw_tot_bar_Z ~ timepoint*fam_wage_loss_cv  + sex_br + race_white + race_black + ethnicity_hisp  +
       (timepoint  | rel_family_id/src_subject_id), 
     , data=dataset )


mod1 = lmer(felt_sad_cv_raw_tot_bar_Z ~ fam_wage_loss_cv  + sex_br + race_white + race_black + ethnicity_hisp  + interview_age+
             (1  | rel_family_id/src_subject_id), 
           , data=dataset )


mod11 = lmer(felt_sad_cv_raw_tot_bar_Z ~ fam_wage_loss_cv  + sex_br + race_white + race_black + ethnicity_hisp  + interview_age*fam_wage_loss_cv+
              (1  | rel_family_id/src_subject_id), 
            , data=dataset )


mod2 = lmer(felt_sad_cv_raw_tot_bar_Z ~ fam_wage_loss_cv  + sex_br + race_white + race_black + ethnicity_hisp  + interview_age+
              (1  | rel_family_id) + (interview_age|src_subject_id), 
            , data=dataset )

