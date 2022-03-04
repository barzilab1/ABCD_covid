source("config.R")
source("utility_fun.R")


demographics_long <- read.csv("outputs/demographics_long.csv")
demographics_baseline <- read.csv("outputs/demographics_baseline.csv")
covid_final <- read.csv("outputs/covid_final.csv")


covid_final_cv_id <-
  covid_final %>% select(src_subject_id) %>% pull()

demo_1year <-
  demographics_long %>% 
  select(
    src_subject_id,
    eventname,
    age,
    sex_br,
    parents_married,
    separated_or_divorced,
    household_income,
    demo_fam_poverty,
    parents_avg_edu
  ) %>% 
  filter(eventname == "1_year_follow_up_y_arm_1") %>%
  
  full_join(
    demographics_baseline %>% select(
      src_subject_id,
      race_white,
      race_black,
      race_asian,
      ethnicity_hisp
    ),
    by = "src_subject_id"
  ) %>%
  
  mutate(participated_cv = case_when(src_subject_id %in% covid_final_cv_id ~ 1,
                                     TRUE ~ 0))

write.csv(file = "outputs/demo_1year.csv",x = one_family_member, row.names=F, na = "")
