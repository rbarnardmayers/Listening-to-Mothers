# Chapter Variables

# Chapter 1 ----

cat_1 <- LTM_final %>%
  select(c("NUMB_BIRTH", 'PARITY', 'PREG_INT', starts_with("PREPREG_PH"),
           starts_with("PREPREG_MH"), 'PROVIDER', 'PROVIDERCHOICE',
           DOULA, DOULAC1, starts_with("DOULA1"), LEARNED1, LEARNED2, starts_with("CARESETTING"),
           starts_with("CARETYPE"), starts_with("CAREMODE"), WHYTELE,
           starts_with("ATHOMECARE"), CONFIDENCE_ANY, BPCONFID, URINECONFID, 
           WEIGHTCONFID, BABYHRCONFID, CURREDUC, PRIOREDUC, EDUCTYPE, EDUCMODE,
           EDUCIMPACT, starts_with("PREGCOND"), PHQ2_cat, GAD2_cat, PHQ4_cat, 
           MENTALSUPPORT, starts_with("SOCIALNEED"), EMPLOY, EMPLOYHOURS, 
           EMPLOYBEN, EMPLOYCHANGE, EMPLOYCHANGE1, BIGBABY1, BIGBABY2,
           starts_with("PLANNEDFEED"))) %>%
  colnames()

cont_1 <- LTM_final %>%
  select(c(PREPREG_WEIGHT, HEIGHT, BMI, PREG_WEIGHT, WEIGHTGAIN)) %>%
  colnames()

# Chapter 2 ----
cat_2 <- LTM_final %>%
  select(c(BIRTHWEIGHT_CAT, GOLDENHOUR,SKIN,HOSPLOC, NICU, BIRTHATTEND,
           ATTENDSTUDENT, DOULAC2, starts_with("DOULA2"), 
           OTHERSUPPORT, starts_with("INDUCE"), SDM, starts_with("MEDINDUCE"),
           MODE2023, starts_with("SELFINDUCE"), starts_with("MEDINDUCE"),
           starts_with("LABORPERMIT"), LABORWALK, POSITION, POSITIONCHOICE, 
           HOSPFEEDC8)) %>% 
  colnames()

cont_2 <-  LTM_final %>%
  select(c(AGEBIRTH, NUMBBIRTH, LABORLENGTH, DAYSHOSP,GESTAGE,
           BIRTHWEIGHT, BABYHOSP)) %>% 
  colnames()

# # Chapter 3 ----
# cat_3 <- LTM_final %>% 
#   select(c()) %>% colnames()
# cont_3 <- LTM_final %>% 
#   select(c()) %>% colnames()
# # Chapter 4 ----
# cat_4 <- LTM_final %>% 
#   select(c()) %>% colnames()
# 
# cont_4 <- LTM_final %>% 
#   select(c()) %>% colnames()
# 
# # Chapter 5 ----
# cat_5 <- LTM_final %>% 
#   select(c()) %>% colnames()
# 
# cont_5 <- LTM_final %>% 
#   select(c()) %>% colnames()



