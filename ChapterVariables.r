# Chapter Variables

# SELFINDUCE2C8, PPMEDSC4, POSITION2C7, NOPRENATALC13
# Chapter 1 ----

cat_1 <- LTM_final %>%
  select(c(PARITY, 'PREG_INT', starts_with("PREPREG_PH"),
           starts_with("PREPREG_MH"), 'PROVIDER', 'PROVIDERCHOICE',
           DOULA, DOULAC1, starts_with("DOULA1"), LEARNED1, LEARNED2, starts_with("CARESETTING"),
          starts_with("CARETYPE"), starts_with("CAREMODE"), starts_with("WHYTELE"),
          starts_with("ATHOMECARE"), CONFIDENCE_ANY, BPCONFID, URINECONFID,
          WEIGHCONFID, BABYHRCONFID, CURREDUC, PRIOREDUC, EDUCTYPE, starts_with("EDUCMODE"),
          starts_with("EDUCIMPACT"), starts_with("PREGCOND"), PHQ2_cat, GAD2_cat, PHQ4_cat,
          MENTALSUPPORT, starts_with("SOCIALNEED"), EMPLOY, EMPLOYHOURS,
          EMPLOYBEN, EMPLOYCHANGE, EMPLOYCHANGE1, BIGBABY1, BIGBABY2,
          starts_with("PLANNEDFEED")
          )) %>%
  select(-c(PREGCONDITIONC11, PREPREG_MHCONDC5, PREPREG_PHYSCONDC3,
            DOULA1C6, DOULA1C7, PREGCONDITIONC9)) %>%
  colnames()

cont_1 <- LTM_final %>%
  select(c(PREPREG_WEIGHT, HEIGHT, BMI, PREG_WEIGHT, WEIGHTGAIN, NUMB_BIRTH)) %>%
  colnames()

# Chapter 2 ----
# cat_2 <- LTM_final %>%
#   select(c(BIRTHWEIGHT_CAT, GOLDENHOUR,SKIN,HOSPLOC, NICU, BIRTHATTEND,
#            ATTENDSTUDENT, DOULAC2, starts_with("DOULA2"), 
#            starts_with("OTHERSUPPORT"), starts_with("INDUCE"), SDM, starts_with("MEDINDUCE"),
#            MODE2023, starts_with("SELFINDUCE"), starts_with("MEDINDUCE"),
#            starts_with("LABORPERMIT"), LABORWALK, POSITION, POSITIONCHOICE, 
#            HOSPFEEDC8)) %>% 
#   colnames()
# 
# cont_2 <-  LTM_final %>%
#   select(c(AGEBIRTH, NUMBBIRTH, LABORLENGTH, DAYSHOSP,GESTAGE,
#            BIRTHWEIGHT, BABYHOSP)) %>% 
#   colnames()

# # Chapter 3 ----
# CSECTIONTYPE by repeat vs. first time 
# VAGASSIST by VBAC vs. vaginal

# cat_3 <- LTM_final %>%
#   select(c(MODE2023, VAGASSIT, CSECTIONTYPE, UNPLANNEDREASON,
#            LABORLENGTH, REPEATCSEC,VBACCHOICE, VBACINTEREST)) %>% 
#   colnames()

# # Chapter 4 ----
# cat_4 <- LTM_final %>%
#   select(c(DOULA, DOULA2,PPVISIT, VISITTOPIC,FEED_CONCORDANT,
#            PLANNEDFEEDC1, PLANNEDFEEDC2, FEED1WEEKC1,FEED1WEEKC2,
#            starts_with("HOSPFEED"), starts_with("CURRENTFEED"),
#            EXCLUSIVEBF,EXCLBFGOAL,BFGOAL, PHQ4_cat,PHQ2_cat,GAD2_cat,
#            starts_with("PPBOTHER"),PPTHERAPY,starts_with("PPMED"),
#            starts_with("PREPREG_MHCOND"),starts_with("SOCIALNEEDC"),
#            SNMEAL, SNLIVE, SNUTILITIES, SNTRANSPORT, 
#            SNCHILDCARE, SNINCOME, SNDRUGS, SNUNSAFE, SNABUSE)) %>% colnames()
# 
# cont_4 <- LTM_final %>%
#   select(c(PPVISITTIME1, PPVISITTIME2)) %>% colnames()

# # Chapter 5 ----
# cat_5 <- LTM_final %>% 
#   select(c()) %>% colnames()
# 
# cont_5 <- LTM_final %>% 
#   select(c()) %>% colnames()
