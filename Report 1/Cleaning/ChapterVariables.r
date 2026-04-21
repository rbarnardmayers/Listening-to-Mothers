# Chapter Variables

group_vars <- LTM_final %>% 
  select(c(RACEALONE, PROVIDER, INSURCAT, MODE2023)) %>% 
  colnames()

# SELFINDUCE2C8, PPMEDSC4, POSITION2C7, NOPRENATALC13
# Chapter 1 ----

cat_1 <- LTM_final %>%
  select(c(NUMB_BIRTH, 'PREG_INT', starts_with("PREPREG_PH"),
           starts_with("PREPREG_MH"), 'PROVIDER', 'PROVIDERCHOICE',
           DOULA, DOULAC1, starts_with("DOULA1"), starts_with("CARESETTING"),
           CAREMODEC1, CAREMODEC2, CAREMODEPREF,
           starts_with("WHYTELE"),starts_with("CARETYPE"),
          starts_with("ATHOMECARE"), CONFIDENCE_ANY, BPCONFID, URINECONFID,
          WEIGHCONFID, BABYHRCONFID, CURREDUC, PRIOREDUC, EDUCTYPE, starts_with("EDUCMODE"),
          starts_with("EDUCIMPACT"), starts_with("PREGCOND"), PHQ2_cat, GAD2_cat, PHQ4_cat,
          MENTALSUPPORT, starts_with("SOCIALNEED"), EMPLOY, EMPLOYHOURS,
          EMPLOYBEN, EMPLOYCHANGE, EMPLOYCHANGE1, BIGBABY1, BIGBABY2,
          starts_with("PLANNEDFEED")
          )) %>%
  select(-c( PREPREG_PHYSCONDC3,WHYTELEC7O,WHYTELEC8,CARETYPEC3,ATHOMECAREC6,
             EDUCMODEC5,PLANNEDFEEDC4,PLANNEDFEEDC3,SOCIALNEEDC11,DOULA1C8,
             SOCIALNEEDC10,PREGCONDITIONC11,EDUCIMPACTC8,CARESETTINGC9,
             PREPREG_MHCONDC7,PREPREG_PHYSCONDC5,
            ends_with("O"))) %>%
  colnames()

cont_1 <- LTM_final %>%
  select(c(PREPREG_WEIGHT, HEIGHT, BMI, PREG_WEIGHT, WEIGHTGAIN, 
           NUMB_BIRTH, LEARNED1, LEARNED2)) %>%
  colnames()

# Chapter 2 ----
cat_2 <- LTM_final %>%
  select(c(BW_CAT, GOLDENHOUR,SKIN,HOSPLOC, NICU, BIRTHATTEND,
           ATTENDSTUDENT, DOULAC2, starts_with("DOULA2"),
           starts_with("OTHERSUPPORT"), starts_with("INDUCE"), SDM, starts_with("MEDINDUCE"),
           MODE2023, starts_with("SELFINDUCE"), starts_with("MEDINDUCE"),
           starts_with("LABORPERMIT"), LABORWALK, POSITION, POSITIONCHOICE,
           HOSPFEEDC8)) %>%
  select(-c(INDUCE6, MEDINDUCE3C8, MEDINDUCE4, MEDINDUCE5, SELFINDUCE2C8, 
            # LABORPERMIT_D1, LABORPERMIT_D2, LABORPERMIT_E1, LABORPERMIT_E2, 
            ends_with("O"))) %>%
  colnames()

cont_2 <-  LTM_final %>%
  select(c(#AGEBIRTH, 
           NUMB_BIRTH, LABORLENGTH, DAYSHOSP,#GESTAGE,
           BIRTHWEIGHT, BABYHOSP, MEDINDUCE5, MEDINDUCE4)) %>%
  colnames()

# # Chapter 3 ----
# CSECTIONTYPE by repeat vs. first time 
# VAGASSIST by VBAC vs. vaginal

cat_3 <- LTM_final %>%
  select(c(MODE2023, VAGASSIST, CSECTIONTYPE, UNPLANNEDREASON,
           POLIT, EPIST,
           # REPEATCSEC,
           VBACCHOICE, VBACINTEREST)) %>%
  colnames()

cont_3 <- LTM_final %>% 
  select(c(LABORLENGTH)) %>% colnames()

# # Chapter 4 ----
cat_4 <- LTM_final %>%
  select(c(DOULA, starts_with("DOULA2"), starts_with("VISITTOPIC"),
           FEED_CONCORDANT,
           PLANNEDFEEDC1, PLANNEDFEEDC2, FEED1WEEKC1,FEED1WEEKC2,
           starts_with("HOSPFEED"), starts_with("CURRENTFEED"),
           EXCLBFGOAL,BFGOAL, PHQ4_cat,PHQ2_cat,GAD2_cat,
           starts_with("PPBOTHER"),PPTHERAPY,starts_with("PPMED"),
           starts_with("PREPREG_MHCOND"),starts_with("SOCIALNEEDC"),
           SNMEAL, SNLIVE, SNUTILITIES, SNTRANSPORT,
           SNCHILDCARE, SNINCOME, SNDRUGS, SNUNSAFE, SNABUSE)) %>%
  select(-c(PPMEDSC4, PREPREG_MHCONDC5, ends_with("O"))) %>%
  colnames()

cont_4 <- LTM_final %>%
  select(c(PPVISITTIME1, PPVISITTIME2, PPVISIT, EXCLUSIVEBF)) %>% colnames()



