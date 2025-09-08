# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Data Cleaning.R")

LTM_include <- LTM2 %>% select(c(all_of(include)), UID2)
LTM_include <- recode_vrs(data = LTM_include, data_dictionary = data_dict, 
                          vrs = include)

LTM_include[is.na(LTM_include)] <- "Missing"

LTM_final <- LTM2 %>% 
  select(all_of(exclude)) %>% 
  full_join(LTM_include)

# Refactor ----
col_list <- include
for(i in col_list){
  
  LTM_final[[i]] <- refac.fun(i)
  
}

LTM_final <- LTM_final %>% 
  mutate(wght = rnorm(1613, mean = 1, sd = .02))


# LTM_final$LEARNED1 <- refac.num('LEARNED1')
# LTM_final$LEARNED2 <- refac.num("LEARNED2", 
#                                 val = c("I did not have any prenatal visits", 
#                                         "I’d prefer not to answer",
#                                         "Missing"))
# 
# 
nums <- LTM_final %>%
  select(c(ends_with("BIRTHYEAR"), starts_with("SCREEN"),
           starts_with("PREPREG_PHYSCOND"), starts_with("TRAP"),
           starts_with("PREPREG_MHCOND"),starts_with("x"),
           contains("FLAG"), 
           starts_with("F2_"),  starts_with("F3_"),  starts_with("F1_"),
           starts_with("X"),
           starts_with("NOPRENATALC"))) %>% colnames()

include <- LTM_final %>%
  select(-c(HEIGHT,CaseId,MDID,ST, INTRO, AGE,SCREEN,TIMES,YEARBIRTH, 
            all_of(ignores),BW_LBSANDOZ,SURVEYYEAR,SURVEYMONTHS,SURVEYDAYS,
            SURVEYDATE,BABY_SURVEYAGE_MONTHS,FINAL_QC,FINAL_DETERMINATION,
            all_of(nums), AGE_FLAG,CURRWEIGHT_LBS,PREGWEIGHT_B1,PREGWEIGHT_A1,
            SCREEN3,NUMB_BIRTH_OLD, PREG_INT,PREPREG_WEIGHT_B1,LEARNED2,
            TRAP_AREA_SELECTOR,NUMB_BIRTH,HEIGHT_FEET,HEIGHT_INCHES,CURRWEIGHT_KG,
            LABORPERMIT_D1,LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,
            BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGEBIRTH,AGECHECK,
            DISABLEYRS,SPANISHFLAG,DKREF_COUNT,DKREF_PERCENT,K,
            INTERNATIONAL_IP,AGEDIFF, ATTENTION_CHECK,BIRTHDATE,wght,
            DUEDATE,DAYSDIFFERENCE,GESTAGE,GESTAGE_WEEKS,
            GESTAGE_DAYS,HEIGHTFT_IN,COMBINEDHEIGHT,PREGWEIGHTCHECK, 
            PREPREG_WEIGHT_A1,FIRSTVISIT,PREGCONDITIONC11, 
            DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,  
            VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, 
            PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,
            HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, DOULA,
            DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT, PRENAT,
            WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION)) %>%
  select(-c(UID2)) %>%
  colnames()

continuous <- LTM_final %>%  
  select(c(HEIGHT, AGE,YEARBIRTH,
           BABY_SURVEYAGE_MONTHS,PREG_INT,NUMB_BIRTH,LABORPERMIT_D1,
           LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,
           BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGEBIRTH,
           DISABLEYRS,AGEDIFF,
           DAYSDIFFERENCE,GESTAGE,
           FIRSTVISIT, VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, 
           PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,
           HEIGHT, PREPREG_WEIGHT, BMI,
           LEARNED1, WEAN, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION)) %>% 
  colnames()


for(i in continuous){
  
  LTM_final[[i]] <- as.numeric(LTM_final[[i]])
  
}


# for(i in c('AGE', 'SCREEN', 'YEARBIRTH','BABY_SURVEYAGE_MONTHS','PREG_INT','LABORPERMIT_D1',
#   'LABORPERMIT_D2','LABORPERMIT_E1','LABORPERMIT_E2','BIRTHWEIGHT_LBS','BIRTHWEIGHT_OZ',
#   'BIRTHWEIGHT_G','AGEBIRTH','DISABLEYRS','AGEDIFF','DAYSDIFFERENCE','GESTAGE',
#   'FIRSTVISIT')){
#   
#   LTM_final[[i]] <- as.numeric(LTM_final[[i]])
#   
# }
# 



