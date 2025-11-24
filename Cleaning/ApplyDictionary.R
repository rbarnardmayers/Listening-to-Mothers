# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers")
LTM2 <- read.csv("LTM_clean.csv")

# Data dictionary prep ----
# Getting rid of columns with no conversion in dict

LTM_include <- LTM2 %>% 
  select(-c(NUMB_BIRTH, HEIGHT_FEET, HEIGHT_INCHES, LEARNED2,LEARNED1,
            DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,  VAGEXAM, LABORLENGTH, DAYSHOSP,
            BABYHOSP, PPVISIT, PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,PARITY, 
            HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, DOULA, 
            DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT, WEAN, ZIP, FAMSIZE1, 
            FAMSIZE2, INCOME, IMMIGRATION,
            DOULA3, INDUCE6, REPEATCSEC,
            MEDINDUCE4, MEDINDUCE5, WENTWELL, DIDNTGOWELL, ANYTHINGELSE, AIAN,
            DISABILITYCOND, Source,ends_with("O"),
            FEED_CONCORDANT, phys_cb, SDM_1, SDM_2, SDM_3, SDM_4, WEIGHTGAIN, 
            BIRTHWEIGHT_CAT, BIRTHWEIGHT,CONFIDENCE_ANY, ATHOMECARE_ANY, 
            MODE_ALL, SUM_RESPECT, SDM, VBAC, MODE_ALL))

# 
LTM2 <- LTM2 %>% 
  select(c(NUMB_BIRTH, HEIGHT_FEET, HEIGHT_INCHES, LEARNED2,LEARNED1,
           DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,  VAGEXAM, LABORLENGTH, DAYSHOSP,
           BABYHOSP, PPVISIT, PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,
           PARITY, HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, 
           DOULA, DOULAC1, DOULAC2, DOULAC3, LEARNED1, PRENAT,
           WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION, UID2, 
           DOULA3, INDUCE6, REPEATCSEC,
           MEDINDUCE4, MEDINDUCE5, WENTWELL, DIDNTGOWELL, ANYTHINGELSE, AIAN,
           DISABILITYCOND, Source,ends_with("O"),
           FEED_CONCORDANT, phys_cb, SDM_1, SDM_2, SDM_3, SDM_4, WEIGHTGAIN, 
           BIRTHWEIGHT_CAT, BIRTHWEIGHT,CONFIDENCE_ANY, ATHOMECARE_ANY, 
           MODE_ALL, SUM_RESPECT, SDM, VBAC, MODE_ALL))

# Apply dictionary recoding and labeling 
LTM_include <- recode_vrs(data = LTM_include, data_dictionary = data_dict, 
                          vrs = colnames(LTM_include))
# recode NA to missing
# LTM_include2 <- LTM_include %>% select(c(MDID, UID2, UID))
# LTM_include <- LTM_include %>% select(-c(MDID))
# LTM_include[is.na(LTM_include), select(-c('MDID', 'UID', 'UID2'))] <- "Missing"
# LTM_include <- LTM_include %>% replace(is.na(.), "Missing")

# Final dataset 
LTM_final <- LTM2 %>% 
  full_join(LTM_include)

# Create a weight variable while waiting for final version of dataset ----
# LTM_final$wght = rnorm(n=nrow(LTM_final), mean = 1, sd = .02)
LTM_final$wght = 1

# Create list of categorical variables by excluding numeric ----
categorical <- LTM_final %>%
  select(-c(HEIGHT,CaseId,MDID,ST, INTRO, AGE,TIMES,YEARBIRTH, 
            DOULA3, INDUCE6, REPEATCSEC,
            MEDINDUCE4, MEDINDUCE5, WENTWELL, DIDNTGOWELL, ANYTHINGELSE, AIAN,
            DISABILITYCOND, Source,ends_with("O"),
            BW_LBSANDOZ,SURVEYYEAR,SURVEYMONTHS,SURVEYDAYS,
            SURVEYDATE,
            ends_with("BIRTHYEAR"), starts_with("SCREEN"), starts_with("TRAP"),
            CURRWEIGHT_LBS,PREGWEIGHT_B1,PREGWEIGHT_A1,
            starts_with("FOLLOWUP"),
            starts_with("x"),contains("FLAG"), starts_with("F2_"),ends_with("O"),
            starts_with("F3_"),  starts_with("F1_"),starts_with("X"),
            NUMB_BIRTH_OLD, PREG_INT,PREPREG_WEIGHT_B1,LEARNED2,
            TRAP_AREA_SELECTOR,NUMB_BIRTH,HEIGHT_FEET,HEIGHT_INCHES,CURRWEIGHT_KG,
            LABORPERMIT_D1,LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,
            BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGEBIRTH,AGECHECK,
            DISABLEYRS,DKREF_COUNT,DKREF_PERCENT,MODE_ALL,
            INTERNATIONAL_IP,AGEDIFF, ATTENTION_CHECK,BIRTHDATE,wght,
            DUEDATE,DAYSDIFFERENCE,GESTAGE,GESTAGE_WEEKS,LABORPERMIT_D1,
            LABORPERMIT_D2,LABORPERMIT_E1,LABORPERMIT_E2,PREG_INT,
            GESTAGE_DAYS,HEIGHTFT_IN,COMBINEDHEIGHT,PREGWEIGHTCHECK, 
            PREPREG_WEIGHT_A1,PREGCONDITIONC11, PREPREGWEIGHTCHECK,
            DUEDATE_D, DUEDATE_Y, BIRTHDATE_D,WEIGHTGAIN,LEARNED1,
            VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, PREG_WEIGHT,
            PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,ZIPSTATE,BIRTHWEIGHT,
            HEIGHT, PREPREG_WEIGHT, RACE, INSURANCE, LANGUAGE, BMI, DOULA,
            DOULAC1, DOULAC2, DOULAC3, PRENAT, PRENAT,IMMIGRATION,
            WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION, "RANDOM",
            "SURMODE","META_USERAGENT"  ,"META_REFERER" , "META_IPADDRESS" ,
            "META_BROWSER_LANGUAGE","META_JAVASUPPORT","META_OPERATINGSYSTEM", UID))  %>% 
  select(-c(UID2)) %>%
  colnames()

# Factor categorical variables and reorder values----
for(i in categorical){
  LTM_final[[i]] <- refac.fun(i)
}

# Convert continuous variables into numeric class ----
continuous <- LTM_final %>%  
  select(c(AGE,YEARBIRTH,BABY_SURVEYAGE_MONTHS,
           NUMB_BIRTH,BIRTHWEIGHT,AGEBIRTH,PPVISIT,LEARNED2,
           DISABLEYRS,MODE_ALL,MEDINDUCE4,MEDINDUCE5,LEARNED1,
           PREG_WEIGHT, PREPREG_WEIGHT, WEIGHTGAIN, HEIGHT, BMI,
           GESTAGE, VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, 
           PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF, WEAN, FAMSIZE1, FAMSIZE2, INCOME )) %>% 
  colnames()

for(i in continuous){
  LTM_final[[i]] <- as.numeric(LTM_final[[i]])
}

# rm(LTM_include)
# rm(LTM2)
gc()

for (i in seq_len(nrow(dict2))) {
  var_name <- dict2$variable[i]
  var_label <- dict2$variable_label[i]
  
  if (var_name %in% names(LTM_final)) {
    var_label(LTM_final[[var_name]]) <- var_label
  }
}

# LTM_final <- lapply(LTM_final, function(x)  gsub("I’d prefer not to answer", "Missing", x))

LTM_dsn <- LTM_final %>% 
  as_survey_design(weight = wght, id = 1)

