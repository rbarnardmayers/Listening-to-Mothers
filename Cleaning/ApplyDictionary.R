# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Helpful_Functions.R")
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers")
LTM2 <- read.csv("LTM_clean.csv")

# Data dictionary prep ----
# Getting rid of columns with no conversion in dict

dict3 <- dict3 %>% 
  mutate(KEEP = case_when(variable %in% colnames(LTM2) ~ 1, TRUE ~ 0)) %>% 
  subset(KEEP == 1) %>% 
  select(-c(KEEP))

LTM_include <- LTM2 %>% select(dict3$variable)

# 
LTM2 <- LTM2 %>% 
  select(c(setdiff(colnames(LTM2), colnames(LTM_include)), MDID))

# Apply dictionary recoding and labeling 
LTM_include <- recode_vrs(data = LTM_include, 
                          data_dictionary = data_dict, 
                          vrs = colnames(LTM_include))
# recode NA to missing
# LTM_include2 <- LTM_include %>% select(c(MDID, UID2, UID))
# LTM_include <- LTM_include %>% select(-c(MDID))
# LTM_include[is.na(LTM_include), select(-c('MDID', 'UID', 'UID2'))] <- "Missing"
# LTM_include <- LTM_include %>% replace(is.na(.), "Missing")

# Final dataset 
LTM_final <- LTM2 %>% 
  full_join(LTM_include, by = join_by(MDID))

# Create a weight variable while waiting for final version of dataset ----
# LTM_final$wght = rnorm(n=nrow(LTM_final), mean = 1, sd = .02)

# Create list of categorical variables by excluding numeric ----
categorical <- LTM_final %>%
  select(-c(HEIGHT,MDID, AGE, YEARBIRTH, WEIGHTGAIN_R,AGE_ATBIRTH,
            DOULA3, INDUCE6, REPEATCSEC,#GESTAGE, DUEDATE, BIRTHDATE,BIRTHDATE_Y,
            MEDINDUCE4, MEDINDUCE5, #WENTWELL, DIDNTGOWELL, AIAN,
            ANYTHINGELSE,GESTAGE_R_cont,starts_with("SUM_"),
            DISABILITYCOND, Source,ends_with("O"), YEARBIRTH,
            ends_with("BIRTHYEAR"), starts_with("SCREEN"), starts_with("TRAP"),
            CURRWEIGHT_LBS,PREGWEIGHT_KG_2,PREGWEIGHT_LBS_2,
            starts_with("FOLLOWUP"), UID2,RACE,
            starts_with("x"),contains("FLAG"), starts_with("F2_"),ends_with("O"),
            starts_with("F3_"),  starts_with("F1_"),starts_with("X"),
            PREG_INT,PREPREG_WEIGHT_B1,LEARNED2,
            NUMB_BIRTH,HEIGHT_FEET,HEIGHT_INCHES,CURRWEIGHT_KG,
            BIRTHWEIGHT_LBS,BIRTHWEIGHT_OZ,BIRTHWEIGHT_G,AGECHECK,
            DISABLEYRS,MODE_ALL,GESTAGE_WEEKS,GESTAGE,
            PREG_INT, HEIGHT_FEET,HEIGHT_INCHES, HEIGHT,
            PREPREG_WEIGHT_A1,PREGCONDITIONC11,DUEDATE_M, DUEDATE_Y,
            DUEDATE_D, BIRTHDATE_D, BIRTHDATE_M,LEARNED1,BIRTHWEIGHT_LBSOZ,
            VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, PREG_WEIGHT,
            PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF,BIRTHWEIGHT,
            HEIGHT, PREPREG_WEIGHT, RACEALONE, INSURCAT, ResLanguage, BMI, DOULA,
            DOULAC1, DOULAC2, DOULAC3, PRENAT,IMMIGRATION,
            WEAN, ZIP, FAMSIZE1, FAMSIZE2, INCOME, IMMIGRATION))  %>% colnames()

# Factor categorical variables and reorder values----
for(i in categorical){
  LTM_final[[i]] <- refac.fun(i)
}

# Convert continuous variables into numeric class ----
continuous <- LTM_final %>% 
  select(c(AGE,YEARBIRTH,GESTAGE,GESTAGE_WEEKS,starts_with("SUM_"),
           NUMB_BIRTH,BIRTHWEIGHT,PPVISIT,LEARNED2,AGE_ATBIRTH,
           DISABLEYRS,MODE_ALL,MEDINDUCE4,MEDINDUCE5,LEARNED1,GESTAGE_R_cont,
           PREG_WEIGHT, PREPREG_WEIGHT, HEIGHT, BMI,WEIGHTGAIN_R,
           VAGEXAM, LABORLENGTH, DAYSHOSP, BABYHOSP, PPVISIT, BIRTHWEIGHT_LBSOZ,
           PPVISITTIME1, PPVISITTIME2, EXCLUSIVEBF, WEAN, FAMSIZE1, FAMSIZE2, INCOME )) %>% 
  colnames()
# BABY_SURVEYAGE_MONTHS, GESTAGE
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

for(i in categorical){
    LTM_final[i] <- lapply(LTM_final[i], function(x)  gsub("I’d prefer not to answer", "Missing", x))  
}

for(i in categorical){
  LTM_final[i] <- lapply(LTM_final[i], function(x)  gsub("Missing", NA, x))  
}


LTM_dsn <- LTM_final %>% 
  mutate(FINALWT = as.numeric(FINALWT)) %>%
  as_survey_design(weight = FINALWT, id = 1)

