source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Helpful_Functions.R")

# Data Read in ----
LTM <- read.csv("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Data_9.4.25.csv")
LTM <- LTM %>% subset(FINAL_DETERMINATION == "Keep")

# Get rid of identifying information 

LTM1 <- LTM[,c(1,2,46,47,55,58,74:730)] %>% 
  select(-c(CHILDNAME, FINAL_DETERMINATION, FINAL_QC, contains("FLAG"),
            starts_with("F1"), starts_with("F2"), starts_with("F3"), 
            starts_with("x"), starts_with("X")))

# Identify text response columns ending for other ----
ends_o <- LTM1[str_ends(colnames(LTM1), "O")] %>% 
  select(-c(INTRO)) %>% colnames()

# Create dataset of just text responses and UID2 ----
LTM_ignore <- LTM1 %>% 
  select(c(all_of(ends_o), UID2, DOULA3, INDUCE6, REPEATCSEC,
           MEDINDUCE4, MEDINDUCE5, WENTWELL, DIDNTGOWELL, ANYTHINGELSE, AIAN,
           DISABILITYCOND, Source)) 

ignores <- LTM_ignore %>% 
  select(-c(UID2)) %>% 
  colnames() 

# Create dataset of the rest of the columns ----
LTM_keep <- LTM1 %>% 
  select(-c(all_of(ignores)), MDID, ST)

# Create list of column names to conver to numeric
tochange <- LTM_keep %>% 
  select(-c(UID2)) %>% 
  colnames()

# Convert to numeric
for(i in tochange){
  LTM_keep[[i]] <- as.numeric(LTM_keep[[i]])
}
 
# for(i in colnames(LTM_ignore)){
#   if(i %in% colnames(LTM_keep)){
#     print(i)
#   } 
# }

LTM1 <- full_join(LTM_ignore, LTM_keep)

# test2 <- LTM_keep[131,] %>% select(c(UID2))
# test1 <- LTM_ignore[1,] %>% select(c(UID2))
# 
# LTM_ignore[LTM_ignore$UID2 == test1$UID2, ] %>% 
#   relocate(UID2) %>% 
#   View()
# 
# LTM_keep[LTM_keep$UID2 == test2$UID2, ] %>% 
#   relocate(UID2) %>% 
#   View()

rm(LTM_ignore)
rm(LTM_keep)

# Recode variables ----

LTM2 <- LTM1 %>% 
  mutate(PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous", 
                            NUMB_BIRTH > 1 ~ "Multiparous", 
                            TRUE ~ "Missing"),
         HEIGHT = (HEIGHT_FEET*12) + HEIGHT_INCHES, 
         PREPREG_WEIGHT = case_when(is.na(PREPREG_WEIGHT_A1) ~ 2.20462 * PREPREG_WEIGHT_B1, 
                                    !is.na(PREPREG_WEIGHT_A1) ~ PREPREG_WEIGHT_A1), 
         PREG_WEIGHT = case_when(is.na(PREGWEIGHT_A1) ~ 2.20462 * PREGWEIGHT_B1, 
                                 !is.na(PREGWEIGHT_A1) ~ PREGWEIGHT_A1), 
         RACE = case_when(RACEC2 == 1 ~ "Hispanic", 
                          RACEC1 == 1 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NHW", 
                          RACEC1 == 0 & RACEC3 == 1 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Black or African American", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 1 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Asian", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 1 & RACEC6 == 0 & RACEC7 == 0 ~ "AIAN", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 1 & RACEC7 == 0 ~ "MENA", 
                          RACEC1 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 1 ~ "NHPI", 
                          RACEC1 == 1 | RACEC3 == 1 | RACEC4 == 1 | RACEC5 == 1 | RACEC6 == 1 | RACEC7 == 1 ~ "Multiracial", 
                          RACEC8 == 1 ~ "Missing", 
                          TRUE ~ "Missing"),
         INSURANCE = case_when(INSURC1 == 1 ~ "Private",
                               INSURC2 == 1 ~ "Medicaid/CHIP",
                               INSURC3 == 1 ~ "TRICARE or other military health care",
                               INSURC4 == 1 ~ "Indian Health Service or tribal",
                               INSURC5 == 1 ~ "Other",
                               INSURC6 == 1 ~ "None",
                               INSURC7 == 1 ~ "Missing",
                               TRUE ~ "Missing"),
         LANGUAGE = case_when(LANGHOMEC1 == 1 ~ "English",
                              LANGHOMEC1 == 0 ~ "Other",
                              TRUE ~ "Missing"),
         BMI = PREPREG_WEIGHT *703 / HEIGHT^2, 
         DOULA = case_when(DOULAC1 == 1 ~ "Yes", 
                           DOULAC2 == 1 ~ "Yes", 
                           DOULAC3 == 1 ~ "Yes",
                           DOULAC4 == 1 ~ "No", 
                           DOULAC5 == 1 ~ "Missing", 
                           TRUE ~ "Missing"), 
         DOULAC1 = case_when(DOULA == "Yes" & DOULAC1 == 1 ~ "During Pregnancy", 
                             DOULA == "Yes" & DOULAC1 == 0 ~ "Not During Pregnancy", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC2 = case_when(DOULA == "Yes" & DOULAC2 == 1 ~ "During Birth", 
                             DOULA == "Yes" & DOULAC2 == 0 ~ "Not During Birth", 
                             DOULA == "No" | DOULA == "Missing" ~ NA),
         DOULAC3 = case_when(DOULA == "Yes" & DOULAC3 == 1 ~ "Postpartum", 
                             DOULA == "Yes" & DOULAC3 == 0 ~ "Not Postpartum", 
                             DOULA == "No" | DOULA == "Missing" ~ NA), 
         LEARNED1 = case_when(LEARNED1 == 99 ~ "Missing", 
                              is.na(LEARNED1) ~ "Missing",
                              TRUE ~ as.character(LEARNED1)), 
         PRENAT = case_when(LEARNED2 == 1 ~ "No Prenatal Care", 
                            LEARNED2 == 99 ~ "Missing", 
                            is.na(LEARNED2) ~ "Missing",
                            TRUE ~ "Had Prenatal Care"))


rm(LTM1)
# Midwife, physician, other for provider

