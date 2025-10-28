# Checking odd responses 
LTM_check <- LTM %>% 
  subset(Source == 12 & FINAL_DETERMINATION == "Keep")%>% 
  mutate(RACE = case_when(RACEC1 == 1 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NHW", 
                          RACEC1 == 0 & RACEC2 == 1 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "Hispanic", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 1 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Black or African American", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 1 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 0 ~ "NH Asian", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 1 & RACEC6 == 0 & RACEC7 == 0 ~ "AIAN", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 1 & RACEC7 == 0 ~ "MENA", 
                          RACEC1 == 0 & RACEC2 == 0 & RACEC3 == 0 & RACEC4 == 0 & RACEC5 == 0 & RACEC6 == 0 & RACEC7 == 1 ~ "NHPI", 
                          RACEC1 == 1 | RACEC2 == 1 | RACEC3 == 1 | RACEC4 == 1 | RACEC5 == 1 | RACEC6 == 1 | RACEC7 == 1 ~ "Multiracial", 
                          RACEC8 == 1 ~ "Prefer not to answer", 
                          TRUE ~ "Missing"),
         PARITY = case_when(NUMB_BIRTH == 1 ~ "Nulliparous", 
                            NUMB_BIRTH > 1 ~ "Multiparous", 
                            TRUE ~ "Missing"))

# Race
prop.table(table(LTM_check$RACE))

# UIDS  ---
#3002673 and 3000880 wrote same thing in ANYTHINGELSE

# Checking against flag items ----
# Cesarean
prop.table(table(LTM_check$MODE2023))

# Married
prop.table(table(LTM_check$RELATIONSHIP))

# 1st birth
prop.table(table(LTM_check$PARITY))

# Epidurals
prop.table(table(LTM_check$PAINMEDSC1))

# 7+ months solid food
prop.table(table(LTM_check$F3_GAFOOD))

# Vaginal Assist
prop.table(table(LTM_check$F3_VAGASSIST))

# 1st Prenatal in 1st month
prop.table(table(LTM_check$F3_EARLYPNC))

# Prepreg HT
prop.table(table(LTM_check$F3_PRE_HYPER))

# Prepreg diabetes
prop.table(table(LTM_check$F3_PRE_DIABETES))

# No Prenatal Care
prop.table(table(LTM_check$F3_NO_PNC))




