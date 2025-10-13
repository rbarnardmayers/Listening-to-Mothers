
cat_1 <- LTM_final %>% select(c(starts_with("DOULA1"))) %>% 
  select(-c(DOULA1C6O, DOULA1C6, DOULA1C7)) %>%
  colnames()

cols = bases %>% subset(!is.na(Base)) %>% pull(variable)

for(i in c("DOULA1C1", "DOULA1C2", "DOULA1C3", "DOULA1C4", 
           "DOULA1C5")){
  lab = dict2[dict2$variable == i,]$variable_label
  if(i %in% cols){
    k <- print.cat.from.bases(i)} else {
      k <- print.cat(i)
    } 
  
  print(k) 
  
}


for(i in c("DOULA1C1", "DOULA1C2", "DOULA1C3", "DOULA1C4", 
           "DOULA1C5")){ 
  print(print.cat.from.bases(i))
  }

# Figure out concordance bewteen PLANNEDFEED and FEEDWEEK1
# PLANNEDFEEDC1 - breast milk
# PLANNEDFEEDC2 - formula 
# FEED1WEEKC1 - breast mlik
# FEED1WEEKC2 - formula

mutate(FEED_CONCORDANT = case_when(PLANNEDFEEDC1 == 1 & FEED1WEEKC1 == 1 ~ 1, 
                                   PLANNEDFEEDC2 == 1 & FEED1WEEKC2 == 1 ~ 1, 
                                   PLANNEDFEEDC1 == 1 &  FEED1WEEKC2 == 1 ~ 0,
                                   PLANNEDFEEDC2 == 1 &  FEED1WEEKC1 == 1 ~ 0))

# VBAC
mutate(MODE = case_when(MODE == 1 ~ 0, 
                        MODE == 2 ~ 1),
       MODE1 = case_when(MODE1 == 1 ~ 0, 
                         MODE1 == 2 ~ 1),
       MODE2 = case_when(MODE2 == 1 ~ 0, 
                         MODE2 == 2 ~ 1),
       MODE3 = case_when(MODE3 == 1 ~ 0, 
                         MODE3 == 2 ~ 1),
       MODE4 = case_when(MODE4 == 1 ~ 0, 
                         MODE4 == 2 ~ 1),
       MODE5 = case_when(MODE5 == 1 ~ 0, 
                         MODE5 == 2 ~ 1),
       MODE6 = case_when(MODE6 == 1 ~ 0, 
                         MODE6 == 2 ~ 1),
       MODE7 = case_when(MODE7 == 1 ~ 0, 
                         MODE7 == 2 ~ 1),
       MODE8 = case_when(MODE8 == 1 ~ 0, 
                         MODE8 == 2 ~ 1),
       MODE9 = case_when(MODE9 == 1 ~ 0, 
                         MODE9 == 2 ~ 1),
       MODE10 = case_when(MODE10 == 1 ~ 0, 
                          MODE10 == 2 ~ 1),
       MODE11 = case_when(MODE11 == 1 ~ 0, 
                          MODE11 == 2 ~ 1),
       MODE12 = case_when(MODE12 == 1 ~ 0, 
                          MODE12 == 2 ~ 1),
       MODE13 = case_when(MODE13 == 1 ~ 0, 
                          MODE13 == 2 ~ 1),
       MODE14 = case_when(MODE14 == 1 ~ 0, 
                          MODE14 == 2 ~ 1),
       MODE15 = case_when(MODE15 == 1 ~ 0, 
                          MODE15 == 2 ~ 1)) %>% 
  rename(MDE2023 = MODE2023)
rowwise() %>% 
  mutate(MODE_ALL = sum(across(starts_with("MODE")), na.rm = T)) 

mutate(VBAC = case_when(MODE_ALL > 0 & MDE2023 == 1 ~ 1,
                        MODE_ALL > 0 & MDE2023 == 2 ~ 0,
                        MODE_ALL == 0 ~ 0))

# Respect care: developer’s 0-100 scoring guidance
rowwise() %>% 
  mutate(SUM_RESPECT = sum(RESPECT, KNOWLEDGE, HEARD, 
                           DECISIONS, CONSENT, INFORMED,
                           TIMELINESS, TRUST, FEEDING,
                           SAFE, DISCRIMINATION, NEGLECT, na.rm = T))

# CUSTOMS 

# childbirth interventions
# sweeping or stripping of membranes, MEDINDUCE1C2 == 1 | MEDINDUCE1C3 == 1
# artificial rupture of membranes, MEDINDUCE1C1 == 1
# synthetic oxytocin LABORINTC3 == 1
# epidural analgesia for pain, PAINMEDSC1 == 1
# narcotics for pain, PAINMEDSC3 == 1
# vacuum or forceps VAGASSIST == 1 | VAGASSIST == 2
# cesarean birth - MODE2023 == 2

# Physiologic childbirth
# Opiates or nitrous oxide - PAINMEDSC3 & PAINMEDSC2 & PAINMEDSC1
# Augmentation of labor - MEDINDUCE -- i think? 
# Regional anesthesia analgesia except for the purpose of 
##  spontaneous laceration repair - PAINMEDSC4 & PAINMEDSC5
# Artificial rupture of membranes - MEDINDUCE
# Episiotomy -- EPIST  

mutate(phys_cb = case_when(PAINMEDSC1 == 1 ~ 1, 
                           PAINMEDSC2 == 1 ~ 1, 
                           PAINMEDSC3 == 1 ~ 1, 
                           PAINMEDSC4 == 1 ~ 1, 
                           PAINMEDSC5 == 1 ~ 1, 
                           EPIST == 1 ~ 1, 
                           TRUE ~ 0))

# SDM induction
# 1	Yes, my provider thought my labor should be induced
# 2	Yes, my provider thought my labor should not be induced
# 3	No, my provider did not make a recommendation
# 99	I’d prefer not to answer

table(LTM$INDUCE1)
mutate(SDM_1 = case_when(INDUCE1 == 1 ~ 1, 
                         INDUCE1 == 2 ~ 1, 
                         INDUCE1 == 3 ~ 0, 
                         INDUCE1 == 4 ~ 0),
       SDM_2 = case_when(INDUCE2 == 1 ~ 1, 
                         INDUCE2 == 2 ~ 1, 
                         INDUCE2 == 3 ~ 0, 
                         INDUCE2 == 4 ~ 0),
       SDM_3 = case_when(INDUCE3 == 1 ~ 1, 
                         INDUCE3 == 2 ~ 0),
       SDM_4 = case_when(INDUCE4 == 1 ~ 1, 
                         INDUCE4 == 2 ~ 0)) %>% 
  rowwise() %>%
  mutate(SDM = sum(SDM_1,SDM_2,SDM_3,SDM_4, na.rm = T)) 

mutate(SDM = case_when(is.na(SDM_1)~NA,
                       is.na(SDM_2)~NA,
                       is.na(SDM_3)~NA,
                       is.na(SDM_4)~NA, 
                       TRUE ~ SDM))


# PREGWEIGHT - PREPREGWEIGHT
mutate(PREG_WEIGHT - PREPREG_WEIGHT)

# BIRTHWEIGHT
# VLBW, LBW, normal BW, macrosomic
mutate(BIRTHWEIGHT_CAT = case_when(BIRTHWEIGHT < 1500 ~ "VLBW",
                                   BIRTHWEIGHT < 2500 ~ "LBW",
                                   BIRTHWEIGHT > 4,000 ~ "LBW",
                                   is.na(BRITHWEIGHT) ~ NA,
                                   TRUE ~ "Normal BW"))

# RURALURBAN

# BPCONFID+URINECONFID+WEIGHCONFID+BABYHRCONFID

mutate(BPCONFID = case_when(BPCONFID == 99 ~ NA), 
       URINECONFID = case_when(URINECONFID == 99 ~ NA),
       WEIGHCONFID = case_when(WEIGHCONFID == 99 ~ NA),
       BABYHRCONFID = case_when(BABYHRCONFID == 99 ~ NA), 
       
       CONFIDENCE_ANY = case_when(BPCONFID < 3  | URINECONFID < 3 | 
                                    WEIGHCONFID < 3 | BABYHRCONFID < 3 ~ "Yes",
                                  TRUE ~ "No"))

# EQUIPMENT / SUPPLIES
ATHOMECARE_ANY = case_when( ATHOMECAREC1 == 1 ~ 1, 
                            ATHOMECAREC2 == 1 ~ 1, 
                            ATHOMECAREC3 == 1 ~ 1,
                            ATHOMECAREC4 == 1 ~ 1, 
                            TRUE ~ 0)
