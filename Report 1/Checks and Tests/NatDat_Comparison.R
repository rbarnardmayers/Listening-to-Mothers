LTM %>% 
  group_by(BIRTHATTEND) %>%
  summarise(n = n()) %>% 
  mutate(p = n/3681) %>% 
  select(-c(n))

LTM %>% 
  group_by(FLAG1) %>% 
  summarise(n = n(), 
            OBGYN = sum(BIRTHATTEND == 1, na.rm = T),
            FM = sum(BIRTHATTEND == 2, na.rm = T),
            Doctor = sum(BIRTHATTEND == 3, na.rm = T),
            CNM = sum(BIRTHATTEND == 4, na.rm = T),
            Nurse = sum(BIRTHATTEND == 5, na.rm = T),
            PA = sum(BIRTHATTEND == 6, na.rm = T)) %>%
  subset(FLAG1 == 0) %>% t() %>% as.data.frame() %>% 
  mutate(V1 = V1/2527)

LTM %>% 
  subset(FLAG1 == 0) %>% 
  mutate(FLAG2 = case_when(FLAG2 < 3 ~ 0, TRUE ~ 1)) %>%
  group_by(FLAG2) %>% 
  summarise(n = n(), 
            OBGYN = sum(BIRTHATTEND == 1, na.rm = T),
            FM = sum(BIRTHATTEND == 2, na.rm = T),
            Doctor = sum(BIRTHATTEND == 3, na.rm = T),
            CNM = sum(BIRTHATTEND == 4, na.rm = T),
            Nurse = sum(BIRTHATTEND == 5, na.rm = T),
            PA = sum(BIRTHATTEND == 6, na.rm = T)) %>%
  subset(FLAG2 == 0) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = V1/2465)

LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3) %>% 
  mutate(FLAG3 = case_when(FLAG3 < 4 ~ 0, TRUE ~ 1)) %>%
  group_by(FLAG3) %>% 
  summarise(n = n(), 
            OBGYN = sum(BIRTHATTEND == 1, na.rm = T),
            FM = sum(BIRTHATTEND == 2, na.rm = T),
            Doctor = sum(BIRTHATTEND == 3, na.rm = T),
            CNM = sum(BIRTHATTEND == 4, na.rm = T),
            Nurse = sum(BIRTHATTEND == 5, na.rm = T),
            PA = sum(BIRTHATTEND == 6, na.rm = T)) %>%
  subset(FLAG3 == 0) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = V1/2406)

LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG3 < 4) %>% 
  mutate(FLAG4 = case_when(FLAG4 < 4 ~ 0, TRUE ~ 1)) %>%
  group_by(FLAG4) %>% 
  summarise(n = n(), 
            OBGYN = sum(BIRTHATTEND == 1, na.rm = T),
            FM = sum(BIRTHATTEND == 2, na.rm = T),
            Doctor = sum(BIRTHATTEND == 3, na.rm = T),
            CNM = sum(BIRTHATTEND == 4, na.rm = T),
            Nurse = sum(BIRTHATTEND == 5, na.rm = T),
            PA = sum(BIRTHATTEND == 6, na.rm = T)) %>%
  subset(FLAG4 == 0) %>%
  t() %>%
  as.data.frame() %>%
  mutate(V1 = V1/2357)


# INSURANCE ----
LTM %>% 
  group_by(INSURC7) %>%
  summarise(n = n()) %>% 
  mutate(p = n/3681) %>% 
  select(-c(n))




# Text responses ----

LTM %>% 
  select(c(F_WELL, WENTWELL, 
           F_DIDNT, DIDNTGOWELL, 
           F_ANYTHINGELSE, ANYTHINGELSE, DOULA3)) %>% 
  View()

# Nan data ----






