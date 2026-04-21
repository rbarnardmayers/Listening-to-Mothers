# CHECK FLAG CODING FROM MDR DATA DICTIONARY AND OURS
# Check gestational age calculations from DUEDATE and BIRTHDATE


LTM %>% 
  relocate(FLAG1, RFLAG1, F1_SPEED, R_F1_SPEED, TotalDurationSec,
           F1_USKG_ANY,R_F1_USKG_ANY, 
           DUP_FLAG, R_F1_DUP, F1_NOTRIBALAFF) %>% 
  # subset(FLAG1 == 0) %>% 
  View()

num1 %>% full_join(num2) %>% View()

# Full Dataset 
t_0 <- LTM %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  t() %>% as.data.frame()
rownames(t_0) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_0) <- c("Full Dataset")

# First Step 
t_1 <- LTM %>% 
  group_by(FLAG1) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG1 == 0) %>%
  t() %>% as.data.frame()
t_1 <- t_1[-1,] %>% as.data.frame()
rownames(t_1) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_1) <- c("Deletes removed")

# Second Step 
t_2 <- LTM %>% 
  subset(FLAG1 == 0) %>% 
  mutate(FLAG2 = case_when(FLAG2 < 3 ~ 0, 
                           FLAG2 >= 3 ~ 1)) %>%
  group_by(FLAG2) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG2 == 0) %>%
  t() %>% as.data.frame()
t_2 <- t_2[-1,] %>% as.data.frame()
rownames(t_2) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_2) <- c("Three Strikes and Out")

# Third Step (< 4)

t_3_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3) %>% 
  mutate(FLAG23 = case_when(FLAG23 < 4 ~ 0, 
                            FLAG23 >= 4 ~ 1)) %>%
  group_by(FLAG23) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG23 == 0) %>%
  t() %>% as.data.frame()
t_3_4 <- t_3_4[-1,] %>% as.data.frame()
rownames(t_3_4) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_3_4) <- c("Step 3a < 4 Total Flags")

# Third Step (< 5)
t_3_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3) %>% 
  mutate(FLAG23 = case_when(FLAG23 < 5 ~ 0, 
                            FLAG23 >= 5 ~ 1)) %>%
  group_by(FLAG23) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG23 == 0) %>%
  t() %>% as.data.frame()
t_3_5 <- t_3_5[-1,] %>% as.data.frame()
rownames(t_3_5) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_3_5) <- c("Step 3b < 5 Total Flags")

# Fourth Step (< 4 & < 4)
t_4_4_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 4 ~ 0, 
                             FLAG234 >= 4 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_4 <- t_4_4_4[-1,] %>% as.data.frame()
rownames(t_4_4_4) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_4_4_4) <- c("Step 4aa < 4 Total Flags")

# Fourth Step (< 4 & < 5)
t_4_4_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 5 ~ 0, 
                             FLAG234 >= 5 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_5 <- t_4_4_5[-1,] %>% as.data.frame()
rownames(t_4_4_5) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_4_4_5) <- c("Step 4ab < 5 Total Flags")

# Fourth Step (< 4 & < 6)
t_4_4_6 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 4) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 6 ~ 0, 
                             FLAG234 >= 6 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_4_6 <- t_4_4_6[-1,] %>% as.data.frame()
rownames(t_4_4_6) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_4_4_6) <- c("Step 4ac < 6 Total Flags")

# Fourth Step (< 5 & < 4)
t_4_5_4 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 4 ~ 0, 
                             FLAG234 >= 4 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_4 <- t_4_5_4[-1,] %>% as.data.frame()
rownames(t_4_5_4) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_4_5_4) <- c("Step 4ba < 4 Total Flags")

# Fourth Step (< 5 & < 5)
t_4_5_5 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 5 ~ 0, 
                             FLAG234 >= 5 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_5 <- t_4_5_5[-1,] %>% as.data.frame()
rownames(t_4_5_5) <-c("n", paste0("Source_", seq(1:20)))
colnames(t_4_5_5) <- c("Step 4bb < 5 Total Flags")

# Fourth Step (< 5 & < 6)
t_4_5_6 <- LTM %>% 
  subset(FLAG1 == 0 & FLAG2 < 3 & FLAG23 < 5) %>% 
  mutate(FLAG234 = case_when(FLAG234 < 6 ~ 0, 
                             FLAG234 >= 6 ~ 1)) %>%
  group_by(FLAG234) %>% 
  summarise(n = n(), 
            Source_1 = sum(Source == 1), 
            Source_2 = sum(Source == 2),
            Source_3 = sum(Source == 3),
            Source_4 = sum(Source == 4),
            Source_5 = sum(Source == 5),
            Source_6 = sum(Source == 6),
            Source_7 = sum(Source == 7),
            Source_8 = sum(Source == 8),
            Source_9 = sum(Source == 9),
            Source_10 = sum(Source == 10),
            Source_11 = sum(Source == 11),
            Source_12 = sum(Source == 12),
            Source_13 = sum(Source == 13),
            Source_14 = sum(Source == 14),
            Source_15 = sum(Source == 15),
            Source_16 = sum(Source == 16),
            Source_17 = sum(Source == 17),
            Source_18 = sum(Source == 18),
            Source_19 = sum(Source == 19),
            Source_20 = sum(Source == 20)) %>% 
  subset(FLAG234 == 0) %>%
  t() %>% as.data.frame()
t_4_5_6 <- t_4_5_6[-1,] %>% as.data.frame()
rownames(t_4_5_6) <- c("n", paste0("Source_", seq(1:20)))
colnames(t_4_5_6) <- c("Step 4bc < 6 Total Flags")

# MERGING TOGETHER ----
all_results <- cbind(t_0, t_1, t_2, t_3_5,t_3_4,
                     t_4_5_6,t_4_4_6,t_4_5_5, 
                     t_4_4_5, t_4_5_4, t_4_4_4)
all_results$Source <- rownames(all_results)
all_results <- all_results %>% relocate(Source)


LTM %>% 
  summarise(F1_USKG_ANY = sum(F1_USKG_ANY == 1, na.rm = T), 
            F1_SPEED = sum(F1_SPEED == 1, na.rm = T),
            F1_DUP = sum(F1_DUP == 1, na.rm = T),
            
            F2_PNCB4LEARN= sum(F2_PNCB4LEARN == 1, na.rm = T),
            F2_WT_EXT= sum(F2_WT_EXT == 1, na.rm = T),
            F2_HT_EXT= sum(F2_HT_EXT == 1, na.rm = T),
            F2_GA_EXT= sum(F2_GA_EXT == 1, na.rm = T),
            F2_BW_EXT= sum(F2_BW_EXT == 1, na.rm = T),
            F2_VLBNICU= sum(F2_VLBNICU == 1, na.rm = T),
            F2_VLBHOSP= sum(F2_VLBHOSP == 1, na.rm = T),
            F2_MCAID_INC_EXT= sum(F2_MCAID_INC_EXT == 1, na.rm = T),
            F2_ZIPCHECK= sum(F2_ZIPCHECK == 1, na.rm = T),
            
            F3_EARLYPNC= sum(F3_EARLYPNC == 1, na.rm = T), 
            F3_PREGWT= sum(F3_PREGWT == 1, na.rm = T), 
            F3_WTDIFF= sum(F3_WTDIFF == 1, na.rm = T),
            F3_HT= sum(F3_HT == 1, na.rm = T),
            F3_LBW_NICU= sum(F3_LBW_NICU == 1, na.rm = T),
            F3_LBW_DAYS= sum(F3_LBW_DAYS == 1, na.rm = T),
            F3_VAGASSIST= sum(F3_VAGASSIST == 1, na.rm = T),
            F3_GAFOOD= sum(F3_GAFOOD == 1, na.rm = T),
            F3_INC_MCAID= sum(F3_INC_MCAID == 1, na.rm = T),
            F3_INC_PAN= sum(F3_INC_PAN == 1, na.rm = T),
            F3_INC_NEEDS= sum(F3_INC_NEEDS == 1, na.rm = T),
            
            F4_PRE_HYPER= sum(F4_PRE_HYPER == 1, na.rm = T),
            F4_PRE_DIABETES= sum(F4_PRE_DIABETES == 1, na.rm = T),
            F4_NO_PNC= sum(F4_NO_PNC == 1, na.rm = T),
            F4_CS_LABOR= sum(F4_CS_LABOR == 1, na.rm = T),
            F4_VAGEXAM= sum(F4_VAGEXAM == 1, na.rm = T),
            F4_TERM_NICU= sum(F4_TERM_NICU == 1, na.rm = T),
            F4_PPTVISITS= sum(F4_PPTVISITS == 1, na.rm = T),
            F4_MISSING_BW= sum(F4_MISSING_BW == 1, na.rm = T),
            F4_MISSING_LABORHRS= sum(F4_MISSING_LABORHRS == 1, na.rm = T),
            F4_MISSING_MOMDAYS= sum(F4_MISSING_MOMDAYS == 1, na.rm = T),
            F4_MISSING_BABYDAYS= sum(F4_MISSING_BABYDAYS == 1, na.rm = T)) %>%
  t() %>% as.data.frame() %>%
  mutate(prop = as.numeric(V1)/3681) %>%
  View()

LTM %>% 
  group_by(CARESETTINGC3) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n))


LTM %>% 
  mutate(CURREDUC = as.numeric(CURREDUC)) %>%
  group_by(CURREDUC) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n))


conds <- LTM %>% select(starts_with("PREGCONDITION")) %>% colnames()
conds <- LTM %>% select(starts_with("MEDINDUCE1")) %>% colnames()

for (i in conds){
  t <- LTM %>% 
    mutate(var = as.numeric(get(i))) %>%
    subset(!is.na(var)) %>%
     group_by(var) %>% 
    summarise(n = n()) %>% 
    mutate(prop = round(n / 3681, 3))
  print(i)
  print(t)
}


LTM %>% 
  mutate(FINAL = case_when(FLAG1 > 0 ~ 0, 
                           FLAG1 == 0 & FLAG2 > 2 ~ 0, 
                           FLAG1 == 0 & FLAG2 <= 3 & FLAG234 > 5 ~ 0, 
                           TRUE ~ 1)) %>% 
  select(c( FINAL, EXCL_REASON, ANYTHINGELSE, ends_with("O"), DOULA3)) %>%
  # select(c(FINAL, starts_with("DOULA3"))) %>%
  View()


       
       
