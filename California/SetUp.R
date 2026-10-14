source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")
setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers/California")

CA_Q1 <- read_excel("LTM_CA_Q1.xlsx")
CA_Q1Q2 <- read_excel("LTM_CA_Q1Q2.xlsx")

# Q1 data dictionary ----
CA_dict1 <- read_excel("LTM_CA_Q1_DataDictionary.xlsx", 
                       sheet = "Value Labels", 
                       col_names = c("variable", "value", "value_label"))
CA_dict1 <- CA_dict1[-1,]

CA_dict1 <- CA_dict1 %>% 
  fill(variable) 

CA_dict1_2 <-  read_excel("LTM_CA_Q1_DataDictionary.xlsx", 
                          sheet = "Variable Labels",
                          col_names = T)

colnames(CA_dict1_2) <- c("variable", "variable_label", "missing")
CA_dict1_3 <- CA_dict1_2 %>% 
  select(-c("missing"))
data_dict1 <- merge(CA_dict1, CA_dict1_3)
data_dict1 <- data_dict1 %>% 
  mutate(value = as.numeric(value))

# Q2 data dictionary ----
CA_dict2 <- read_excel("LTM_CA_Q1Q2_DataDictionary.xlsx", 
                       sheet = "Value Labels", 
                       col_names = c("variable", "value", "value_label"))
CA_dict2 <- CA_dict2[-1,]

CA_dict2 <- CA_dict2 %>% 
  fill(variable) 

CA_dict2_2 <-  read_excel("LTM_CA_Q1Q2_DataDictionary.xlsx", 
                          sheet = "Variable Labels",
                          col_names = T)

colnames(CA_dict2_2) <- c("variable", "variable_label", "missing")
CA_dict2_3 <- CA_dict2_2 %>% 
  select(-c("missing"))
data_dict2 <- merge(CA_dict2, CA_dict2_3)
data_dict2 <- data_dict2 %>% 
  mutate(value = as.numeric(value))
