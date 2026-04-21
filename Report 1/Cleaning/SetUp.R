
# Data Dictionary ----
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 1")
dict <- read_excel("Data_Dictionary.xlsx", 
                   sheet = "Variable Values") 
dict <- dict[2:4934,]
dict2 <-  read_excel("Data_Dictionary.xlsx", 
                     sheet = "Variable Labels")

colnames(dict) <- c("variable", "value", "value_label")

dict <- dict %>% 
  fill(variable) #%>% 
# mutate(value = as.numeric(value))

colnames(dict2) <- c("variable", "variable_label", "missing")
dict3 <- dict2 %>% select(-c("missing"))
data_dict <- merge(dict, dict3)
data_dict <- data_dict %>% mutate(value = as.numeric(value))

# Bases ----
bases <- read.csv("Bases.csv")
bases <- bases %>% 
  rename(variable = Variable.Name) %>%
  full_join(dict2)
base_cols = bases %>% subset(!is.na(Base)) %>% pull(variable)
