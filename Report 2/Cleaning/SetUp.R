# Dictionary 
setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2")
dict <- read_excel("Data_Dictionary2.xlsx", 
                   sheet = "Values") 
dict <- dict[1:6571,]

dict2 <-  read_excel("Data_Dictionary2.xlsx", 
                     sheet = "Variable")

colnames(dict) <- c("variable", "value", "value_label")

dict <- dict %>% 
  fill(variable) 

colnames(dict2) <- c("variable", "variable_label", "missing")
dict3 <- dict2 %>% select(-c("missing"))
data_dict <- merge(dict, dict3)
data_dict <- data_dict %>% mutate(value = as.numeric(value))

