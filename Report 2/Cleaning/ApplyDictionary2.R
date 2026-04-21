# Data Dictionary Application
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/Data Cleaning.R")

# Data dictionary prep ----
# Getting rid of columns with no conversion in dict
dict3 <- dict3 %>% 
  mutate(KEEP = case_when(variable %in% colnames(LTM3) ~ 1, TRUE ~ 0)) %>% 
  subset(KEEP == 1) %>% 
  select(-c(KEEP))

LTM_include <- LTM3 %>% select(dict3$variable)

# 
LTM3 <- LTM3 %>% 
  select(c(setdiff(colnames(LTM3), colnames(LTM_include)), MDID))

# Apply dictionary recoding and labeling 
LTM_include <- recode_vrs(data = LTM_include, 
                          data_dictionary = data_dict, 
                          vrs = colnames(LTM_include))

# Final dataset 
LTM_final <- LTM3 %>% 
  full_join(LTM_include, by = join_by(MDID))

# Create list of categorical variables by excluding numeric ----
categorical <- LTM_final %>%
  select(-c( 
    # continuous variables
    ))  %>% colnames()

# Convert continuous variables into numeric class ----
continuous <- LTM_final %>% 
  select(c(
    # Continuous variables
  )) %>% 
  colnames()

for(i in continuous){
  LTM_final[[i]] <- as.numeric(LTM_final[[i]])
}

for (i in seq_len(nrow(dict2))) {
  var_name <- dict2$variable[i]
  var_label <- dict2$variable_label[i]
  
  if (var_name %in% names(LTM_final)) {
    var_label(LTM_final[[var_name]]) <- var_label
  }
}

LTM_final <- LTM_final %>% 
  mutate(FINALWT = as.numeric(FINALWT))

LTM_dsn <- LTM_final %>%
  as_survey_design(weight = FINALWT, id = 1)

