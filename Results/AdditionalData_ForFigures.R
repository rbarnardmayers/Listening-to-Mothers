LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  tbl_svysummary(include = c(MEDINDUCE))

LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  tbl_svysummary(by = MEDINDUCE, include = c(PAINMEDSC1))

LTM_dsn %>% 
  subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  subset(MEDINDUCE == "Yes") %>% 
  # subset(MEDINDUCE == "No") %>% 
  tbl_svysummary(by = PAINMEDSC1, include = c(MODE2023))

LTM_dsn %>% 
  # subset(NUMB_BIRTH == 1 & GESTAGE >= 37 & GESTAGE <= 42) %>% 
  summarize(n = unweighted(n()))

