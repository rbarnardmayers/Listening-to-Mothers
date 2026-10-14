library(janitor)

LTM_Q1_OE <- LTM_final %>% 
  filter(STATE == "California") %>% 
  select(c(MDID, ignores)) %>%
  select(-c(ResLanguage)) %>% 
  relocate(MDID) %>% mutate(MDID = as.character(MDID))

names_q1 <- LTM_Q1_OE %>% 
  select(-c(MDID)) %>% 
  colnames()

LTM_Q1_OE <- LTM_Q1_OE %>% 
  mutate_all(na_if,"") %>% 
  mutate(ANYTHINGELSE = case_when(ANYTHINGELSE == 99 ~ NA, 
                                  TRUE ~ ANYTHINGELSE)) %>% 
  filter(!if_all(.cols = names_q1, .fns = is.na))

LTM_RACE <- LTM_final %>% 
  select(c(MDID, RACEC1, RACEC2, RACEC3, RACEC4, RACEC5,
           RACEC6,RACEC7, RACEC8)) %>% 
  mutate(MDID = as.character(MDID))

LTM_Q1_OE <- LTM_Q1_OE %>% 
  left_join(LTM_RACE)

setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 1/Results")
write.csv(LTM_Q1_OE, "Q1_OEs.csv")

LTM_Q1_OE_Black <- LTM_Q1_OE %>% 
  subset(RACEC3 == "Black or African American")
write.csv(LTM_Q1_OE_Black, "Q1_OEs_BlackOnly.csv")


