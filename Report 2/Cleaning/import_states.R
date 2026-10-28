
state_dat <- read.csv("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/States.csv")
state_dat <- state_dat %>% 
  mutate(STATE = as.character(STATE))

LTM2 <- LTM2 %>% 
  mutate(STATE = as.character(STATE)) %>%
  left_join(state_dat, by = "STATE") %>%
  mutate(PaidLeaveDate = as.Date(PaidLeaveDate, "%m/%d/%Y"), 
         PaidLeave_any = case_when(is.na(PaidLeaveDate) ~ "No", 
                               !is.na(PaidLeaveDate) ~ "Yes"),
         PaidLeave = case_when(PaidLeave_any == "No" ~ "No", 
                               PaidLeave_any == "Yes" & PaidLeaveDate == 2022 ~ "Yes", 
                               PaidLeave_any == "Yes" & PaidLeaveDate == 2025 ~ "No",
                               PaidLeave_any == "Yes" & BIRTHDATE > PaidLeaveDate ~ "Yes",
                               PaidLeave_any == "Yes" & BIRTHDATE < PaidLeaveDate ~ "No"))

