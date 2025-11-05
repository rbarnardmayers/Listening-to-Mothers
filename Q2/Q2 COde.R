dat <- read_excel(file.choose())

dat1 <- dat  %>% 
  mutate(Variable = case_when(is.na(Num) ~ Test, 
                              is.na(Test) ~ Variable), 
         Test = case_when(is.na(Num) ~ NA, 
                        TRUE ~ Test), 
         New = case_when(is.na(Num) ~ paste0(Variable, ": ", lead(Variable))), 
         Variable = New, 
         Num = case_when(Num == "Freq" ~ NA, 
                         is.na(Num) ~ "Freq", 
                       TRUE ~ Num)) %>% select(-c(New)) %>% subset(!is.na(Num))

write.csv(dat1, "Q2 Results Clean.csv")
