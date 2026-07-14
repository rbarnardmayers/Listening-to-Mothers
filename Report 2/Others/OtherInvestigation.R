library(readxl)
Data_Dictionary2 <- read_excel("Data_Dictionary2.xlsx")

col_othes <- Data_Dictionary2 %>% 
  mutate(keep = case_when(str_ends(Variable, "O") ~ 1, 
                          TRUE ~ 0)) %>% 
  subset(keep == 1)
col_othes <- col_othes$Variable

setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Others")
# PREPREG_PHYSCONDC3O ----
LTM_final %>% 
  subset(PREPREG_PHYSCONDC3O != "") %>% 
  select(c(PREPREG_PHYSCONDC1, PREPREG_PHYSCONDC2, 
           PREPREG_PHYSCONDC3O,
           MDID)) %>%
  View()

#CARESETTING ----
LTM_final %>% 
  subset(CARESETTINGC8O != "") %>%
  select(c(CARESETTINGC1,CARESETTINGC2,CARESETTINGC3,CARESETTINGC4,
           CARESETTINGC5,CARESETTINGC6,CARESETTINGC7,CARESETTINGC8O,
           MDID)) %>%
  View()

# PREGCONDITION ----
LTM_final %>% 
  subset(PREGCONDITIONC9O != "") %>% 
  select(c("PREGCONDITIONC1","PREGCONDITIONC2","PREGCONDITIONC3",
           "PREGCONDITIONC4","PREGCONDITIONC5","PREGCONDITIONC6",
           "PREGCONDITIONC7", "PREGCONDITIONC8", "PREGCONDITIONC9",
           "PREGCONDITIONC9O", "MDID")) %>% 
  View()
# 601453 should be recoded to gestational diabetes
# 601453 i think should be hypertension
# 203461 should be hyperemisis gravidarum 
# 202442 should be anemia

# MISTRUSTC8O ----
mistr <- LTM_final %>% 
  subset(MISTRUSTC8O != "") %>% 
  select(c("MISTRUSTC1","MISTRUSTC2","MISTRUSTC3",
           "MISTRUSTC4","MISTRUSTC5","MISTRUSTC6",
           "MISTRUSTC7", "MISTRUSTC8O", "MISTRUSTC8",
           "MISTRUSTC9", "MDID")) %>% 
  View()
write.csv(mistr, "MISTRUST.csv")

# HOSPSAFEC16O ----
hospsa <- LTM_final %>% 
  subset(HOSPSAFEC16O != "") %>% 
  select(c("HOSPSAFEC1","HOSPSAFEC2","HOSPSAFEC3",
           "HOSPSAFEC4","HOSPSAFEC5","HOSPSAFEC6",
           "HOSPSAFEC7","HOSPSAFEC8","HOSPSAFEC9",
           "HOSPSAFEC10","HOSPSAFEC11","HOSPSAFEC12",
           "HOSPSAFEC13","HOSPSAFEC14","HOSPSAFEC15",
           "HOSPSAFEC16O", 'HOSPSAFEC16', "MDID")) #%>% 
  #View()

write.csv(hospsa, "HOSPSAFE.csv")

# CSECTFEELC10O ----
csecfeel <- LTM_final %>% 
  subset(CSECTFEELC10O != "") %>% 
  select(c("CSECTFEELC1","CSECTFEELC2","CSECTFEELC3",
           "CSECTFEELC4","CSECTFEELC5","CSECTFEELC6",
           "CSECTFEELC7","CSECTFEELC8","CSECTFEELC9",
           "CSECTFEELC10O","CSECTFEELC10", "MDID")) #%>% 
 # View()
write.csv(csecfeel, "CSECTFEEL.csv")

# LACKSAFEC16O ----
lacks <- LTM_final %>% 
  subset(LACKSAFEC16O != "") %>% 
  select(c("LACKSAFEC1","LACKSAFEC2","LACKSAFEC3",
           "LACKSAFEC4","LACKSAFEC5","LACKSAFEC6",
           "LACKSAFEC7","LACKSAFEC8","LACKSAFEC9",
           "LACKSAFEC10","LACKSAFEC11","LACKSAFEC12",
           "LACKSAFEC13","LACKSAFEC14","LACKSAFEC15",
           "LACKSAFEC16O","LACKSAFEC16", "MDID")) #%>% 
  #View()
write.csv(lacks, "LACKSAFE.csv")

# BCOFFEREDC4O
bcoff <- LTM_final %>% 
  subset(BCOFFEREDC4O != "") %>% 
  select(c("BCOFFEREDC1","BCOFFEREDC2","BCOFFEREDC3",
           "BCOFFEREDC4O","BCOFFEREDC4","MDID")) #%>% 
  #View()
write.csv(bcoff, "BCOFFERED.csv")

# INTENDLOCALE 

intendl <- LTM_final %>% 
  subset(INTENDLOCALEO != "") %>% 
  select(c("INTENDLOCALE","INTENDLOCALEO","MDID")) #%>% 
#View()
write.csv(intendl, "INTENDLOCALE.csv")


# COUNSELBARR
counselb <- LTM_final %>% 
  subset(COUNSELBARRC8O != "") %>% 
  select(c('COUNSELBARRC1', 'COUNSELBARRC2', 'COUNSELBARRC3',
           'COUNSELBARRC4', 'COUNSELBARRC5', 'COUNSELBARRC6',
           'COUNSELBARRC7',"COUNSELBARRC8","COUNSELBARRC8O", 
           "MDID")) 
write.csv(counselb, "COUNSELBARR.csv")

# INSURANCE 
insurrcur <- LTM_final %>% 
  subset(INSURCURRC5O != "") %>% 
  select(c('INSURCURRC1', 'INSURCURRC2', 'INSURCURRC3',
           'INSURCURRC4',"INSURCURRC5","INSURCURRC5O", "DISABILITY",
           "MDID")) 
write.csv(insurrcur, "INSURCURR.csv")

# CTJUDGEC19O 
LTM_final %>% 
  subset(CTJUDGEC19O != "") %>% 
  select(c('CTJUDGEC1', 'CTJUDGEC2', 'CTJUDGEC3', 'CTJUDGEC4', 'CTJUDGEC5', 
           'CTJUDGEC6', 'CTJUDGEC7', 'CTJUDGEC8', 'CTJUDGEC9', 'CTJUDGEC10',
           'CTJUDGEC11', 'CTJUDGEC12', 'CTJUDGEC13', 'CTJUDGEC14', 'CTJUDGEC15', 
           'CTJUDGEC16', 'CTJUDGEC17', 'CTJUDGEC18', 'CTJUDGEC19', 'CTJUDGEC19O',
           "MDID"))  %>% View()

LTM_final %>% 
  subset(JUDGEEFFECTC11O != "") %>% 
  select(c('JUDGEEFFECTC1', 'JUDGEEFFECTC2', 'JUDGEEFFECTC3', 'JUDGEEFFECTC4', 
           'JUDGEEFFECTC5', 'JUDGEEFFECTC6', 'JUDGEEFFECTC7', 'JUDGEEFFECTC8', 
           'JUDGEEFFECTC9', 'JUDGEEFFECTC10', 'JUDGEEFFECTC11',
           'JUDGEEFFECTC11O' ,"MDID"))  %>% View()

# MIDWIFECONCERNC7O

miwdifeconc <- LTM %>% 
  subset(MIDWIFECONCERNC7O != "") %>% 
  select(c("MIDWIFECONCERNC1", 'MIDWIFECONCERNC2', 'MIDWIFECONCERNC3', 
           'MIDWIFECONCERNC4', 'MIDWIFECONCERNC5', 'MIDWIFECONCERNC6', 
           'MIDWIFECONCERNC7O', 'MIDWIFECONCERNC7', MDID))
write.csv(miwdifeconc, "MIDWIFECONCERN.csv")

# DOULACONCERN
doulaconc <- LTM_final %>% 
  subset(DOULACONCERNC5O != "") %>% 
  select(c('DOULACONCERNC1', 'DOULACONCERNC2', 'DOULACONCERNC3', 'DOULACONCERNC4', 
           'DOULACONCERNC5O', 'DOULACONCERNC5', MDID ))
write.csv(doulaconc, "DOULACONCERN.csv")
