# Libraries ----
library(dplyr)
library(stringr)
library(ggplot2)

# Functions ---- 
convert.fun <- function(dat, vars){
  for(i in vars){
    dat[[i]] <- as.numeric(dat[[i]])
  }
  return(dat)
}

# Data Read in ----
LTM <- read.csv("Data_7.25.25.csv")

# Get rid of identifying information 
LTM1 <- LTM[,c(1,2,46,47,55,74:736)]

LTM1 <- LTM1 %>% 
  select(-c(CHILDNAME)) 

# Convert numeric cols
births <- LTM1 %>% select(ends_with("BIRTHYEAR")) %>% names()
pren <- LTM1 %>% select(starts_with("NOPRENATALC")) %>% 
  select(-c("NOPRENATALC13O")) %>% names()
cares <- LTM1 %>% select(c(starts_with("CARESETTING"), starts_with("CARETYPE"), 
                           starts_with("CAREMODE"), starts_with("WHYTELEC"), 
                           starts_with("ATHOMECAREC"))) %>% 
  select(-c("CARESETTINGC8O", "WHYTELEC7O")) %>%  names()
educ <- LTM1 %>% select(starts_with("EDUCMODE"), starts_with("EDUCIMPACT")) %>% 
  select(-c('EDUCIMPACTC7O')) %>% names()
empl <- LTM1 %>% select(starts_with("EMPLOY")) %>% names()
douls <- LTM1 %>% select(starts_with("DOULA")) %>% 
  select(-c('DOULA1C6O', 'DOULA2C6O', 'DOULA3')) %>% names()
induc <- LTM1 %>% select(starts_with("INDUCE"), 
                         starts_with("SELFINDUCE"), starts_with("MEDINDUCE"), 
                         starts_with("MODE")) %>% 
  select(-c('INDUCE6', "SELFINDUCE2C8O","MEDINDUCE3C8O")) %>% names()
drugs <- LTM1 %>%select(c(starts_with("DRUGFREE"))) %>% 
  select(-c(DRUGFREEC10O)) %>% names()
fetal <- LTM1 %>%select(c(starts_with("FETALMON"))) %>% 
  select(-c(FETALMONC3O)) %>% names()
csec <- LTM1 %>%select(c(starts_with("CSECTIONINT"), starts_with("LABORINT"), 
                         starts_with("LABORPERMIT"))) %>% 
  select(-c('LABORPERMIT_D1', 'LABORPERMIT_D2')) %>% names()
post <- LTM1 %>%select(c(starts_with("POSITION2"))) %>% 
  select(-c("POSITION2C7O")) %>% names()
disc <- LTM1 %>%select(c(starts_with("DISCRIMINATION"))) %>% 
  select(-c(ends_with("O"))) %>% names()
topic <- LTM1 %>%select(c(starts_with("VISITTOPIC"))) %>% 
  select(-c(ends_with("O"))) %>% names()
hospf <- LTM1 %>%select(c(starts_with("HOSPFEED"))) %>% 
  select(-c(ends_with("O"))) %>% names()
sn <- LTM1 %>%select(c(starts_with("SN"))) %>% 
  select(-c(ends_with("O"))) %>% names()

allnames <- c(births, pren, cares, educ, empl, douls, induc, drugs, 
              fetal, csec, post, disc, topic, hospf, sn)

LTM2 <-  convert.fun(LTM1, c('MDID', allnames, 'PREPREG_WEIGHT_B1', 
                                     'NUMB_BIRTH_OLD', 
                                     'PREPREG_WEIGHT_A1', 'FIRSTVISIT', 'PROVIDER', 
                                     'PROVIDERCHOICE', 'BPCONFID', 'URINECONFID', 'WEIGHCONFID', 
                                     'BABYHRCONFID', 'CARETYPEPREF', 'CAREMODEPREF', 'PRIOREDUC', 
                                     'CURREDUC', 'TRAP1', 'BIGBABY2','PREGWEIGHT_B1', 
                                     'CURRWEIGHT_KG', 'CURRWEIGHT_LBS', 'CSECTIONTYPE', 
                                     'LABCSEC', 'PLANNEDC','VBACCHOICE', 'VBACINTEREST', 
                                     'VBACEFFORT', 'VBACACCESSC1', 'VBACACCESSC2', 
                                     'VBACACCESSC3', 'VBACACCESSC4', 'VBACACCESSC5', 
                                     'UNPLANNEDREASON', 'VAGASSIST', 'VAGEXAM', 'LABORWALK', 
                                     'EPIST', 'EPISTCHOICE', 'POSITION', 'POSITIONCHOICE', 
                                     'LABORLENGTH', 'BIRTHWEIGHT_LBS', 'BIRTHWEIGHT_OZ', 
                                     'BIRTHWEIGHT_G', 'SUTURE', 'SKIN', 'TRAP2', 'VISITS', 
                                     'PPVISIT', 
                                     'PPVISITTIME1', 'PPVISITTIME2', 'EXCLUSIVEBF', 
                                     'EXCLUSIVEBF', 'EXCLBFGOAL', 'WEAN', 'BFGOAL', 'TRAP3'))




