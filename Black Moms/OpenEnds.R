source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 1/Cleaning/ApplyDictionary.R")
library(openxlsx)

Verbatims <- read_excel("~/Desktop/Verbatims.xlsx")
Verbatims <- Verbatims %>% select(c(MDID,UID2, WENTWELL, DIDNTGOWELL)) %>% mutate(UID2 = as.character(UID2))
LTM_final <- LTM_final %>% mutate(UID2 = as.character(UID2)) %>%
  full_join(Verbatims)

path = "~/Documents/2025-2026/LTM/Listening-to-Mothers/Black Moms/AllOpenEnds.xlsx"

Edited_V_DIDNTGOWELL <- read_excel(path, sheet = "Edited_V_DIDNTGOWELL")
Edited_V_WENTWELL <- read_excel(path, sheet = "Edited_V_WENTWELL") 
# Edited_V_ANYTHINGELSE <- read_excel(path, sheet = "Edited_V_ANYTHINGELSE") #%>% mutate(Respondent = as.character(Respondent))
# Edited_V_DISCRIMINATION1C18O <- read_excel(path, sheet = "Edited_V_DISCRIMINATION1C18O")
# Edited_V_DISCRIMINATION2O <- read_excel(path, sheet = "Edited_V_DISCRIMINATION2O")
# Edited_V_INDUCE6 <- read_excel(path, sheet = "Edited_V_INDUCE6")
# Edited_EDUCIMPACTC7O <-read_excel(path, sheet = "Edited_EDUCIMPACTC7O")
# Edited_NOPRENATALC13O <- read_excel(path, sheet = "Edited_NOPRENATALC13O")
# Edited_PLANNEDCO <-read_excel(path, sheet = "Edited_PLANNEDCO")
# Edited_V_MEDINDUCE3C8O <- read_excel(path, sheet = "Edited_V_MEDINDUCE3C8O")
# Edited_V_SELFINDUCE2C8O <-read_excel(path, sheet = "Edited_V_SELFINDUCE2C8O")
# Edited_WHYTELEC7O <- read_excel(path, sheet = "Edited_WHYTELEC7O")
# Edited_V_DOULA3 <- read_excel(path, sheet = "Edited_V_DOULA3")

collist_O <- c('Edited_V_DIDNTGOWELL', 'Edited_V_WENTWELL'#,
               # 'Edited_V_ANYTHINGELSE',"Edited_V_DOULA3","Edited_V_DISCRIMINATION1C18O",
               # 'Edited_V_DISCRIMINATION2O', 'Edited_V_INDUCE6', 'Edited_EDUCIMPACTC7O',
               # 'Edited_NOPRENATALC13O', 'Edited_PLANNEDCO', 'Edited_V_MEDINDUCE3C8O',
               # 'Edited_V_SELFINDUCE2C8O', 'Edited_WHYTELEC7O'
)


for(i in collist_O){
  num = match(i, collist_O)
  dat_o <- get(i)
  dat <- dat_o[-c(1,2),]
  colnames(dat) <- dat_o[2,]
  colnames(dat)[3] <- sub(".*_", "", collist_O[num])
  assign(paste0(i, "_1"), dat_o[1:2,])
  assign(i, dat)
  
}

for(i in collist_O){
  num = match(i, collist_O)
  dat <- LTM_final %>% mutate(UID2 = as.character(UID2)) %>% 
    select(c("RACEALONE", "RACE", "RACEC1", "RACEC2", "RACEC3", "RACEC4", "RACEC5", 
             "RACEC6", "RACEC7", sub(".*_", "", collist_O[num]), UID2)) %>% 
    right_join(get(i), by = c(sub(".*_", "", collist_O[num]), "UID2" = "Respondent"))
  
  
  
  assign(paste0(i, "_2"), dat)
}


collist_2 <- paste0(collist_O, "_2")

for(i in collist_2){
  num = match(i, collist_2)
  original <- get(i) %>% subset(!is.na(RACE))
  test1 <- get(i) %>% subset(is.na(RACE)) %>% 
    select(-c("RACEALONE", "RACE", "RACEC1", "RACEC2", "RACEC3", "RACEC4", "RACEC5", 
              "RACEC6", "RACEC7"))
  test <- LTM_final %>% 
    select(c("RACEALONE", "RACE", "RACEC1", "RACEC2", "RACEC3", "RACEC4", "RACEC5", 
             "RACEC6", "RACEC7", sub(".*_", "", collist_O[num]))) %>% 
    right_join(test1, by = c(sub(".*_", "", collist_O[num])))
  
  test <- rbind(original, test) %>% distinct()
  
  assign(paste0(i, "_3"), test)
}

collist_3 <- paste0(collist_O, "_3")

for(i in collist_O){
  
  dat <- get(paste0(i, "_2_3")) %>% 
    rename(Respondent = UID2) %>%
    relocate(colnames(get(i)))
  
  dat_o <- get(paste0(i, "_1")) %>% 
    mutate(RACEALONE= NA,
           RACE= NA, 
           RACEC1= NA, 
           RACEC2= NA, 
           RACEC3= NA,
           RACEC4= NA,
           RACEC5= NA ,
           RACEC6= NA,
           RACEC7 = NA)
  
  # print(head(dat_o))
  # colnames(dat) <- NULL
  # colnames(dat_o) <- NULL
  dat_all <- bind_rows(dat_o, dat)
  # print(head(dat_all))
  
  assign(paste0(i, "_FINAL"), dat_all)
}

collist_final <- paste0(collist_O, "_FINAL")

wb <- loadWorkbook(path)

for(name in collist_final){
  writeData(wb, sheet = sub("_FINAL.*", "", name), x = get(name))
  saveWorkbook(wb, path, overwrite = TRUE)
}

# WENTWELL DIDN'T GO WELL 
WENTWELL_t <- LTM_final %>% select(c(MDID, UID2, RACE, RACEC1, RACEC2, RACEC3, 
                                   RACEC4, RACEC5, RACEC6, RACEC7)) %>% 
  right_join(Edited_V_WENTWELL, join_by( 'UID2' == 'Respondent'))
WENTWELL_t2 <- Edited_V_WENTWELL %>% 
  select(c(Respondent)) %>% 
  left_join(select(LTM_final, c(UID2, WENTWELL)), by = join_by(Respondent == UID2))

DIDNTGOWELL_t <- LTM_final %>% select(c(MDID, UID2, RACE, RACEC1, RACEC2, RACEC3, 
                                     RACEC4, RACEC5, RACEC6, RACEC7)) %>% 
  right_join(Edited_V_DIDNTGOWELL, join_by( 'UID2' == 'Respondent'))

DIDNTGOWELL_t2 <- Edited_V_DIDNTGOWELL %>% 
  select(c(Respondent)) %>% 
  left_join(select(LTM_final, c(UID2, DIDNTGOWELL)), by = join_by(Respondent == UID2))


setwd("~/Documents/2025-2026/LTM/Listening-to-Mothers/Black Moms")
write.csv(WENTWELL_t, "WENTWELL.csv")
write.csv(DIDNTGOWELL_t, "DIDNTGOWELL.csv")
