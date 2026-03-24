# general table  ----
r_svy6 <- function(by_var=NULL, 
                   inc_var = all_of(comp_cols),
                   dat = LTM_dsn){
  data6 <- tbl_svysummary(data = dat,
                          by = by_var, 
                          include = inc_var, 
                          statistic = list(all_categorical() ~ "{n_unweighted};{p}")) %>% 
    add_ci(style_fun = list(all_categorical() ~
                              label_style_sigfig(digits = 3, scale = 100),
                            all_continuous() ~ 
                              label_style_sigfig(digits = 4))) %>% 
    as.data.frame()
  
  return(data6)
}


# Impact White ----
data_comp_IW <- function(inc_var){

test <- r_svy6(by_var = "IMPACT_WHITE", 
               inc_var = inc_var)
colnames(test) <- c("Characteristics",
                    "WhiteAlone", "WA95",
                    "BlackWhite", "BW95",
                    "WhiteCombiNoBlack", "WCNB95",
                    "MultiNoWhite", "MNW95")
test <- test %>% 
  mutate(WhiteAlone = paste(WhiteAlone, WA95, sep = ";"),
         BlackWhite = paste(BlackWhite, BW95, sep = ";"),
         WhiteCombiNoBlack = paste(WhiteCombiNoBlack, WCNB95, sep = ";"),
         MultiNoWhite = paste(MultiNoWhite, MNW95, sep = ";")) %>% 
  select(-c(ends_with("95"))) 
var <- test[1,1]
test <- test[-1,]

if("Not selected" %in% unique(LTM_6[[inc_var]])){
test <- test 
} else {
  test <- test %>%
    arrange(desc(Characteristics))}


test <- test %>%
  separate_wider_delim(
    cols = c(WhiteAlone, BlackWhite,WhiteCombiNoBlack,MultiNoWhite),
    delim = ";",
    names_sep = "_",
    too_few = "align_end") %>% 
  t() %>% as.data.frame()
colnames(test) <- test[1,]
test <- test[-1,]
test$variables <- rownames(test) 
test <- test %>% 
  relocate(variables)

test <- test %>% 
  mutate(Variable = word(variables, 1, sep = "_"), 
         variables = str_extract(variables, "[^_]+$")) 

test <- test %>% 
  mutate(variables = case_when(variables == 1 ~ "Count", 
                               variables == 2 ~ "Proportion", 
                               variables == 3 ~ "ConfInt")) 
rownames(test) <- NULL
vals <- colnames(test)[!colnames(test) %in% c("Variable", "variables")]
test <- test %>%
  relocate(Variable) %>%
  pivot_wider(names_from = variables, 
              id_cols = Variable,
              values_from = vals, 
              names_sep = "_") %>% 
  select(-c(starts_with("variables_")))

row1 <- data.frame(c(" ", 
                     rep(c("Count", "Proportion", "ConfInt"), 
                         (ncol(test)-1)/3))) %>% 
  t() %>% as.data.frame()
rownames(row1) <- NULL
colnames(row1) <- colnames(test)
test <- rbind(row1, test)
test <- rbind(colnames(test), test)
test <- rbind(c(var, rep(" ", ncol(test)-1)), test)
colnames(test) <- NULL

return(test)

}

# Impact Black ----
data_comp_BW <- function(inc_var){
  
  test <- r_svy6(by_var = "IMPACT_BLACK", 
                 inc_var = inc_var)
  colnames(test) <- c("Characteristics",
                      "BlackAlone", "BA95",
                      "BlackWhite", "BW95",
                      "BlackCombiNoWhite", "BCNW95",
                      "MultiNoBlack", "MNB95")
  test <- test %>% 
    mutate(BlackAlone = paste(BlackAlone, BA95, sep = ";"),
           BlackWhite = paste(BlackWhite, BW95, sep = ";"),
           BlackCombiNoWhite = paste(BlackCombiNoWhite, BCNW95, sep = ";"),
           MultiNoBlack = paste(MultiNoBlack, MNB95, sep = ";")) %>% 
    select(-c(ends_with("95"))) 
  var <- test[1,1]
  test <- test[-1,]
  
  if("Not selected" %in% unique(LTM_6[[inc_var]])){
    test <- test 
  } else {
    test <- test %>%
      arrange(desc(Characteristics))}
  
  
  test <- test %>%
    separate_wider_delim(
      cols = c(BlackAlone, BlackWhite,BlackCombiNoWhite,MultiNoBlack),
      delim = ";",
      names_sep = "_",
      too_few = "align_end") %>% 
    t() %>% as.data.frame()
  colnames(test) <- test[1,]
  test <- test[-1,]
  test$variables <- rownames(test) 
  test <- test %>% 
    relocate(variables)
  
  test <- test %>% 
    mutate(Variable = word(variables, 1, sep = "_"), 
           variables = str_extract(variables, "[^_]+$")) 
  
  test <- test %>% 
    mutate(variables = case_when(variables == 1 ~ "Count", 
                                 variables == 2 ~ "Proportion", 
                                 variables == 3 ~ "ConfInt")) 
  rownames(test) <- NULL
  vals <- colnames(test)[!colnames(test) %in% c("Variable", "variables")]
  test <- test %>%
    relocate(Variable) %>%
    pivot_wider(names_from = variables, 
                id_cols = Variable,
                values_from = vals, 
                names_sep = "_") %>% 
    select(-c(starts_with("variables_")))
  
  row1 <- data.frame(c(" ", 
                       rep(c("Count", "Proportion", "ConfInt"), 
                           (ncol(test)-1)/3))) %>% 
    t() %>% as.data.frame()
  rownames(row1) <- NULL
  colnames(row1) <- colnames(test)
  test <- rbind(row1, test)
  test <- rbind(colnames(test), test)
  test <- rbind(c(var, rep(" ", ncol(test)-1)), test)
  colnames(test) <- NULL
  
  return(test)
  
}
# Alone ----
data_comp_alone <- function(inc_var){
  
  test <- r_svy6(by_var = "RACEALONE", 
                 inc_var = inc_var)
  colnames(test) <- c("Characteristics",
                      "AIAN", "AIAN95",
                      "Asian", "Asian95",
                      "Black", "Black95", 
                      "Latine", "Latine95",
                      "MENA", "MENA95", 
                      "NHPI", "NHPI95",
                      "White", "White95")
  test <- test %>% 
    mutate(AIAN = paste(AIAN, AIAN95, sep = ";"),
           Asian = paste(Asian, Asian95, sep = ";"),
           Black = paste(Black, Black95, sep = ";"),
           Latine = paste(Latine, Latine95, sep = ";"),
           MENA = paste(MENA, MENA95, sep = ";"),
           NHPI = paste(NHPI, NHPI95, sep = ";"),
           White = paste(White, White95, sep = ";")) %>% 
    select(-c(ends_with("95"))) 
  var <- test[1,1]
  test <- test[-1,]
  
  if("Not selected" %in% unique(LTM_6[[inc_var]])){
    test <- test 
  } else {
    test <- test %>%
      arrange(desc(Characteristics))}
  
  
  test <- test %>%
    separate_wider_delim(
      cols = c("AIAN", "Asian", "Black", "Latine", "MENA", "NHPI", "White"),
      delim = ";",
      names_sep = "_",
      too_few = "align_end") %>% 
    t() %>% as.data.frame()
  colnames(test) <- test[1,]
  test <- test[-1,]
  test$variables <- rownames(test) 
  test <- test %>% 
    relocate(variables)
  
  test <- test %>% 
    mutate(Variable = word(variables, 1, sep = "_"), 
           variables = str_extract(variables, "[^_]+$")) 
  
  test <- test %>% 
    mutate(variables = case_when(variables == 1 ~ "Count", 
                                 variables == 2 ~ "Proportion", 
                                 variables == 3 ~ "ConfInt")) 
  rownames(test) <- NULL
  vals <- colnames(test)[!colnames(test) %in% c("Variable", "variables")]
  test <- test %>%
    relocate(Variable) %>%
    pivot_wider(names_from = variables, 
                id_cols = Variable,
                values_from = vals, 
                names_sep = "_") %>% 
    select(-c(starts_with("variables_")))
  
  row1 <- data.frame(c(" ", 
                       rep(c("Count", "Proportion", "ConfInt"), 
                           (ncol(test)-1)/3))) %>% 
    t() %>% as.data.frame()
  rownames(row1) <- NULL
  colnames(row1) <- colnames(test)
  test <- rbind(row1, test)
  test <- rbind(colnames(test), test)
  test <- rbind(c(var, rep(" ", ncol(test)-1)), test)
  colnames(test) <- NULL
  
  return(test)
  
}
