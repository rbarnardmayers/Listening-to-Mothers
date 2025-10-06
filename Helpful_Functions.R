# Libraries ----
library(dplyr)
library(stringr)
library(ggplot2)
library(srvyr)
library(readxl)
library(tidyr)
library(rlang)
library(phdcocktail)
library(openxlsx)

# Convert to numeric ----
convert.fun <- function(dat, vars){
  for(i in vars){
    dat[[i]] <- as.numeric(dat[[i]])
  }
  return(dat)
}

# Make yes/no from 1/0 ----
convert.yn <- function(dat, vars){
  for(i in vars){
    dat[[i]] <- ifelse(dat[[i]] == 1, "Yes", 
                       ifelse(dat[[i]] == 0, "No", NA))
  }
  return(dat)
}

# Likert function ----
likert <- function(col){
  LTM_final[,col] <- ifelse(LTM_final[,col] == "No, never", 0,
                            ifelse(LTM_final[,col] == "Yes, a few times", 1, 
                                   ifelse(LTM_final[,col] == "Yes, most of the time", 2,
                                          ifelse(LTM_final[,col] == "Yes, all the time", 3, 
                                                 ifelse(is.na(LTM_final[,col]), NA, 2)))))
  
  return(LTM_final[,col])
}

# Reverse Likert Function ----
rev.likert <- function(col){
  LTM_final[,col] <- ifelse(LTM_final[,col] == "No, never", 3,
                            ifelse(LTM_final[,col] == "Yes, a few times", 2, 
                                   ifelse(LTM_final[,col] == "Yes, most of the time", 1,
                                          ifelse(LTM_final[,col] == "Yes, all the time", 0,NA))))
  
  return(LTM_final[,col])
}


# Refactor categorical ----
refac.fun <- function(col, vars = c("Missing")){
  LTM_final[[col]] <- factor(LTM_final[[col]])
  t <- data.frame(names = levels(LTM_final[[col]]))
  t_n <- t$names 
  all <- t_n[!(t_n %in% vars)]
  all <- c(all, vars)
  LTM_final[[col]] <- factor(LTM_final[[col]], levels = all)
  return(LTM_final[[col]])
}

# Refactor numeric ----
refac.num <- function(col, val = c("Missing")){
  LTM_final[[col]] <- factor(LTM_final[[col]])
  t <- data.frame(names = levels(LTM_final[[col]]))
  t_n <- t$names 
  all <- t_n[!(t_n %in% val)]
  all <- as.numeric(all)
  all <- sort(all)
  all <- c(all, val)  
  LTM_final[[col]] <- factor(LTM_final[[col]], levels = all)
  return(LTM_final[[col]])
  
}

# PHQ amd GAD
recode.phq <- function(col){
  LTM_final[,col] <- ifelse(LTM_final[,col] == "Never", 0,
                            ifelse(LTM_final[,col] == "Sometimes", 1,
                                   ifelse(LTM_final[,col] == "Usually", 2, 
                                          ifelse(LTM_final[,col] == "Always", 3, NA))))
  
  return(LTM_final[,col])
  
}

# Print out categorical frequencies ----
print.cat <- function(var, data = LTM_dsn){
  tab1 <- data %>%
    # as_survey(weights = c(wght)) %>%
    group_by({{var}}) %>%
    summarize(unweighted_n = unweighted(n()), 
              prop = survey_prop(vartype = "ci")) %>% #n = survey_total(vartype = "ci"),
    mutate(prop = 100 * prop, 
           prop_low = 100*prop_low,
           prop_upp = 100 * prop_upp) %>%
    as.data.frame() %>% 
    mutate(ci = paste0(round(prop_low, 1), "%, ", round(prop_upp, 1), "%"), 
           prop = paste0(round(prop, 1), "%"),
           results = paste0(prop, " (", ci, ")")) %>%
    select(-c(prop_low, prop_upp, prop,ci))
  colnames(tab1) <- c( "Unweighted Count","Values", "Weighted Proportion")
  
  return(tab1)
}

# Print out categorical frequencies for figures----
print.fig <- function(var, data = LTM_dsn){
  
  tab1 <- data %>%
    group_by({{var}}) %>%
    summarize(prop = survey_prop(vartype = "ci")) %>% 
    as.data.frame()
  
  return(tab1)
}


# Print continuous info ----
print.cont <- function(var, data = LTM_dsn){
  tab1 <- data %>%
    summarize(mean = survey_mean({{var}}, na.rm = T, vartype = "ci"))%>% 
    mutate(ci = paste0(round(mean_low, 2), ", ", round(mean_upp, 2)), 
           mean = round(mean,2)) %>%
    select(-c(mean_low, mean_upp))
  colnames(tab1) <- c("Mean", "Confidence Interval")
  return(tab1)
}

# Look at 2 by 2 tables ----
print.2by2 <- function(var1, var2, data = LTM_dsn){
  tab1 <- data %>%
    group_by({{var1}}, {{var2}}) %>%
    summarize(unweighted = unweighted(n()),
              prop = survey_prop(vartype = "ci")) %>% 
    mutate(Value = paste0(unweighted,"; ", round(prop,3)*100, "%, (", round(prop_low, 3)*100, "%, ", round(prop_upp,3)*100, "%")) %>% 
    select(-c(prop, prop_low, prop_upp)) %>% 
    spread(key = {{var2}}, value = Value)
  return(tab1)
}

# Print 2 by 2 tables for figures ----
fig.2by2 <- function(var1, var2, data = LTM_dsn){
  tab1 <- data %>%
    group_by({{var1}}, {{var2}}) %>%
    summarize(unweighted_n = unweighted(n()), 
              prop = survey_prop(vartype = "ci")) %>% 
    select(-c(prop_low, prop_upp)) %>% 
    spread(key = {{var2}}, value = prop)
  return(tab1)
}

# Collapsing into one dataset across multiple columns ----
collapse.fun <- function(cols, data = LTM_dsn){ 
  dat <- data.frame(Object = cols, 
                    n = NA,
                    pct = NA, 
                    lower = NA, 
                    upper = NA)
  
  for(i in cols) {
    tab <- data %>% 
      group_by(get(i)) %>% 
      summarize(unweighted = unweighted(n()), 
                pct = survey_prop(vartype = "ci")) %>% 
      as.data.frame()
    colnames(tab) <- c("Var","Unweighted", "pct", "pct_low", "pct_upp")
    tab <- tab %>% subset(Var == "Yes")
    dat[dat$Object == i, 2:5] <- tab[,2:5]
  }
  return(dat)
}

# 2 by 2 for multiple categories ----
collapse.2by2 <- function(cols, var, data = LTM_dsn){

  dat <- data %>% 
    group_by(!!sym(var)) %>% 
    summarise(N = n()) %>% as.data.frame() %>% 
    select(-c(N))
  colnames(dat) <- c("Variable")
  
  for(i in cols){
    var_sym <- sym(i)
    
    tab1 <- data %>%
      group_by(!!sym(var), !!var_sym) %>%
      summarize(prop = survey_prop(vartype = "ci")) %>% 
      as.data.frame()
    colnames(tab1) <- c("Variable", "yn", "prop", "lower", "upper")
    tab1 <- tab1 %>% 
      filter(yn == "Yes") %>%
      select(-c(lower, upper)) %>%
      spread(key = yn, value = prop) 
    colnames(tab1) <- c("Variable", i)
    dat <- merge(dat, tab1)
  }
  
  return(dat)
}


# Data Dictionary ----
dict <- read_excel("Data_Dictionary.xlsx", 
                   sheet = "Variable Values") 
dict2 <-  read_excel("Data_Dictionary.xlsx", 
                     sheet = "Variable Labels") %>% 
  select(-c("Variable Type", "Notes"))

colnames(dict) <- c("variable", "value", "value_label")

dict <- dict %>% 
  fill(variable) #%>% 
# mutate(value = as.numeric(value))

colnames(dict2) <- c("variable", "variable_label")
data_dict <- merge(dict, dict2)

