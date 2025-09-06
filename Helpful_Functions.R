# MAKE A HELPFUL FUNCTION SHEET

# Libraries ----
library(dplyr)
library(stringr)
library(ggplot2)
library(srvyr)
library(readxl)
library(tidyr)
library(phdcocktail)

# Functions ---- 
convert.fun <- function(dat, vars){
  for(i in vars){
    dat[[i]] <- as.numeric(dat[[i]])
  }
  return(dat)
}

convert.yn <- function(dat, vars){
  for(i in vars){
    dat[[i]] <- ifelse(dat[[i]] == 1, "Yes", 
                       ifelse(dat[[i]] == 0, "No", NA))
  }
  return(dat)
}

print.cat <- function(var, data = LTM_final){
  tab1 <- data %>%
    as_survey(weights = c(wght)) %>%
    group_by({{var}}) %>%
    summarize(prop = survey_prop(vartype = "ci")) %>% #n = survey_total(vartype = "ci"),
    mutate(prop = 100 * prop, 
           prop_low = 100*prop_low,
           prop_upp = 100 * prop_upp) %>%
    as.data.frame() %>% 
    mutate(ci = paste0(round(prop_low, 1), "%, ", round(prop_upp, 1), "%"), 
           prop = paste0(round(prop, 1), "%")) %>% 
    select(-c(prop_low, prop_upp)) %>%
    rename(Proportion = prop, 
           CI = ci)
  colnames(tab1) <- c("Values", "Proportion", "Confidence Interval")
  return(tab1)
}

print.cont <- function(var, data = LTM_final){
  tab1 <- LTM_final %>%
    as_survey(weights = c(wght)) %>%
    summarize(mean = survey_mean({{var}}, na.rm = T, vartype = "ci"))%>% 
    mutate(ci = paste0(round(mean_low, 2), ", ", round(mean_upp, 2)), 
           mean = round(mean,2)) %>%
    select(-c(mean_low, mean_upp))
  colnames(tab1) <- c( "Mean", "Confidence Interval")
  return(tab1)
}

print.2by2 <- function(var1, var2){
  tab1 <- LTM_final %>%
    as_survey(weights = c(wght)) %>%
    group_by({{var1}}, {{var2}}) %>%
    summarize(prop = survey_prop(vartype = "ci")) %>% 
    mutate(Value = paste0(round(prop,3)*100, "%, (", round(prop_low, 3)*100, "%, ", round(prop_upp,3)*100, "%")) %>% 
    select(-c(prop, prop_low, prop_upp)) %>% 
    spread(key = {{var2}}, value = Value)
  return(tab1)
}

# Refactor 
refac.fun <- function(col){
  LTM_final[[col]] <- factor(LTM_final[[col]])
  t <- data.frame(names = levels(LTM_final[[col]]))
  t_n <- t$names 
  all <- t_n[t_n != "Missing"]
  all <- c(all, "Missing")
  LTM_final[[col]] <- factor(LTM_final[[col]], levels = all)
  return(LTM_final[[col]])
}

# Refactor numeric 
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









