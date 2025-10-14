# Libraries ----
library(dplyr)
library(stringr)
library(ggplot2)
library(srvyr)
library(readxl)
library(tidyr)
library(rlang)
library(purrr)
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
likert <- function(col, dat = LTM2){
  dat[,col] <- ifelse(dat[,col] == "No, never", 0,
                      ifelse(dat[,col] == "Yes, a few times", 1, 
                             ifelse(dat[,col] == "Yes, most of the time", 2,
                                    ifelse(dat[,col] == "Yes, all the time", 3, 
                                           ifelse(is.na(dat[,col]), NA, 2)))))
  
  return(dat[,col])
}

# Reverse Likert Function ----
rev.likert <- function(col, dat = LTM2){
  dat[,col] <- ifelse(dat[,col] == "No, never", 3,
                      ifelse(dat[,col] == "Yes, a few times", 2, 
                             ifelse(dat[,col] == "Yes, most of the time", 1,
                                    ifelse(dat[,col] == "Yes, all the time", 0,NA))))
  
  return(dat[,col])
}


# Refactor categorical ----
refac.fun <- function(col, vars = c("Missing"), dat = LTM_final){
  dat[[col]] <- ifelse(is.na(dat[[col]]), "Missing", dat[[col]])
  dat[[col]] <- factor(dat[[col]])
  t <- data.frame(names = levels(dat[[col]]))
  t_n <- t$names 
  all <- t_n[!(t_n %in% vars)]
  all <- c(all, vars)
  dat[[col]] <- factor(dat[[col]], levels = all)
  return(dat[[col]])
}

# Refactor numeric ----
refac.num <- function(col, val = c("Missing"), dat = LTM_final){
  dat[[col]] <- factor(dat[[col]])
  t <- data.frame(names = levels(dat[[col]]))
  t_n <- t$names 
  all <- t_n[!(t_n %in% val)]
  all <- as.numeric(all)
  all <- sort(all)
  all <- c(all, val)  
  dat[[col]] <- factor(dat[[col]], levels = all)
  return(dat[[col]])
  
}

# PHQ amd GAD ----
recode.phq <- function(col, dat = LTM2){
  dat[,col] <- ifelse(dat[,col] == 4, 0,
                      ifelse(dat[,col] == 3, 1,
                             ifelse(dat[,col] == 2, 2, 
                                    ifelse(dat[,col] == 1, 3, NA))))
  
  return(dat[,col])
  
}

# Print out categorical frequencies ----
# Don't use alone, use .from.bases function 
print.cat <- function(var, data = LTM_dsn){
  var <- parse_expr(var)
  tab1 <- data %>%
    # as_survey(weights = c(wght)) %>%
    group_by({{var}}) %>%
    # group_by(across(all_of(var))) %>%
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

# Categorical with bases included ----
print.cat.from.bases <- function(var_name, data = LTM_dsn, bases_lookup = bases) {
  # Get the condition from the bases table
  condition_string <- bases_lookup %>%
    filter(variable == var_name) %>%
    pull(Base)
  
  if (length(condition_string) == 0 || is.na(condition_string)) {
    result <- print.cat(var = var_name, data = data)
  } else{
    
    # Parse the condition into an expression
    filter_expr <- parse_expr(condition_string)
    
    # Subset the data
    filtered_data <- data %>%
      filter(!!filter_expr)
    
    # Use the original print.cat with the filtered data and variable
    result <- print.cat(var = var_name, data = filtered_data)
  }
  
  return(result)
  
}


# Print out categorical frequencies for figures----
print.fig <- function(var, data = LTM_dsn, bases_lookup = bases){
  
  condition_string <- bases_lookup %>%
    filter(variable == var) %>%
    pull(Base)
  var <- parse_expr(var)
  
  if (length(condition_string) == 0 || is.na(condition_string)) {
    tab1 <- data %>%
      group_by({{var}}) %>%
      summarize(prop = survey_prop(vartype = "ci")) %>% 
      as.data.frame()
    
  } else {
    
    # Parse the condition into an expression
    filter_expr <- parse_expr(condition_string)
    
    # Subset the data
    # filtered_data <- data %>%
    #   filter(!!filter_expr)
    
    tab1 <- data %>%
      filter(!!filter_expr) %>%
      group_by({{var}}) %>%
      summarize(prop = survey_prop(vartype = "ci")) %>% 
      as.data.frame()
    
  }
  return(tab1)
  
}


# Print continuous info  ----
# don't run alone, use print.cont.from.bases
print.cont <- function(var, data = LTM_dsn){
  var_sym <- parse_expr(var)
  tab1 <- data %>%
    summarize(unweighted = unweighted(n()), 
              mean = survey_mean({{var_sym}}, na.rm = T, vartype = "ci"))%>% 
    mutate(mean = round(mean,1), 
           ci = paste0(round(mean_low, 1), ", ", round(mean_upp,1))) %>%
    select(-c(mean_low, mean_upp))
  colnames(tab1) <- c("Unweighted Count", "Mean", "Confidence Interval")
  return(tab1)
}

# Continuous with bases -----
print.cont.from.bases <- function(var_name, data = LTM_dsn, 
                                  bases_lookup = bases) {
  # Get the condition from the bases table
  condition_string <- bases_lookup %>%
    filter(variable == var_name) %>%
    pull(Base)
  
  if (length(condition_string) == 0 || is.na(condition_string)) {
    print.cont(var = var_name, data = data)
  } else{
    
    # Parse the condition into an expression
    filter_expr <- parse_expr(condition_string)
    
    # Subset the data
    filtered_data <- data %>%
      filter(!!filter_expr)
    
    # Use the original print.cat with the filtered data and variable
    result <- print.cont(var = var_name, data = filtered_data)
  }
  return(result)
}

# Continuous info by groups ----
print.cont.groups <- function(var1, var2, data = LTM_dsn, 
                              bases_lookup = bases){
  var1 <- parse_expr(var1)
  var2 <- parse_expr(var2)
  
  condition_string <- bases_lookup %>%
    filter(variable == var1) %>%
    pull(Base)
  
  if (length(condition_string) == 0 || is.na(condition_string)) {
    tab1 <- data %>%
      group_by({{var1}}) %>%
      summarize(N = unweighted(n()), 
                mean = survey_mean({{var2}}, na.rm = T, vartype = "ci"))%>% 
      mutate(mean = round(mean,2)) %>%
      select(-c(mean_low, mean_upp))
    colnames(tab1) <- c("Groups", "Unweighted Count", "Mean")  
  } else {
    
    tab1 <- data %>%
      group_by({{var1}}) %>%
      summarize(N = unweighted(n()), 
                mean = survey_mean({{var2}}, na.rm = T, vartype = "ci"))%>% 
      mutate(mean = round(mean,2)) %>%
      select(-c(mean_low, mean_upp))
    colnames(tab1) <- c("Groups", "Unweighted Count", "Mean")
  }    
  return(tab1)

}

# Look at 2 by 2 tables -- don't want to use ----

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
# Will group % by first variable listed
# Don't use alone
fig.2by2 <- function(var1, var2, data = LTM_dsn){
  var1 <- parse_expr(var1)
  var2 <- parse_expr(var2)
  
  tab1 <- data %>%
    group_by({{var1}}, {{var2}}) %>%
    summarize(prop = survey_prop(vartype = "ci")) %>% 
    select(-c(prop_low, prop_upp)) %>% 
    spread(key = {{var2}}, value = prop)
  return(tab1)
}

# print 2 by 2 without unweighted counts for bases ----

fig.2by2.bases <- function(var1, var_name, data = LTM_dsn, 
                           bases_lookup = bases) {
  # Get the condition from the bases table
  condition_string <- bases_lookup %>%
    filter(variable == var_name) %>%
    pull(Base)
  
  if (length(condition_string) == 0 || is.na(condition_string)) {
    result <- fig.2by2(var1 = var1, var2 = var_name, 
                       data = data)
  } else{
    
    # Parse the condition into an expression
    filter_expr <- parse_expr(condition_string)
    
    # Subset the data
    filtered_data <- data %>%
      filter(!!filter_expr)
    
    # Use the original print.cat with the filtered data and variable
    result <- fig.2by2(var1 = var1, var2 = var_name, 
                       data = filtered_data)
  }
  return(result)
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

# Bases ----
bases <- read.csv("Bases.csv")
bases <- bases %>% 
  rename(variable = Variable.Name) %>%
  full_join(dict2)
base_cols = bases %>% subset(!is.na(Base)) %>% pull(variable)

