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
library(labelled)

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
                    Unweighted = NA,
                    Weighted = NA, 
                    Base = NA)
  
  for(i in cols) {
    tab2 <- print.cat.from.bases(i) %>% 
      filter(Group == "Yes")
    tab1 <- print.cat.from.bases(i) %>% 
      filter(Group == "Total") %>% 
      select(Values) %>%
      rename(Total = Values)
    
    tab <- cbind(tab2, tab1)
    dat[dat$Object == i, 2:4] <- tab[,2:4]
  }
  return(dat)
}

# 2 by 2 for multiple categories ----
collapse.2by2 <- function(cols, var, data = LTM_dsn){
  
  dat <- data %>% 
    group_by(!!sym(var)) %>% 
    summarise(N = n()) %>% as.data.frame() %>% 
    select(-c(N))
  colnames(dat) <- c(var)
  
  for(i in cols){
    # var_sym <- sym(i)
    #####
    tab1 <- fig.2by2.bases(var, i) %>% select(-c("No"))
    # tab1 <- data %>%
    #   group_by(!!sym(var), !!var_sym) %>%
    #   summarize(prop = survey_prop(vartype = "ci")) %>% 
    #   as.data.frame()
    colnames(tab1) <- c(var, i)
    # 
    # tab1 <- tab1 %>% 
    #   filter(yn == "Yes") %>%
    #   select(-c(lower, upper)) %>%
    #   spread(key = yn, value = prop) 
    # colnames(tab1) <- c("Variable", i)
    dat <- merge(dat, tab1)
  }
  
  return(dat)
}


