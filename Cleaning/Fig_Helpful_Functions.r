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
      filter(!is.na({{var}})) %>%
      filter({{var}} != "Missing") %>%
      group_by({{var}}) %>%
      summarize(prop = survey_prop(vartype = "ci")) %>% 
      select(-c("prop_low", "prop_upp")) %>%
      as.data.frame()
    
  } else {
    
    # Parse the condition into an expression
    filter_expr <- parse_expr(condition_string)
    
    # Subset the data
    # filtered_data <- data %>%
    #   filter(!!filter_expr)
    
    tab1 <- data %>%
      filter(!!filter_expr) %>%
      filter(!is.na({{var}})) %>%
      filter({{var}} != "Missing") %>%
      group_by({{var}}) %>%
      summarize(prop = survey_prop(vartype = "ci")) %>% 
      select(-c("prop_low", "prop_upp")) %>%
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
    filter(!is.na({{var1}}) & !is.na({{var2}})) %>%
    filter({{var1}} != "Missing" & {{var2}}  != "Missing") %>%
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
                    Weighted = NA)
  
  for(i in cols) {
    tab2 <- print.fig(i) %>% 
      rename("Var" = i) %>%
      filter(Var != "Not selected") 
    dat[dat$Object == i, 2] <- tab2[,2]
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
    tab1 <- fig.2by2.bases(var, i) %>% select(-c(`Not selected`))
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

# Compile ----
fig_compile <- function(maincol, others = c("RACE", "INSURANCE", "URBANICITY2")){
  fig_1_2a <- print.fig(maincol) %>% 
    as.data.frame() %>% 
    arrange(prop)
  
  for(i in others){
    fig_1_2b <- fig.2by2.bases(i,maincol) %>% 
      as.data.frame() %>% t() %>% as.data.frame()
    colnames(fig_1_2b) <- fig_1_2b[1,] 
    fig_1_2b <- fig_1_2b[-1,]
    fig_1_2b[[maincol]] <- rownames(fig_1_2b)
    fig_1_2a <- merge(fig_1_2a, fig_1_2b)
  }
  return(fig_1_2a)
  
}

fig_compile_2 <- function(cols, others = c("RACE", "INSURANCE", "URBANICITY2")){
  fig <- data.frame()
  for(i in cols){
    fig2 <- fig_compile(i, others)
    colnames(fig2)[1] <- "Var"
    rownames(fig2) <- NULL
    fig <- rbind(fig, fig2)
  }
  
  return(fig)
}


r_svysummary <- function(by, include){
  LTM_dsn %>% 
    tbl_svysummary(by = by, 
                   include = include, 
                   statistic = list(all_categorical() ~ "{p}%")) %>% 
    add_ci(style_fun = list(all_categorical() ~
                              label_style_sigfig(scale = 1000)))
}

