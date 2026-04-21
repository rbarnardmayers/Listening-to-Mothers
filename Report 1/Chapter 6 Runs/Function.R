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

# Get confidence intervals ----

get_confint <- function(data){
  colnames(data) <- data[2,]
  
  # Get column names where row 3 equals "ConfInt"
  conf_cols <- names(data)[data[3, ] == "ConfInt"]
  
  # Pull out just those columns
  data <- data[-c(1:3), c("Variable",conf_cols)]
  
  data <- data %>%
    separate_wider_delim(
      cols = conf_cols,
      delim = ",",
      names_sep = "_")
  
  data[data == "NA"] <- NA
  data <- data %>% select(where(~!all(is.na(.))))
  
  data <- as.data.frame(
    apply(data, 2, function(x) gsub("%", "", x)),
    stringsAsFactors = FALSE
  )
  
  for(i in 2:ncol(data)){
    data[,i] <- as.numeric(data[,i])
  }
  
  row_names <- data[,1]
  n <- nrow(data)
  
  # get list of pairs for testing
  list_of_dfs <- list()
  for (i in seq(1, ncol(data)-1, by = 2)+1) {
    # Select the current pair of columns
    pair_df <- data[, c(i, i + 1)]
    
    # Add the pair to the list, naming the element for clarity
    list_of_dfs[[paste0("pair_", i, "_", i + 1)]] <- pair_df
  }
  
  full_res <- data.frame(row = row_names)
  
  for(b in list_of_dfs){
    dat_temp <- data[,c(names(b))]
    
    overlap_results <- data.frame(row = character(),
                                  significant_difference = character())
    for (i in 1:n) {
      overlapping_rows <- c()
      
      for (j in 1:n) {
        if (i != j) {
          overlaps <- dat_temp[i,1] <= dat_temp[j,2] && dat_temp[j,1] <= dat_temp[i,2]
          if (overlaps) {
            overlapping_rows <- c(overlapping_rows, row_names[j])
          }
        }
      }
      
      overlap_results <- rbind(overlap_results, 
                               data.frame(row= row_names[i],
                                          significant_difference = ifelse(length(overlapping_rows) == 0, 
                                                                          "none", paste(overlapping_rows, 
                                                                                        collapse = ", "))
                               ))
    }
    colnames(overlap_results) <- c("row", colnames(dat_temp[1]))
    full_res <- merge(full_res, overlap_results, by = "row")
    
  }
  
  colnames(full_res) <- gsub("_ConfInt", "", colnames(full_res))
  
  row1 <- data.frame(V1 = rep(" ", ncol(full_res)),
                     V2 = rep(" ", ncol(full_res)),
                     V3 = rep(" ", ncol(full_res))) 
  colnames(row1) <- colnames(full_res)
 full_res <- rbind(row1, full_res)
  
  
  return(full_res)
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
data_comp_IB <- function(inc_var){
  
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

# Afrolatine ----
data_comp_AL <- function(inc_var){
  
  test <- r_svy6(by_var = "AFROLATINE", 
                 inc_var = inc_var)
  colnames(test) <- c("Characteristics",
                      "BlackLatine", "BL95",
                      "Other", "O95")
  test <- test %>% 
    mutate(BlackLatine = paste(BlackLatine, BL95, sep = ";"),
           Other = paste(Other, O95, sep = ";")) %>% 
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
      cols = c("BlackLatine", "Other"),
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

# Latine White ----
data_comp_LW <- function(inc_var){
  
  test <- r_svy6(by_var = "LATINEWHITE", 
                 inc_var = inc_var)
  colnames(test) <- c("Characteristics",
                      "Other", "O95",
                      "WhiteLatine", "WL95")
  test <- test %>% 
    mutate(WhiteLatine = paste(WhiteLatine, WL95, sep = ";"),
           Other = paste(Other, O95, sep = ";")) %>% 
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
      cols = c("WhiteLatine", "Other"),
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


