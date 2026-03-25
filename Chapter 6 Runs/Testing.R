inc_var = "SOCIALNEEDC10"
# data_comp_IW <- function(inc_var){

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
#########
row2 <- test[1:3,]
colnames(row2) <- NULL
test <- merge(test,get_confint(test), by.x = colnames(test)[1], by.y = "row")
test <- test[-c(1:3), ]

colnames(test) <- NULL

return(test)

# }