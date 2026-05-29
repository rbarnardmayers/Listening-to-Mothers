source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Report 2/Cleaning/ApplyDictionary2.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")


# WIC trajectory
table(LTM_final$WICANY)

r_svysummary(#by = "RACE", 
             include = "WIC_TREND")

# RESOURCE VALUE
r_svysummary(include = c('RESOURCEVALUE_A1', 'RESOURCEVALUE_A2', 'RESOURCEVALUE_A3',
                         'RESOURCEVALUE_A4', 'RESOURCEVALUE_A5', 'RESOURCEVALUE_A6',
                         'RESOURCEVALUE_A7', 'RESOURCEVALUE_A8', 'RESOURCEVALUE_A9',
                         'RESOURCEVALUE_A10','RESOURCEVALUE_A11','RESOURCEVALUE_A12'))


r_svysummary(include = c('RESOURCEVALUE2_A1', 'RESOURCEVALUE2_A2', 'RESOURCEVALUE2_A3',
                         'RESOURCEVALUE2_A4', 'RESOURCEVALUE2_A5', 'RESOURCEVALUE2_A6',
                         'RESOURCEVALUE2_A7', 'RESOURCEVALUE2_A8', 'RESOURCEVALUE2_A9',
                         'RESOURCEVALUE2_A10'))

r_svysummary(include = c('RESOURCETRUST_A1', 'RESOURCETRUST_A2', 'RESOURCETRUST_A3', 
                         'RESOURCETRUST_A4', 'RESOURCETRUST_A5', 'RESOURCETRUST_A6',
                         'RESOURCETRUST_A7', 'RESOURCETRUST_A8', 'RESOURCETRUST_A9',
                         'RESOURCETRUST_A10', 'RESOURCETRUST_A11', 'RESOURCETRUST_A12',
                         'RESOURCETRUST_A13', 'RESOURCETRUST_A14', 'RESOURCETRUST_A15', 
                         'RESOURCETRUST_A16', 'RESOURCETRUST_A17', 'RESOURCETRUST_A18',
                         'RESOURCETRUST_A19', 'RESOURCETRUST_A20'))


r_svysummary(include = "ANYCOVIDVAC")


