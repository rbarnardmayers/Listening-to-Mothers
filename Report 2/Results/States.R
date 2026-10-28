source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Fig_Helpful_Functions.R")


r_svysummary(by = "PaidLeave", 
             include = c("EMPLOY", "PTLEAVE", "SELF_PT", "MATLEAVEC5"))


# 
# Also, it would be good to look at the PFML data and see
# Among those in PFML requirement states - what percentages of people took PFML leave
# Among those in NO required PFML – what percentages of people took PFML (that would be voluntarily provided by their companies).