
# Postpartum visits. We would like to be able to report 
#  this data in different groupings that was provided in 
# the data compendium. We would like: 
# 0 visits, 1 visit, 2 visits, 3 or more visits.

# RACE INSURANCE PARITY DISABILITY
r_svysummary(by = "DISABILITY",
             include = "PPVISIT_CA", 
             data = CA_Q1_dsn)

# Centimeters dilated. 
# with dilation of 5cm or less 
# with dilation of 6cm or more.

# RACE INSURANCE PARITY DISABILITY
r_svysummary(#by = "DISABILITY",
             include = "VAGEXAM2", 
             data = filter(CA_Q1_dsn, 
                           LABORINTC5 == "You had one or more vaginal exams to check how much your cervix was open/dilated"))

# INSURANCE
r_svysummary(by = "INSURCAT", 
             include = "LEARNED2_R", 
             data = CA_Q1_dsn)

r_svysummary(by = "INSURCAT", 
             include = "LEARNED2_R", 
             data = filter(CA_Q1_dsn, STATE == "California"))

CA_Q1 %>% 
  subset(INSURC5O != "") %>%
  select(c(INSURC1, INSURC2,INSURC3,INSURC4,INSURC5,
           INSURC5O)) %>%
  View()

# LANGUAGES

r_svysummary(include =c("LANGSIMP2", "LANGHOMEC1","LANGHOMEC2"), 
             data = CA_Q1_dsn)
count_svysummary(by = "LANGHOMEC2", 
                 include = "LANGHOMEC1",
                 data = CA_Q1_dsn)

r_svysummary(include =c("LANGSIMP2", "LANGHOMEC1", "LANGHOMEC2"), 
             data = CA_Q1Q2_dsn)

count_svysummary(by = "LANGHOMEC2", 
                 include = "LANGHOMEC1",
                 data = CA_Q1Q2_dsn)
