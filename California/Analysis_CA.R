
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
r_svysummary(by = "DISABILITY",
             include = "VAGEXAM_5", 
             data = filter(CA_Q1_dsn, LABORINTC5 == 1))
