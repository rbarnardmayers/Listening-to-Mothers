# Power Calc
LTM_final <- 
  LTM_final %>% mutate(FINALWT_2 = FINALWT^2)

DEFF = 3857 * (sum(LTM_final$FINALWT_2)/
             sum(LTM_final$FINALWT)^2) 
ESS = 3857 / DEFF

# Power calculation for sample size
# 1.282 is z score set at 80%
# 50% is the proportion for safest sample size
# second 50% is 1 - p
# 0.1 is the margin of error

nrsr = ((1.282^2)*.5*.5)/(0.1^2)

DEFF * nrsr

# 
SE_1 = sqrt(.5*.5/ESS)

MDC = 1.96 * SE_1
