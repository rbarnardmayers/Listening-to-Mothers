setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/ApplyDictionary.R")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/Cleaning/Fig_Helpful_Functions.R")

# 3.1	GA vertical bars by gestational week or other viz to show how much we have deviated from a bell curve
LTM_dsn %>%
  tbl_svysummary(include = GESTAGE_R,
                 statistic = list(all_categorical() ~ "{n}"))

# 3.2	 GOLDENHOUR segmented bar graph for percentage with mom, spouse/partner, staff/routine, staff/special, I’m not sure,
fig_3_2 <- fig_compile("GOLDENHOUR", others = c("RACE", "INSURANCE", "MODE2023", "BIRTHATTEND", "LANGSIMP"))

# 3.3	SKIN: simple bar graph with percentage with mom/spouse/partner and skin-to-skin, 
fig_3_3 <- fig_compile("SKIN", others = c("RACE", "MODE2023", "BIRTHATTEND", "LANGSIMP")) 

# 3.4	 NICU: entire time, part time (segmented or stacked bars? Percentage of babies in NICU part-time or throughout with low-risk characteristics (Could be segmented or stacked (pairs) bars) 
fig_3_4 <- fig_compile("NICU", others = c("xBABYHOSP", "BW_CAT", "GESTAGE"))
# maybe no maternal complications,

# 3.5	 BIRTHATTEND: best to be parallel to 2.3 (prenatal provider: has doctor, midwife, other
fig_3_5 <- fig_compile("BIRTHATTEND2")

# 3.6	DOULA (at birth): Summative display of doula use across 3 phases of care and by combinations of phases


# 3.7	DOULA Doula use around time of birth, parallel to prenatal: whether had birth doula by race and ethnicity, type of insurance (11.5% = 11% or 12%?), midwife vs obstetrician birth attendant


# 3.8	Elective induction at term (INDUCE5 == 1): for whom is elective induction at term being recommended: INDUCE5=1 by race and ethnicity, private vs Medicaid insurance: given that vast majority of people with previous cesarean have repeats high rates of discussing induction because it's around term


# 3.9	Narrative could report whether recommendation differs by metro vs nonmetro residence, by age, education, first-time vs experienced


# 3.10	SDM in induction: to what extent are various subgroups experiencing SDM, by race and ethnicity, private vs Medicaid insurance
fig_3_10 <- fig_compile("SDM_dich")

# 3.11	SELFINDUCE: simple bar graph of the response choices in descending order of frequency with Overall throughline
# 3.12	MEDINDUCE1: bar graph of  response choices in descending order of frequency
# 3.13	MEDINDUCE3: simple bar graph of the response choices in descending order of frequency with Overall throughline
# 3.14	LABORPERMIT = 1 or 2: Among those who labored/MODE=1 or LABCSEC=1, whether had oral fluids/LABORPERMIT=1 or oral solids/LABORPERMIT=2 (need to creatively distinguish between no/not interested and no/interested but not allowed); Ambulation; walking; all restrictions among denominator - people who labored
# 3.15	POSITIONCHOICE: positions used with and without choice
# 3.16	DRUGFREE: simple bar graph showing use of non-pharm methods in descending order and those who used none. Among those who labored//MODE=1 or LABCSEC=1, percentage who did vs did not use drugfree methods
# 3.17	DRUGFREE: Consider comparing those who did/did not use drugfree methods and their attributes (e.g., RE, first-time vs experienced mom, physician vs midwife birth attendant, no doula vs doula)
# 3.18	PAINMEDS: simple bar graph showing use of pharm methods in descending order and those who used none with Overall throughline
# 3.19	Did/did not use pain meds comparison: Consider comparing those who did/did not use pain meds and their attributes (e.g., RE, first-time vs experienced mom, physician vs midwife birth attendant, no doula vs doula)
# 3.20	LABORINT: simple bar graph of use of these five interventions in descending order of frequency with Overall throughline
# 3.21	POSITION: simple bar graph of use of various positions in descending order of frequency with Overall throughline
# 3.22	EPISTCHOICE (no choice): Consider no choice in having epis or x-ref to that as restriction
# 3.23	Physiologic childbirth: those who met and did not meet criteria by midwife v physician (prenatal or intrapartum, whichever has greater contrast) and by birth doula vs no birth doula (noting that having both was most associated with meeting the definition in CA)
# 3.24	LTM cascade of interventions flowchart
# 3.25	Baby friendly steps: simple bar graph showing cumulative Baby-Friendly steps (leaving HOSPFEED = 8 aside) and likelihood of breastfeeding at 1 week, separating exclusive and mixed feeding
# 3.26	Respectful care: by race/ethnicity, private vs Medicaid payment
# 3.27	CULTURE: race and ethnicity [hypothesize greatest acknowledgment of cultural traditions and greatest failure to accommodate them among AIAN, then Black]
