setwd("/Users/rubybarnard-mayers/Documents/2025-2026/LTM/Listening-to-Mothers")
source("~/Documents/2025-2026/LTM/Listening-to-Mothers/ApplyDictionary.R")

# Data manip for figures ----
# PPVISIT categorized as 0, 1, 2, 3, 4+ 

LTM_final <- LTM_final %>% 
  mutate(INSURANCE = case_when(INSURANCE == "Medicaid/CHIP" ~ "Medicaid/CHIP", 
                               INSURANCE == "Private" ~ "Private",
                               INSURANCE == "None" ~ "None", 
                               INSURANCE == "Missing" ~ NA, 
                               TRUE ~ "Other"),
         PROVIDER = case_when(PROVIDER == "An obstetrician-gynecologist doctor (could be called OB, OBGYN, or maternal-fetal medicine specialist)" ~ "Any Doctor", 
                              PROVIDER == "A midwife (could be called CNM)" ~ "Midwife", 
                              PROVIDER == "A family medicine doctor (could be called FP)" ~ "Any Doctor", 
                              PROVIDER == "A doctor but I'm not sure what kind" ~ "Any Doctor",
                              PROVIDER == "Missing" | PROVIDER == "I'd prefer not to answer" | is.na(PROVIDER) ~ "Missing",
                              TRUE ~ "Other"),
         PROVIDERCHOICE = case_when(PRENAT == "No Prenatal Care" ~ "No Prenatal Care", 
                                    PROVIDERCHOICE == "No, I had no choice; my maternity care provider was assigned to me" ~ "No Choice",
                                    PROVIDERCHOICE %in% c("Missing", "I'd prefer not to answer") ~ "Missing",
                                    TRUE ~ "Yes"),
         PROVIDERCHOICE = factor(PROVIDERCHOICE, levels = c("Yes", "No Choice", "No Prenatal Care", "Missing")),
         PPVISIT = case_when(PPVISIT == 0 ~ 0, 
                             PPVISIT == 1 ~ 1, 
                             PPVISIT == 2 ~ 2, 
                             PPVISIT == 3 ~ 3, 
                             PPVISIT >= 4 ~ 4))

LTM_final$PPBOTHER_A1 <- recode.phq("PPBOTHER_A1")
LTM_final$PPBOTHER_A2 <- recode.phq("PPBOTHER_A2")
LTM_final$PPBOTHER_A3 <- recode.phq("PPBOTHER_A3")
LTM_final$PPBOTHER_A4 <- recode.phq("PPBOTHER_A4")

LTM_final <- LTM_final %>% 
  mutate(PPBOTHER_A1 = as.numeric(PPBOTHER_A1), 
         PPBOTHER_A2 = as.numeric(PPBOTHER_A2),
         PPBOTHER_A3 = as.numeric(PPBOTHER_A3),
         PPBOTHER_A4 = as.numeric(PPBOTHER_A4))
LTM_final$PHQ2 = LTM_final$PPBOTHER_A3 + LTM_final$PPBOTHER_A4
LTM_final$GAD2 = LTM_final$PPBOTHER_A1 + LTM_final$PPBOTHER_A2
LTM_final$PHQ4 = LTM_final$PPBOTHER_A1 + LTM_final$PPBOTHER_A2 + LTM_final$PPBOTHER_A3 + LTM_final$PPBOTHER_A4

LTM_final <- LTM_final %>% 
  mutate(PHQ2_cat = case_when(PHQ2 < 3 ~ "No", 
                              PHQ2 >= 3 ~ "Yes"), 
         GAD2_cat = case_when(GAD2 < 3 ~ "No", 
                              GAD2 >= 3 ~ "Yes"), 
         PHQ4_cat = case_when(PHQ4 >= 0 & PHQ4 <= 2 ~ "None",
                              PHQ4 > 2 & PHQ4 <= 5 ~ "Mild",
                              PHQ4 > 5 & PHQ4 <= 8 ~ "Moderate", 
                              PHQ4 > 8 ~ "Severe"))
LTM_final <- LTM_final %>% 
  mutate(RACE2 = case_when(RACE == "NH Asian" ~ "NH_Asian",
                           RACE == "NH Black or African American" ~ "NHB", 
                           TRUE ~ RACE))
# Subscale respectful care ----
LTM_final$RESPECT <- likert('RESPECT')
LTM_final$KNOWLEDGE <- likert('KNOWLEDGE')
LTM_final$HEARD <- likert('HEARD')
LTM_final$DECISIONS <- likert('DECISIONS')
LTM_final$CONSENT <- likert('CONSENT')
LTM_final$INFORMED <- likert('INFORMED')
LTM_final$TIMELINESS <- likert('TIMELINESS')
LTM_final$NEGLECT <- likert('NEGLECT')
LTM_final$TRUST <- likert('TRUST')
LTM_final$FEEDING <- likert('FEEDING')
LTM_final$SAFE <- likert('SAFE')
LTM_final$DISCRIMINATION <- likert('DISCRIMINATION')
 
# Create survey design ----
LTM_dsn <- LTM_final %>% as_survey(weight = wght, id = 1)

# Tables as excel sheets

# Label by figure, each figure as a tab
# Excel sheet of number
# No % or $ signs
# Order for graphics to appear
# example of figures in ppt  

pinks <- c("#e89a96", "#f3cccb", "#ebaba9", "#f9f3f4", "#efb9b7")
greys <- c("#616366","#c1c3c5", "#c0c3c4","#edeced", "#e5e6e7")


# CHAPTER 1 ----
# Provider Type ----
# Percent of racial distribution for each provider
fig_1_1 <- print.fig(PROVIDER)

# get % of race for each provider type 
fig_1_2 <- fig.2by2(PROVIDER, RACE) %>% as.data.frame()

fig_1_3 <- fig.2by2(PROVIDER, INSURANCE) %>% as.data.frame()

# Provider Choice ----
fig_1_4 <- print.fig(PROVIDERCHOICE)

# Across each answer for provider choice, distribution of race
fig_1_5 <- fig.2by2(PROVIDERCHOICE, RACE)

# Across each answer for provider choice, distribution of insurance
fig_1_6 <- fig.2by2(PROVIDERCHOICE, INSURANCE)



# Doula during pregnancy ----
# DOULAC1	== 1 
fig_1_7 <- print.fig(DOULAC1)

# Across each answer for doula, distribution of race
fig_1_8 <- fig.2by2(DOULAC1, RACE)

# Across each answer for doula, distribution of insurance
fig_1_9 <- fig.2by2(DOULAC1, INSURANCE)

# First visit ----
# FIRSTVISIT 1 == YES, 2 == NO
fig_1_10 <- print.fig(FIRSTVISIT)

# Across each answer for doula, distribution of race
fig_1_11 <- fig.2by2(FIRSTVISIT, RACE)

# Across each answer for doula, distribution of insurance
fig_1_12 <- fig.2by2(FIRSTVISIT, INSURANCE)

# Reason for no prenatal ----
fig_1_13 <- collapse.fun(c('NOPRENATALC1','NOPRENATALC2','NOPRENATALC3','NOPRENATALC4',
                           'NOPRENATALC5','NOPRENATALC6','NOPRENATALC7','NOPRENATALC8',
                           'NOPRENATALC9','NOPRENATALC10','NOPRENATALC11','NOPRENATALC12',
                           'NOPRENATALC13','NOPRENATALC14'))


# NOPRENATALC1 1 = The earliest available appointment was later than I wanted
# NOPRENATALC2 2 = The maternity care provider wouldn’t see me until my pregnancy was further along
# NOPRENATALC3 3 = I was not yet enrolled in Medicaid (MediCal, MassHealth, etc.)
# NOPRENATALC4 4 = I was not yet enrolled in private insurance (such as through employer or the Marketplace)
# NOPRENATALC5 5 = It took a while to find a place that was accepting Medicaid
# NOPRENATALC6 6 = It took a while to find a place that was accepting new patients
# NOPRENATALC7 7 = It took a while to find a place where I wanted to get care
# NOPRENATALC8 8 = I couldn’t take time off from my paid job or school
# NOPRENATALC9 9 = I didn’t have transportation to get to the clinic or medical office
# NOPRENATALC10 10 = I was told I was high-risk and was not accepted as a patient
# NOPRENATALC11 11 = I was concerned about feeling unsafe or disrespected
# NOPRENATALC12 12 = I was told I was too late or too far along in my pregnancy
# NOPRENATALC13 95 = Other, please specify
# NOPRENATALC14 99 = I’d prefer not to answer

# Care setting ----
# CARESETTINGC1	 1 = Hospital
# CARESETTINGC2	 2 = Private medical office
# CARESETTINGC3	 3 = Community health center
# CARESETTINGC4	 4 = County or local health department clinic
# CARESETTINGC5	 5 = Planned Parenthood health center
# CARESETTINGC6	 6 = Mobile clinic
# CARESETTINGC7	 7 = Birth center (not in a hospital)
# CARESETTINGC8	 95 = Other, please specify
# CARESETTINGC8O	 95 = Other, please specify
# CARESETTINGC9	 99 = I’d prefer not to answer

fig_1_14 <- collapse.fun(c('CARESETTINGC1','CARESETTINGC2','CARESETTINGC3','CARESETTINGC4',
                           'CARESETTINGC5','CARESETTINGC6','CARESETTINGC7','CARESETTINGC8',
                           'CARESETTINGC9'))

# CARETYPE1 ----
# CARETYPE1	Did you have a choice about whether to have office prenatal visits by yourself or group prenatal visits?
fig_1_15 <- print.fig(CARETYPE1)

fig_1_16 <- fig.2by2(CARETYPE1, RACE)
fig_1_17 <- fig.2by2(CARETYPE1, INSURANCE)

# CARETYPEPREF ----
# CARETYPEPREF	Which type of prenatal care do you prefer?
# 1	Prenatal visits by myself
# 2	Group prenatal visits
# 3	I like a mix of both
# 99	I’d prefer not to answer

fig_1_18 <- print.fig(CARETYPEPREF)

fig_1_19 <- fig.2by2(CARETYPEPREF, RACE)

fig_1_20 <- fig.2by2(CARETYPEPREF, INSURANCE)

# WHYTELE ----
# WHYTELEC1	  1 = Transportation to in-person care was a challenge
# WHYTELEC2	  2 = I wanted to save travel time
# WHYTELEC3	  3 = I had childcare responsibilities
# WHYTELEC4	  4 = I wanted to avoid exposure to COVID-19
# WHYTELEC5	  5 = I was more comfortable with remote than office visits
# WHYTELEC6	  6 = My maternity care provider preferred televisits
# WHYTELEC7	  95 = Other, please specify

fig_1_21 <- collapse.fun(c('WHYTELEC1','WHYTELEC2','WHYTELEC2','WHYTELEC4',
                           'WHYTELEC5','WHYTELEC6','WHYTELEC7'))

# BPCONFID+URINECONFID+WEIGHCONFID+BABYHRCONFID ----
# 1	Yes, fully confident
# 2	Yes, somewhat confident
# 3	No, not confident
# 99	I’d prefer not to answer


fig_1_22 <- print.fig(BPCONFID)

# CAREMODEPREF ----
#   CAREMODEPREF	Which mode of prenatal care do you prefer?
# 1	In-person visits
# 2	Televisits
# 3	I like a mix of both
# 99	I’d prefer not to answer

fig_1_23 <- print.fig(CAREMODEPREF)

fig_1_24 <- fig.2by2(CAREMODEPREF, RACE)

# fig_1_25 <- fig.2by2(CAREMODEPREF, RURALURBAN)

# EDUIMPACT ----
# EDUCIMPACTC1 1 = Helped me understand what would happen at the hospital
# EDUCIMPACTC2 2 = Helped me understand bodily childbirth processes
# EDUCIMPACTC3 3 = Helped me feel more confident about giving birth
# EDUCIMPACTC4 4 = Caused me to feel more concerned about giving birth
# EDUCIMPACTC5 5 = Helped my spouse/partner understand what I would experience
# EDUCIMPACTC6 6 = Had no effect
# EDUCIMPACTC7 95 = Something else, please specify
# EDUCIMPACTC8 99 = I’d prefer not to answer
fig_1_26 <- collapse.fun(c('EDUCIMPACTC1','EDUCIMPACTC2','EDUCIMPACTC3',
                           'EDUCIMPACTC4','EDUCIMPACTC5','EDUCIMPACTC6',
                           'EDUCIMPACTC7'))

# PREGCONDITION ----
# PREGCONDITIONC1	1 = Gestational diabetes (high blood sugar that comes on during pregnancy)
# PREGCONDITIONC2	2 = Gestational hypertension/pregnancy-induced hypertension or PIH (high blood pressure that comes on during pregnancy)
# PREGCONDITIONC3	3 = Anemia (lower-than-normal amount of red blood cells or related conditions)
# PREGCONDITIONC4	4 = Pre-eclampsia
# PREGCONDITIONC5	5 = HELLP (Hemolysis, Elevated Liver enzymes and Low Platelets) syndrome
# PREGCONDITIONC6	6 = Blood clotting disorders (low platelets, blood clot/embolism, etc.)
# PREGCONDITIONC7	7 = Placenta previa
# PREGCONDITIONC8	8 = Hyperemesis gravidarum (severe nausea and vomiting)
# PREGCONDITIONC9	95 = Other pregnancy conditions, please specify
# PREGCONDITIONC9O	95 = Other pregnancy conditions, please specify
# PREGCONDITIONC10	97 = No, none of the above
# PREGCONDITIONC11	99 = I’d prefer not to answer
fig_1_27 <-collapse.fun(c('PREGCONDITIONC1','PREGCONDITIONC2','PREGCONDITIONC3',
                          'PREGCONDITIONC4','PREGCONDITIONC5','PREGCONDITIONC6',
                          'PREGCONDITIONC7','PREGCONDITIONC8','PREGCONDITIONC9',
                          'PREGCONDITIONC10'))

# fig_1_28 <- race/ethnicioty
# fig_1_29 <- insurance

# Depression ----
# PREPREG_MHCONDC1

fig_1_30 <- print.fig(PREPREG_MHCONDC1)

fig_1_31 <- fig.2by2(PREPREG_MHCONDC1, RACE)

# fig_1_32 <- fig.2by2(PREPREG_MHCONDC1, RURALURBAN)

# Anxiety ----
# PREPREG_MHCONDC2

fig_1_33 <- print.fig(PREPREG_MHCONDC1)

fig_1_34 <- fig.2by2(PREPREG_MHCONDC1, RACE)

# fig_1_35 <- fig.2by2(PREPREG_MHCONDC1, RURALURBAN)

# MENTALSUPPORT ----
# MENTALSUPPORT

fig_1_36 <- print.fig(MENTALSUPPORT)

fig_1_37 <- fig.2by2(MENTALSUPPORT, RACE)

# fig_1_38 <- fig.2by2(MENTALSUPPORT, RURALURBAN)

# Social needs ----
# SOCIALNEEDC1 1 = You or others you lived with ate smaller meals or skipped meals because you didn’t have enough money for food
# SOCIALNEEDC2 2 = You were homeless or worried you might become homeless in the future
# SOCIALNEEDC3 3 = You had trouble paying for your utilities (gas, electric, phone)
# SOCIALNEEDC4 4 = You had trouble finding or paying for a ride/transportation
# SOCIALNEEDC5 5 = You needed daycare, or better daycare, for your kids
# SOCIALNEEDC6 6 = You were unemployed or without regular income
# SOCIALNEEDC7 7 = You were concerned about someone in your home using drugs or alcohol
# SOCIALNEEDC8 8 = You felt unsafe in your daily life
# SOCIALNEEDC9 9 = Someone in your home was threatening or abusing you
# SOCIALNEEDC10 97 = None of the above
# SOCIALNEEDC11 99 = I’d prefer not to answer
fig_1_39 <- collapse.fun(c('SOCIALNEEDC1','SOCIALNEEDC2','SOCIALNEEDC3',
                           'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                           'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                           'SOCIALNEEDC10','SOCIALNEEDC11'))

fig_1_40 <- collapse.2by2(c('SOCIALNEEDC1','SOCIALNEEDC2','SOCIALNEEDC3',
                            'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                            'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                            'SOCIALNEEDC10','SOCIALNEEDC11'), "RACE")

fig_1_41 <- collapse.2by2(c('SOCIALNEEDC1','SOCIALNEEDC2','SOCIALNEEDC3',
                            'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
                            'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
                            'SOCIALNEEDC10','SOCIALNEEDC11'), "INSURANCE")

# EMPLOYBEN ----
# EMPLOYBEN
fig_1_42 <- print.fig(EMPLOYBEN)

fig_1_43 <- fig.2by2(EMPLOYBEN, RACE)

# fig_1_44 <- fig.2by2(EMPLOYBEN, RURALURBAN)

# BIGBABY1 == 1 ----
# BIGBABY1
fig_1_45 <- print.fig(BIGBABY1)

fig_1_46 <- fig.2by2(BIGBABY1, MODE2023)

fig_1_47 <- fig.2by2(BIGBABY1, MEDINDUCE)

# fig_1_48 <- fig.2by2(BIGBABY1, BIRTHWEIGHT)

# PLANNEDFEED ----
# PLANNEDFEEDC1	 1 = Breast milk
# PLANNEDFEEDC2	 2 = Formula
# PLANNEDFEEDC3	 98 = Unsure
# PLANNEDFEEDC4	 99 = I’d prefer not to answer
fig_1_49 <- collapse.fun(c('PLANNEDFEEDC1','PLANNEDFEEDC2',
                           'PLANNEDFEEDC3','PLANNEDFEEDC4'))


fig_1_50 <- collapse.2by2(c('PLANNEDFEEDC1','PLANNEDFEEDC2','PLANNEDFEEDC3'), "RACE")
fig_1_51 <- collapse.2by2(c('PLANNEDFEEDC1','PLANNEDFEEDC2','PLANNEDFEEDC3'), "INSURANCE")

# Preg weight gain ----
# PREGWEIGHT - PREPREGWEIGHT
# fig_1_52 <- mean diff
# fig_1_53 <- mean diff by BMI

# CHAPTER 2 ####
# DUE DATE & BIRTHDATE ----
# DUEDATE 
# BIRTHDATE
fig_2_1 <- print.fig(DUEDATE)
fig_2_2 <- print.fig(DUEDATE)

# GOLDENHOUR ----
fig_2_3 <- print.fig(GOLDENHOUR)

fig_2_4 <- fig.2by2(GOLDENHOUR, RACE)

fig_2_5 <- fig.2by2(GOLDENHOUR, INSURANCE)

fig_2_6 <- fig.2by2(GOLDENHOUR, MODE2023)

# SKIN ----
fig_2_7 <- print.fig(SKIN)

fig_2_8 <- fig.2by2(SKIN, RACE)

fig_2_9 <- fig.2by2(SKIN, MODE2023)

# BIRTHATTEND ----
fig_2_10 <- print.fig(BIRTHATTEND)

fig_2_11 <- fig.2by2(BIRTHATTEND, RACE)

fig_2_12 <- fig.2by2(BIRTHATTEND, MODE2023)


# BIRTH DOULA ----
# DOULAC2
fig_2_13 <- print.fig(DOULAC2)

fig_2_14 <- fig.2by2(DOULAC2, RACE)

fig_2_15 <- fig.2by2(DOULAC2, INSURANCE)

# SDM with induction ----
# INDUCE1	How much did your maternity care provider talk about inducing your labor for no other reason except because it was around your due date (or full term or the 39th week of pregnancy)? 
# INDUCE2	How much did your maternity care provider talk about not inducing your labor for no other reason except because it was around your due date (or full term or the 39th week of pregnancy)?
# INDUCE3	Did your maternity care provider talk about waiting for labor to begin on its own as something that you should seriously consider?
# INDUCE4	Did your maternity care provider ask if you wanted your labor induced for no other reason except because it was around your due date (or full term or the 39th week of pregnancy)?

# 1	A lot - score of 0
# 2	Some - score of 1
# 3	A little - score of 2
# 4	Not at all - score of 3
# 99	I’d prefer not to answer - score of NA

# fig_2_16
# fig_2_17 race
# fig_2_18 insurance

# INDUCE5 by SDM induction ----
# fig_2_19 

# 1	Yes, my provider thought my labor should be induced
# 2	Yes, my provider thought my labor should not be induced
# 3	No, my provider did not make a recommendation
# 99	I’d prefer not to answer

# SELFINDUCE ----
# SELFINDUCE	Did you try to self-induce your labor before [CHILDNAME]'s birth? That is, did you do anything to try to cause your labor to begin before it started on its own?
# SELFINDUCE1	Did your self-induction start your labor?
fig_2_20 <- print.fig(SELFINDUCE)


# Methods in MEDINDUCE =1 ----
# MEDINDUCE	Did your maternity care provider try to induce your labor in any way? That is, use medicine or some other method to try to start regular labor contractions – before they started on their own?
# MEDINDUCE1C1	  1 = Break your bag of water with a small tool like a crochet hook (before labor)
# MEDINDUCE1C2	  2 = Insert a finger into your cervix to “sweep” or “strip” the membranes loose
# MEDINDUCE1C3	  3 = Put an inflatable bulb in your cervix (Foley balloon)
# MEDINDUCE1C4	  4 = Give you Pitocin (“pit” or synthetic oxytocin) through an intravenous (IV) line (before labor)
# MEDINDUCE1C5	  5 = Place medicine (gel, pouch, tablet) near your cervix
# MEDINDUCE1C6	  6 = Give you a tablet by mouth
# MEDINDUCE1C7	  7 = Direct you to try non-medical approaches (for example walking, castor oil, etc)
# MEDINDUCE1C8	  98 = I’m not sure
# MEDINDUCE1C9	  99 = I’d prefer not to answer
fig_2_21 <- collapse.fun(c('MEDINDUCE1C1','MEDINDUCE1C2','MEDINDUCE1C3',
                           'MEDINDUCE1C4','MEDINDUCE1C5','MEDINDUCE1C6',
                           'MEDINDUCE1C7','MEDINDUCE1C8','MEDINDUCE1C9'))

# MEDINDUE3 | MEDINDUCE = 1 ----
# MEDINDUCE3C1	 1 = There was a concern about my health
# MEDINDUCE3C2	 2 = There was a concern about the baby’s health
# MEDINDUCE3C3	 3 = It was around my due date (or full term or pregnancy week 39)
# MEDINDUCE3C4	 4 = My maternity care provider was concerned that the baby was too big
# MEDINDUCE3C5	 5 = I wanted to get the pregnancy over with
# MEDINDUCE3C6	 6 = I wanted to give birth with a specific provider
# MEDINDUCE3C7	 7 = I wanted to control the timing of my birth for other reasons (such as a job or visiting family)
# MEDINDUCE3C8	 95 = Other, please specify
# MEDINDUCE3C9	 99 = I’d prefer not to answer
fig_2_22 <- collapse.fun(c('MEDINDUCE3C1','MEDINDUCE3C2','MEDINDUCE3C3',
                           'MEDINDUCE3C4','MEDINDUCE3C5','MEDINDUCE3C6',
                           'MEDINDUCE3C7','MEDINDUCE3C8'),#'MEDINDUCE3C9'), 
                         data = subset(LTM_dsn, MEDINDUCE1C1 == "Yes"))

# Position | Mode = 1 ----
# What was the main position you used while pushing before the birth of your baby?
# 1	Lying on my back
# 2	With my back propped up (semi-sitting)
# 3	Upright (for example, squatting or sitting)
# 4	Lying on my side
# 5	On my hands and knees
# 95	Other, please specify
# 99	I’d prefer not to answer


# POSITIONCHOICE
# fig_2_23 <- fig.print(POSITION, data = subset(LTM_dsn, MODE == 1))
# fig_2_24 <- fig.2by2(POSITION, POSITIONCHOICE, data = subset(LTM_dsn, MODE == 1))

# Drugfree ----
# DRUGFREEC1	 1 = Sitting or soaking in a tub or pool
# DRUGFREEC2	 2 = Shower
# DRUGFREEC3	 3 = Use of a large inflated “birth ball” or “peanut ball”
# DRUGFREEC4	 4 = Hot or cold objects (such as a heating pad or ice pack) applied to my body
# DRUGFREEC5	 5 = Position changes and/or movement
# DRUGFREEC6	 6 = Mental methods (such as meditation, visualization, hypnosis, etc.)
# DRUGFREEC7	 7 = Hands-on methods (such as massages, stroking, acupressure, etc.)
# DRUGFREEC8	 8 = Breathing methods
# DRUGFREEC9	 9 = Prayer
# DRUGFREEC10	 95 = Some other method, please specify
# DRUGFREEC11	 97 = I did not use any drug-free pain relief methods while giving birth
# DRUGFREEC12	 99 = I’d prefer not to answer
fig_2_25 <- collapse.fun(c("DRUGFREEC1","DRUGFREEC2","DRUGFREEC3",
                           "DRUGFREEC4","DRUGFREEC5","DRUGFREEC6",
                           "DRUGFREEC7","DRUGFREEC8","DRUGFREEC9",
                           "DRUGFREEC10","DRUGFREEC11","DRUGFREEC12"))
fig_2_26 <- collapse.2by2(c("DRUGFREEC1","DRUGFREEC2","DRUGFREEC3",
                            "DRUGFREEC4","DRUGFREEC5","DRUGFREEC6",
                            "DRUGFREEC7","DRUGFREEC8","DRUGFREEC9",
                            "DRUGFREEC10","DRUGFREEC11","DRUGFREEC12"), "PARITY")
fig_2_27 <- collapse.2by2(c("DRUGFREEC1","DRUGFREEC2","DRUGFREEC3",
                             "DRUGFREEC4","DRUGFREEC5","DRUGFREEC6",
                             "DRUGFREEC7","DRUGFREEC8","DRUGFREEC9",
                             "DRUGFREEC10","DRUGFREEC11","DRUGFREEC12"), "BIRTHATTEND")
fig_2_28 <- collapse.2by2(c("DRUGFREEC1","DRUGFREEC2","DRUGFREEC3",
                            "DRUGFREEC4","DRUGFREEC5","DRUGFREEC6",
                            "DRUGFREEC7","DRUGFREEC8","DRUGFREEC9",
                            "DRUGFREEC10","DRUGFREEC11","DRUGFREEC12"), "DOULA")

# Painmeds ----
# PAINMEDSC1 1 = Epidural or spinal (medicine delivered into the spinal column)
# PAINMEDSC2 2 = Nitrous oxide gas (not oxygen) breathed through a mask or mouthpiece while remaining conscious
# PAINMEDSC3 3 = Narcotics (such as Demerol or Stadol), by intravenous (IV) line or spray in your nose
# PAINMEDSC4 4 = General anesthesia (you were “put to sleep” – no sensation, no conscious)
# PAINMEDSC5 5 = Pudendal or other local blocks (injections into the vagina or cervix)
# PAINMEDSC6 98 = Used pain medicine but not sure what
# PAINMEDSC7 97 = I did not use any pain medicine while giving birth
# PAINMEDSC8 99 = I’d prefer not to answer
fig_2_29 <- collapse.fun(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
                           "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
                           "PAINMEDSC7","PAINMEDSC8"))
fig_2_30 <- collapse.2by2(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
                            "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
                            "PAINMEDSC7","PAINMEDSC8"), "PARITY") #by parity
fig_2_31 <-collapse.2by2(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
                           "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
                           "PAINMEDSC7","PAINMEDSC8"), "BIRTHATTEND") #attendante
fig_2_32 <-collapse.2by2(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
                           "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
                           "PAINMEDSC7","PAINMEDSC8"), "DOULA") #doula

# laborint 1 - 4 ----
# LABORINTC1 1 = A care team member broke your bag of water after labor contractions had begun
# LABORINTC2 2 = You got intravenous (IV) fluids through a vein in your arm or hand
# LABORINTC3 3 = You were given the drug Pitocin (“Pit” or synthetic oxytocin) to speed up labor after labor contractions had begun
# LABORINTC4 4 = A tube (catheter) was inserted into your bladder to remove urine

fig_2_33 <- collapse.fun(c('LABORINTC1','LABORINTC2','LABORINTC3','LABORINTC4'))

# position ----
# POSITION
fig_2_34 <- print.fig(POSITION)

# psych childbirth ----
# Spontaneous labor and birth at term without the use 
# of pharmacologic or mechanical interventions for 
# labor stimulation or pain management throughout labor and birth 

# 35 psych child birth
# 36 attendant
# 37 doula

# None of the following: 
# Opiates or nitrous oxide - PAINMEDSC3 & PAINMEDSC2 & PAINMEDSC1
# Augmentation of labor - MEDINDUCE -- i think? 
# Regional anesthesia analgesia except for the purpose of 
##  spontaneous laceration repair - PAINMEDSC4 & PAINMEDSC5
# Artificial rupture of membranes - MEDINDUCE
# Episiotomy -- EPIST 

# childbirth interventions ----
# (LTM-CA Figure 27), have created flow chart 
# (LTM-CA Figure 28 Cascade of Intervention), 
# and have enumerated in a table (LTM-CA Table 3) 
# fig_2_38 <- Overall 

# hospfeed ----
# Among those who planned partial or exclusive BF/PLANNEDFEED = 1, 
# whether nurses/other staff aligned with Baby-Friendly “steps” 
# that women could reasonably know/HOSPFEED = 1-7, 9-10
fig_2_39 <- collapse.fun(c('HOSPFEEDC1','HOSPFEEDC2','HOSPFEEDC3','HOSPFEEDC4',
                           'HOSPFEEDC5','HOSPFEEDC6','HOSPFEEDC7',
                           'HOSPFEEDC9','HOSPFEEDC10'))

# Subscale respectful care ----
# Respect care: developer’s 0-100 scoring guidance
# fig_2_40 <- scores overall
# fig_2_41 <- race/ethnicity
# fig_2_42 <- insurance

# culture ----
# Not sure about this 
# fig_2_43 <- overall
# fig_2_44 <- race

# CHAPTER 3 ####
# Mode of Birth ----
# MODE2023
fig_3_1 <- print.fig(MODE2023)
fig_3_2 <- fig.2by2(MODE2023, RACE)
fig_3_3 <- fig.2by2(MODE2023, INSURANCE)

# Unplanned Reason | CSECTIONTYPE = 2 ----
# CSECTIONTYPE == 2 
fig_3_4 <- print.fig(UNPLANNEDREASON, data = subset(LTM_dsn, CSECTIONTYPE == "Unplanned"))

# UNPLANNEDREASON
# 1	Labor was taking too long
# 2	The baby was not doing well in labor
# 3	Because of my health problem, the baby needed to be born soon
# 95	Some other medical reason, please specify
# 98	There was no medical reason
# 99	I’d prefer not to answer

# Planned C reason ----
# PLANNEDC
fig_3_5 <- print.fig(PLANNEDC, 
                     data = subset(LTM_dsn, 
                                   CSECTIONTYPE == "Planned ahead of time and scheduled before you went into labor"))

# 1	I had a prior cesarean birth
# 2	My baby was not doing well and needed to be born soon
# 3	Because of my health problem, my baby needed to be born soon
# 4	My baby was in the wrong position
# 5	My maternity care provider was concerned that the baby was too big
# 6	I was encouraged to plan a cesarean due to my larger size
# 7	There was a problem with the placenta
# 8	It was past my due date
# 95	Some other reason, please specify
# 97	There was no medical reason
# 99	I’d prefer not to answer

# VBAC Rate ----
# MODE2023 == 1 & MODE, MODE2 - MODE15 == 2
fig_3_6 <- print.fig(MODE2023)

# Reason for Repeat CS ----
fig_3_7 <- print.fig(PLANNEDC)
fig_3_8 <- fig.2by2(PLANNEDC, RACE)
fig_3_9 <- fig.2by2(PLANNEDC, INSURANCE)

# CHAPTER 4 ####
# DOULA ----
# DOULAC3  == 1
fig_4_1 <- print.fig(DOULAC3) # what subset? 

# PPVISIT ----
# PPVISIT categorized as 0, 1, 2, 3, 4+ 
fig_4_2 <- print.fig(PPVISIT)
fig_4_3 <- fig.2by2(PPVISIT, RACE)
fig_4_4 <- fig.2by2(PPVISIT, INSURANCE)

# PLANNEDFEED vs. FEED1WEEK ----

fig_4_5 <- collapse.fun(c("PLANNEDFEEDC1","PLANNEDFEEDC2","PLANNEDFEEDC3",
                          "PLANNEDFEEDC4"))
fig_4_6 <- collapse.fun(c("FEED1WEEKC1","FEED1WEEKC2","FEED1WEEKC3"))

# fig_4_7 <- race
# fig_4_8 <- insurance
# fig_4_9 <- birth attendant
# PLANNEDFEEDC1
# PLANNEDFEEDC2
# PLANNEDFEEDC3
# PLANNEDFEEDC4

# FEED1WEEKC1 1 = Breast milk
# FEED1WEEKC2 2 = Formula
# FEED1WEEKC3 99 = I’d prefer not to answer

# HOSPFEED ----
# HOSPFEEDC1 1 = Help you get started with breastfeeding when you and your baby were ready
# HOSPFEEDC2 2 = Encourage you to feed whenever your baby was interested (“on demand”)
# HOSPFEEDC3 3 = Show you how to position your baby for a good latch
# HOSPFEEDC4 4 = Give you any free formula samples, coupons, or other offers
# HOSPFEEDC5 5 = Tell you about breastfeeding support resources in the community
# HOSPFEEDC6 6 = Give your baby a pacifier
# HOSPFEEDC7 7 = Provide your baby with formula or water to supplement your breast milk
# HOSPFEEDC8 8 = Give your baby formula without your permission
# HOSPFEEDC9 9 = Provide guidance about pumping and storing your milk
# HOSPFEEDC10 10 = Connect you with a lactation specialist
# HOSPFEEDC11 97 = None of these
# HOSPFEEDC12 99 = I’d prefer not to answer

fig_4_10 <- collapse.fun(c("HOSPFEEDC1","HOSPFEEDC2","HOSPFEEDC3",
                           "HOSPFEEDC4","HOSPFEEDC5","HOSPFEEDC6",
                           "HOSPFEEDC7","HOSPFEEDC8","HOSPFEEDC9",
                           "HOSPFEEDC10","HOSPFEEDC11","HOSPFEEDC12"))
fig_4_11 <- collapse.2by2(c("HOSPFEEDC1","HOSPFEEDC2","HOSPFEEDC3",
                            "HOSPFEEDC4","HOSPFEEDC5","HOSPFEEDC6",
                            "HOSPFEEDC7","HOSPFEEDC8","HOSPFEEDC9",
                            "HOSPFEEDC10","HOSPFEEDC11","HOSPFEEDC12"), 
                          "BFGOAL") # BF GOALS

# EXCLBFGOAL ----
# EXCLBFGOAL
fig_4_12 <- print.fig(EXCLBFGOAL)
fig_4_13 <- fig.2by2(EXCLBFGOAL, RACE)
fig_4_14 <- fig.2by2(EXCLBFGOAL, PARITY)

# BFGOAL ----
fig_4_15 <- print.fig(BFGOAL)
fig_4_16 <- fig.2by2(BFGOAL, RACE)
fig_4_17 <- fig.2by2(BFGOAL, PARITY)

# PPBOTHER as PHQ2 ----
# PPBOTHER_A3 Having little interest or pleasure in doing things
# PPBOTHER_A4 Feeling down, depressed, or hopeless
# 0 - not at all
# > 3 

fig_4_18 <- print.fig(PHQ2_cat)
fig_4_19 <- fig.2by2(PHQ2_cat, RACE)
fig_4_20 <- fig.2by2(PHQ2_cat, INSURANCE)

# PPBOTHER as GAD2 ----
# 0 - not at all
# > 3 
# PPBOTHER_A1 Feeling nervous, anxious, or on edge
# PPBOTHER_A2 Not being able to stop or control worrying

fig_4_21 <- print.fig(GAD2_cat)
fig_4_22 <- fig.2by2(GAD2_cat, RACE)
fig_4_23 <- fig.2by2(GAD2_cat, INSURANCE)

# PPBOTHER as PHQ4 ----
# 0 - not at all

# 0-2: No significant symptoms of anxiety or depression
# 3-5: Mild anxiety or depression
# 6-8: Moderate anxiety or depression
# 9-12: Severe anxiety or depressio

# PPBOTHER_A1 Feeling nervous, anxious, or on edge
# PPBOTHER_A2 Not being able to stop or control worrying
# PPBOTHER_A3 Having little interest or pleasure in doing things
# PPBOTHER_A4 Feeling down, depressed, or hopeless
fig_4_24 <- print.fig(PHQ4_cat)
fig_4_25 <- fig.2by2(PHQ4_cat, RACE)
fig_4_26 <- fig.2by2(PHQ4_cat, INSURANCE)

# PPMEDS ----
# PPMEDSC1 1 = Yes, for anxiety
# PPMEDSC2 2 = Yes, for depression
# PPMEDSC3 3 = Yes, for substance use disorder
# PPMEDSC4 95 = Yes, for something else, please specify
# PPMEDSC4O 95 = Yes, for something else, please specify
# PPMEDSC5 97 = No
# PPMEDSC6 99 = I’d prefer not to answer
fig_4_27 <- collapse.fun(c('PPMEDSC1', 'PPMEDSC2', 'PPMEDSC3',
                           'PPMEDSC4','PPMEDSC5','PPMEDSC6'))
fig_4_28 <- collapse.2by2(c('PPMEDSC1', 'PPMEDSC2', 'PPMEDSC3',
                            'PPMEDSC4','PPMEDSC5','PPMEDSC6') ,"RACE")
fig_4_29 <- collapse.2by2(c('PPMEDSC1', 'PPMEDSC2', 'PPMEDSC3',
                            'PPMEDSC4','PPMEDSC5','PPMEDSC6') ,"INSURANCE")

# Social needs resolved & unresolved ----
# SOCIALNEEDC1 & SNMEAL	Is having enough money for food a concern now?
# SOCIALNEEDC2 & SNLIVE	Is having a place to live a concern now?
# SOCIALNEEDC3 & SNUTILITIES	Is paying for utilities a concern now?
# SOCIALNEEDC4 & SNTRANSPORT	Is getting where you need to go a concern now?
# SOCIALNEEDC5 & SNCHILDCARE	Is daycare/childcare a concern now?
# SOCIALNEEDC6 & SNINCOME	Is having regular income a concern now?
# SOCIALNEEDC7 & SNDRUGS	Are you concerned now about someone in your home using drugs or alcohol?
# SOCIALNEEDC8 & SNUNSAFE	Do you feel unsafe in your daily life now?
# SOCIALNEEDC9 & SNABUSE	Is someone in your home threatening or abusing you now?
fig_4_30 <- print.fig(SNMEAL, data = subset(LTM_dsn, SOCIALNEEDC1 == "Yes"))
fig_4_31 <- print.fig(SNLIVE, data = subset(LTM_dsn, SOCIALNEEDC2 == "Yes"))
fig_4_32 <- print.fig(SNUTILITIES, data = subset(LTM_dsn, SOCIALNEEDC3 == "Yes"))
fig_4_33 <- print.fig(SNTRANSPORT, data = subset(LTM_dsn, SOCIALNEEDC4 == "Yes"))
fig_4_34 <- print.fig(SNCHILDCARE, data = subset(LTM_dsn, SOCIALNEEDC5 == "Yes"))
fig_4_35 <- print.fig(SNINCOME, data = subset(LTM_dsn, SOCIALNEEDC6 == "Yes"))
fig_4_36 <- print.fig(SNDRUGS, data = subset(LTM_dsn, SOCIALNEEDC7 == "Yes"))
fig_4_37 <- print.fig(SNUNSAFE, data = subset(LTM_dsn, SOCIALNEEDC8 == "Yes"))
fig_4_38 <- print.fig(SNABUSE, data = subset(LTM_dsn, SOCIALNEEDC9 == "Yes"))

# Gathering names of figures for each chaper for export ----
# names_1 <- data.frame(COLS = objects()) %>%
#   subset(startsWith(as.character(COLS), 'fig_1_'))
# names_2 <- data.frame(COLS = objects()) %>%
#   subset(startsWith(as.character(COLS), 'fig_2_'))
# names_3 <- data.frame(COLS = objects()) %>%
#   subset(startsWith(as.character(COLS), 'fig_3_'))
# names_4 <- data.frame(COLS = objects()) %>%
#   subset(startsWith(as.character(COLS), 'fig_4_'))
# 
# datasets_1 <- paste0("list(")
# for(i in names_1$COLS){
#   add <- paste0(i, " = ",sym(i), ", ")
#   datasets_1 <- paste0(datasets_1, add)
# }
# datasets_1 <- paste0(datasets_1, ")")
# datasets_1 <- gsub(", )", "\\)", datasets_1)
# 
# datasets_2 <- paste0("list(")
# for(i in names_2$COLS){
#   add <- paste0(i, " = ",sym(i), ", ")
#   datasets_2 <- paste0(datasets_2, add)
# }
# datasets_2 <- paste0(datasets_2, ")")
# datasets_2 <- gsub(", )", "\\)", datasets_2)
# 
# datasets_3 <- paste0("list(")
# for(i in names_3$COLS){
#   add <- paste0(i, " = ",sym(i), ", ")
#   datasets_3 <- paste0(datasets_3, add)
# }
# datasets_3 <- paste0(datasets_3, ")")
# datasets_3 <- gsub(", )", "\\)", datasets_3)
# 
# datasets_4 <- paste0("list(")
# for(i in names_4$COLS){
#   add <- paste0(i, " = ",sym(i), ", ")
#   datasets_4 <- paste0(datasets_4, add)
# }
# datasets_4 <- paste0(datasets_4, ")")
# datasets_4 <- gsub(", )", "\\)", datasets_4)

# List of datasets ----
list_1 <- list('fig_1_1' = fig_1_1, 'fig_1_10' = fig_1_10, 'fig_1_11' = fig_1_11, 
               'fig_1_12' = fig_1_12, 'fig_1_13' = fig_1_13, 
               'fig_1_14' = fig_1_14, 'fig_1_15' = fig_1_15, 'fig_1_16' = fig_1_16,
               'fig_1_17' = fig_1_17, 'fig_1_18' = fig_1_18, 'fig_1_19' = fig_1_19, 
               'fig_1_2' = fig_1_2, 'fig_1_20' = fig_1_20, 'fig_1_21' = fig_1_21, 
               'fig_1_22' = fig_1_22, 'fig_1_23' = fig_1_23, 'fig_1_24' = fig_1_24, 
               'fig_1_26' = fig_1_26, 'fig_1_27' = fig_1_27, 'fig_1_3' = fig_1_3, 
               'fig_1_30' = fig_1_30, 'fig_1_31' = fig_1_31, 'fig_1_33' = fig_1_33, 
               'fig_1_34' = fig_1_34, 'fig_1_36' = fig_1_36, 'fig_1_37' = fig_1_37, 
               'fig_1_39' = fig_1_39, 'fig_1_4' = fig_1_4, 'fig_1_40' = fig_1_40, 
               'fig_1_41' = fig_1_41, 'fig_1_42' = fig_1_42, 'fig_1_43' = fig_1_43, 
               'fig_1_45' = fig_1_45, 'fig_1_46' = fig_1_46, 'fig_1_47' = fig_1_47, 
               'fig_1_49' = fig_1_49, 'fig_1_5' = fig_1_5, 'fig_1_50' = fig_1_50, 
               'fig_1_51' = fig_1_51, 'fig_1_6' = fig_1_6, 'fig_1_7' = fig_1_7, 
               'fig_1_8' = fig_1_8, 'fig_1_9' = fig_1_9)

list_2 <- list(fig_2_1 = fig_2_1, 'fig_2_10' = fig_2_10, 'fig_2_11' = fig_2_11, 
               'fig_2_12' = fig_2_12, 'fig_2_13' = fig_2_13, 'fig_2_14' = fig_2_14, 
               'fig_2_15' = fig_2_15, 'fig_2_2' = fig_2_2, 'fig_2_20' = fig_2_20, 
               'fig_2_21' = fig_2_21, 'fig_2_22' = fig_2_22, 'fig_2_25' = fig_2_25, 
               'fig_2_26' = fig_2_26, 'fig_2_27' = fig_2_27, 'fig_2_28' = fig_2_28, 
               'fig_2_29' = fig_2_29, 'fig_2_3' = fig_2_3, 'fig_2_30' = fig_2_30, 
               'fig_2_31' = fig_2_31, 'fig_2_32' = fig_2_32, 'fig_2_33' = fig_2_33, 
               'fig_2_34' = fig_2_34, 'fig_2_39' = fig_2_39, 'fig_2_4' = fig_2_4, 
               'fig_2_5' = fig_2_5, 'fig_2_6' = fig_2_6, 'fig_2_7' = fig_2_7, 
               'fig_2_8' = fig_2_8, 'fig_2_9' = fig_2_9)

list_3 <- list('fig_3_1' = fig_3_1, 'fig_3_2' = fig_3_2, 'fig_3_3' = fig_3_3, 
               'fig_3_4' = fig_3_4, 'fig_3_5' = fig_3_5, 'fig_3_6' = fig_3_6, 
               'fig_3_7' = fig_3_7, 'fig_3_8' = fig_3_8, 'fig_3_9' = fig_3_9)

list_4 <- list('fig_4_1' = fig_4_1, 'fig_4_10' = fig_4_10, 'fig_4_11' = fig_4_11, 
               'fig_4_12' = fig_4_12, 'fig_4_13' = fig_4_13, 'fig_4_14' = fig_4_14, 
               'fig_4_15' = fig_4_15, 'fig_4_16' = fig_4_16, 'fig_4_17' = fig_4_17, 
               'fig_4_18' = fig_4_18, 'fig_4_19' = fig_4_19, 'fig_4_2' = fig_4_2, 
               'fig_4_20' = fig_4_20, 'fig_4_21' = fig_4_21, 'fig_4_22' = fig_4_22, 
               'fig_4_23' = fig_4_23, 'fig_4_24' = fig_4_24, 'fig_4_25' = fig_4_25, 
               'fig_4_26' = fig_4_26, 'fig_4_27' = fig_4_27, 'fig_4_28' = fig_4_28, 
               'fig_4_29' = fig_4_29, 'fig_4_3' = fig_4_3, 'fig_4_30' = fig_4_30, 
               'fig_4_31' = fig_4_31, 'fig_4_32' = fig_4_32, 'fig_4_33' = fig_4_33, 
               'fig_4_34' = fig_4_34, 'fig_4_35' = fig_4_35, 'fig_4_36' = fig_4_36, 
               'fig_4_37' = fig_4_37, 'fig_4_38' = fig_4_38, 'fig_4_4' = fig_4_4, 
               'fig_4_5' = fig_4_5, 'fig_4_6' = fig_4_6)
# Final Export ----
write.xlsx(list_1, file = "Chapter_1_Figures.xlsx")
write.xlsx(list_2, file = "Chapter_2_Figures.xlsx")
write.xlsx(list_3, file = "Chapter_3_Figures.xlsx")
write.xlsx(list_4, file = "Chapter_4_Figures.xlsx")
