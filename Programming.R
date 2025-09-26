source("~/Documents/2025-2026/LTM/Listening-to-Mothers/ApplyDictionary.R")

LTM_final <- LTM_final %>% 
  mutate(INSURANCE = case_when(INSURANCE == "Medicaid/CHIP" ~ "Medicaid/CHIP", 
                               INSURANCE == "Private" ~ "Private",
                               INSURANCE == "None" ~ "None", 
                               INSURANCE == "Missing" ~ NA, 
                               TRUE ~ "Other"))

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
fig1 <- LTM_final %>% 
  mutate(PROVIDER = case_when(PROVIDER == "An obstetrician-gynecologist doctor \\(could be called OB, OBGYN, or maternal-fetal medicine specialist\\)" ~ "Any Doctor", 
                              PROVIDER == "A midwife (could be called CNM)" ~ "Midwife", 
                              PROVIDER == "A family medicine doctor (could be called FP)" ~ "Any Doctor", 
                              PROVIDER == "A doctor but I'm not sure what kind" ~ "Any Doctor",
                              PROVIDER == "Missing" | PROVIDER == "I'd prefer not to answer" | is.na(PROVIDER) ~ "Missing",
                              TRUE ~ "Other")) %>% 
  as_survey(weights = c(wght)) %>% 
  group_by(PROVIDER, RACE) %>% 
  summarize(pct = survey_prop(vartype = "ci"))

fig2 <- LTM_final %>% 
  mutate(PROVIDER = case_when(PROVIDER == "An obstetrician-gynecologist doctor \\(could be called OB, OBGYN, or maternal-fetal medicine specialist\\)" ~ "Any Doctor", 
                              PROVIDER == "A midwife (could be called CNM)" ~ "Midwife", 
                              PROVIDER == "A family medicine doctor (could be called FP)" ~ "Any Doctor", 
                              PROVIDER == "A doctor but I'm not sure what kind" ~ "Any Doctor",
                              PROVIDER == "Missing" | PROVIDER == "I'd prefer not to answer" | is.na(PROVIDER) ~ "Missing",
                              TRUE ~ "Other")) %>% 
  # subset(INSURANCE %in% c("Medicaid/CHIP", "Private")) %>%
  as_survey(weights = c(wght)) %>% 
  group_by(PROVIDER, INSURANCE) %>% 
  summarize(pct = survey_prop(vartype = "ci"))

# Provider Choice ----
fig3 <- LTM_final %>%
  mutate(PROVIDERCHOICE = case_when(PRENAT == "No Prenatal Care" ~ "No Prenatal Care", 
                                    PROVIDERCHOICE == "No, I had no choice; my maternity care provider was assigned to me" ~ "No Choice",
                                    PROVIDERCHOICE %in% c("Missing", "I'd prefer not to answer") ~ "Missing",
                                    TRUE ~ "Yes"),
         PROVIDERCHOICE = factor(PROVIDERCHOICE, levels = c("Yes", "No Choice", "No Prenatal Care", "Missing"))) %>%
  as_survey(weights = c(wght)) %>%
  group_by(RACE, PROVIDERCHOICE) %>%
  summarize(pct = survey_prop(vartype = "ci"))

fig3 <- LTM_final %>%
  mutate(PROVIDERCHOICE = case_when(PRENAT == "No Prenatal Care" ~ "No Prenatal Care", 
                                    PROVIDERCHOICE == "No, I had no choice; my maternity care provider was assigned to me" ~ "No Choice",
                                    PROVIDERCHOICE %in% c("Missing", "I'd prefer not to answer") ~ "Missing",
                                    TRUE ~ "Yes"),
         PROVIDERCHOICE = factor(PROVIDERCHOICE, levels = c("Yes", "No Choice", "No Prenatal Care", "Missing"))) %>%
  # subset(INSURANCE %in% c("Medicaid/CHIP", "Private" )) %>%
  as_survey(weights = c(wght)) %>%
  group_by(INSURANCE, PROVIDERCHOICE) %>%
  summarize(pct = survey_prop(vartype = "ci"))

# Doula during pregnancy ----
# DOULAC1	== 1 

# First visit ----
# FIRSTVISIT 1 == YES, 2 == NO

# Reason for no prenatal ----
collapse.fun(c('NOPRENATALC1','NOPRENATALC2','NOPRENATALC3','NOPRENATALC4',
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
# NOPRENATALC13O 95 = Other, please specify
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

collapse.fun(c('CARESETTINGC1','CARESETTINGC2','CARESETTINGC3','CARESETTINGC4',
               'CARESETTINGC5','CARESETTINGC6','CARESETTINGC7','CARESETTINGC8',
               'CARESETTINGC9'))

# CARETYPE1 ----
# CARETYPE1	Did you have a choice about whether to have office prenatal visits by yourself or group prenatal visits?

# CARETYPEPREF ----
# CARETYPEPREF	Which type of prenatal care do you prefer?
# 1	Prenatal visits by myself
# 2	Group prenatal visits
# 3	I like a mix of both
# 99	I’d prefer not to answer

# WHYTELE ----
# WHYTELEC1	  1 = Transportation to in-person care was a challenge
# WHYTELEC2	  2 = I wanted to save travel time
# WHYTELEC3	  3 = I had childcare responsibilities
# WHYTELEC4	  4 = I wanted to avoid exposure to COVID-19
# WHYTELEC5	  5 = I was more comfortable with remote than office visits
# WHYTELEC6	  6 = My maternity care provider preferred televisits
# WHYTELEC7	  95 = Other, please specify

collapse.fun(c('WHYTELEC1','WHYTELEC2','WHYTELEC2','WHYTELEC4',
               'WHYTELEC5','WHYTELEC6','WHYTELEC7'))

# BPCONFID+URINECONFID+WEIGHCONFID+BABYHRCONFID ----
# 1	Yes, fully confident
# 2	Yes, somewhat confident
# 3	No, not confident
# 99	I’d prefer not to answer

# CAREMODEPREF ----
#   CAREMODEPREF	Which mode of prenatal care do you prefer?
# 1	In-person visits
# 2	Televisits
# 3	I like a mix of both
# 99	I’d prefer not to answer

# EDUIMPACT ----
# EDUCIMPACTC1 1 = Helped me understand what would happen at the hospital
# EDUCIMPACTC2 2 = Helped me understand bodily childbirth processes
# EDUCIMPACTC3 3 = Helped me feel more confident about giving birth
# EDUCIMPACTC4 4 = Caused me to feel more concerned about giving birth
# EDUCIMPACTC5 5 = Helped my spouse/partner understand what I would experience
# EDUCIMPACTC6 6 = Had no effect
# EDUCIMPACTC7 95 = Something else, please specify
# EDUCIMPACTC8 99 = I’d prefer not to answer
collapse.fun(c('EDUCIMPACTC1','EDUCIMPACTC2','EDUCIMPACTC3',
               'EDUCIMPACTC4','EDUCIMPACTC5','EDUCIMPACTC6',
               'EDUCIMPACTC7','EDUCIMPACTC8'))

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

# Depression ----
# PREPREG_MHCONDC1

# Anxiety ----
# PREPREG_MHCONDC2

# MENTALSUPPORT ----
# MENTALSUPPORT

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
collapse.fun(c('SOCIALNEEDC1','SOCIALNEEDC2','SOCIALNEEDC3',
               'SOCIALNEEDC4','SOCIALNEEDC5','SOCIALNEEDC6',
               'SOCIALNEEDC7','SOCIALNEEDC8','SOCIALNEEDC9',
               'SOCIALNEEDC10','SOCIALNEEDC11'))

# EMPLOYBEN ----
# EMPLOYBEN

# BIGBABY1 == 1 ----
# BIGBABY1

# PLANNEDFEED ----
# PLANNEDFEEDC1	 1 = Breast milk
# PLANNEDFEEDC2	 2 = Formula
# PLANNEDFEEDC3	 98 = Unsure
# PLANNEDFEEDC4	 99 = I’d prefer not to answer

# Preg weight gain ----
# PREGWEIGHT - PREPREGWEIGHT

# CHAPTER 2 ####
# DUE DATE & BIRTHDATE ----
# DUEDATE 
# BIRTHDATE

# GOLDENHOUR ----

# SKIN ----

# BIRTHATTEND ----

# BIRTH DOULA ----
# DOULAC2

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

# INDUCE5 by SDM induction ----

# 1	Yes, my provider thought my labor should be induced
# 2	Yes, my provider thought my labor should not be induced
# 3	No, my provider did not make a recommendation
# 99	I’d prefer not to answer

# SELFINDUCE ----
# SELFINDUCE	Did you try to self-induce your labor before [CHILDNAME]'s birth? That is, did you do anything to try to cause your labor to begin before it started on its own?
# SELFINDUCE1	Did your self-induction start your labor?

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
collapse.fun(c('MEDINDUCE1C1','MEDINDUCE1C2','MEDINDUCE1C3',
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
collapse.fun(c('MEDINDUCE3C1','MEDINDUCE3C2','MEDINDUCE3C3',
               'MEDINDUCE3C4','MEDINDUCE3C5','MEDINDUCE3C6',
               'MEDINDUCE3C7','MEDINDUCE3C8'),#'MEDINDUCE3C9'), 
             data = subset(LTM_final, MEDINDUCE1C1 == "Yes"))

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
collapse.fun(c("DRUGFREEC1","DRUGFREEC2","DRUGFREEC3",
               "DRUGFREEC4","DRUGFREEC5","DRUGFREEC6",
               "DRUGFREEC7","DRUGFREEC8","DRUGFREEC9",
               "DRUGFREEC10","DRUGFREEC11","DRUGFREEC12"))

# Painmeds ----
# PAINMEDSC1 1 = Epidural or spinal (medicine delivered into the spinal column)
# PAINMEDSC2 2 = Nitrous oxide gas (not oxygen) breathed through a mask or mouthpiece while remaining conscious
# PAINMEDSC3 3 = Narcotics (such as Demerol or Stadol), by intravenous (IV) line or spray in your nose
# PAINMEDSC4 4 = General anesthesia (you were “put to sleep” – no sensation, no conscious)
# PAINMEDSC5 5 = Pudendal or other local blocks (injections into the vagina or cervix)
# PAINMEDSC6 98 = Used pain medicine but not sure what
# PAINMEDSC7 97 = I did not use any pain medicine while giving birth
# PAINMEDSC8 99 = I’d prefer not to answer
collapse.fun(c("PAINMEDSC1","PAINMEDSC2","PAINMEDSC3",
               "PAINMEDSC4","PAINMEDSC5","PAINMEDSC6",
               "PAINMEDSC7","PAINMEDSC8"))

# laborint 1 - 4 ----
# LABORINTC1 1 = A care team member broke your bag of water after labor contractions had begun
# LABORINTC2 2 = You got intravenous (IV) fluids through a vein in your arm or hand
# LABORINTC3 3 = You were given the drug Pitocin (“Pit” or synthetic oxytocin) to speed up labor after labor contractions had begun
# LABORINTC4 4 = A tube (catheter) was inserted into your bladder to remove urine

# position ----
# POSITION

# psych childbirth ----
# Spontaneous labor and birth at term without the use 
# of pharmacologic or mechanical interventions for 
# labor stimulation or pain management throughout labor and birth 

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

# hospfeed ----
# Among those who planned partial or exclusive BF/PLANNEDFEED = 1, 
# whether nurses/other staff aligned with Baby-Friendly “steps” 
# that women could reasonably know/HOSPFEED = 1-7, 9-10

# subscale respectful care ----
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

# Respect care: developer’s 0-100 scoring guidance

# culture ----
# Not sure about this 

# CHAPTER 3 ####
# Mode of Birth ----
# MODE2023

# Unplanned Reason | CSECTIONTYPE = 2 ----
# CSECTIONTYPE == 2 

fig <- LTM_final %>%
  subset(CSECTIONTYPE == 2)

# UNPLANNEDREASON
# 1	Labor was taking too long
# 2	The baby was not doing well in labor
# 3	Because of my health problem, the baby needed to be born soon
# 95	Some other medical reason, please specify
# 98	There was no medical reason
# 99	I’d prefer not to answer

# Planned C reason ----
# PLANNEDC
fig <- LTM_final %>%
  subset(CSECTIONTYPE == 1)

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

# Reason for Repeat CS ----


# CHAPTER 4 ####
# DOULA ----
# DOULAC3  == 1

# PPVISIT ----
# PPVISIT categorized as 0, 1, 2, 3, 4+ 

# PLANNEDFEED vs. FEED1WEEK ----
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

collapse.fun(c("HOSPFEEDC1","HOSPFEEDC2","HOSPFEEDC3",
               "HOSPFEEDC4","HOSPFEEDC5","HOSPFEEDC6",
               "HOSPFEEDC7","HOSPFEEDC8","HOSPFEEDC9",
               "HOSPFEEDC10","HOSPFEEDC11","HOSPFEEDC12"))

# EXCLBFGOAL ----
# EXCLBFGOAL

# BFGOAL ----


# PPBOTHER as PHQ2 ----
# PPBOTHER_A3 Having little interest or pleasure in doing things
# PPBOTHER_A4 Feeling down, depressed, or hopeless

# PPBOTHER as GAD2 ----
# PPBOTHER_A1 Feeling nervous, anxious, or on edge
# PPBOTHER_A2 Not being able to stop or control worrying

# PPBOTHER as PHQ4 ----
# PPBOTHER_A1 Feeling nervous, anxious, or on edge
# PPBOTHER_A2 Not being able to stop or control worrying
# PPBOTHER_A3 Having little interest or pleasure in doing things
# PPBOTHER_A4 Feeling down, depressed, or hopeless

# PPMEDS ----
# PPMEDSC1 1 = Yes, for anxiety
# PPMEDSC2 2 = Yes, for depression
# PPMEDSC3 3 = Yes, for substance use disorder
# PPMEDSC4 95 = Yes, for something else, please specify
# PPMEDSC4O 95 = Yes, for something else, please specify
# PPMEDSC5 97 = No
# PPMEDSC6 99 = I’d prefer not to answer

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

# Exporting ----
#list_of_datasets <- list("Name of DataSheet1" = dataframe1, "Name of Datasheet2" = dataframe2)
#write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")
