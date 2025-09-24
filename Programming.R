source("~/Documents/2025-2026/LTM/Listening-to-Mothers/ApplyDictionary.R")

# Tables as excel sheets

# Label by figure, each figure as a tab
# Excel sheet of number
# No % or $ signs
# Order for graphics to appear
# example of figures in ppt  

pinks <- c("#e89a96", "#f3cccb", "#ebaba9", "#f9f3f4", "#efb9b7")
greys <- c("#616366","#c1c3c5", "#c0c3c4","#edeced", "#e5e6e7")

# Figure prep example. 
fig1 <- LTM_final %>%
  mutate(PROVIDERCHOICE = case_when(PRENAT == "No Prenatal Care" ~ "No Prenatal Care", 
                                    PROVIDERCHOICE == "No, I had no choice; my maternity care provider was assigned to me" ~ "No Choice",
                                    PROVIDERCHOICE %in% c("Missing", "I'd prefer not to answer") ~ "Missing",
                                    TRUE ~ "Yes"),
         PROVIDERCHOICE = factor(PROVIDERCHOICE, levels = c("Yes", "No Choice", "No Prenatal Care", "Missing")),
         INSURANCE = case_when(INSURANCE == "Medicaid/CHIP" ~ "Medicaid/CHIP", 
                               INSURANCE == "Private" ~ "Private",
                               INSURANCE == "None" ~ "None", 
                               INSURANCE == "Missing" ~ NA, 
                               TRUE ~ "Other"))

list_of_datasets <- list("Name of DataSheet1" = dataframe1, "Name of Datasheet2" = dataframe2)
write.xlsx(list_of_datasets, file = "writeXLSX2.xlsx")

# Figure coding ----
  # fig1 %>% 
  # subset(INSURANCE %in% c("Medicaid/CHIP", "Private" )) %>%
  # as_survey(weights = c(wght)) %>%
  # group_by(RACE, PROVIDERCHOICE) %>%
  # summarize(pct = survey_prop(vartype = "ci")) %>% #View()
  # ggplot(aes(x = RACE, y = pct, fill = PROVIDERCHOICE)) +
  # geom_col(position = position_stack(reverse = FALSE)) +
  # # facet_wrap(~ INSURANCE) +
  # coord_flip() +  # horizontal bars like in report
  # scale_fill_manual(values = c("No Choice" = "#e89a96", "Yes" = "#f3cccb","No Prenatal Care" = "#c1c3c5" , "Missing" = "#c1c3c5")) +
  # labs(
  #   title = "Choice of Prenatal Care Provider, by Race/Ethnicity and Payer (California, 2016)",
  #   x = "Race / Ethnicity",
  #   y = "Percent of respondents",
  #   fill = "Provider choice"
  # ) +
  # theme_classic(base_size = 14) +
  # theme(
  #   strip.text = element_text(size = 12),  # facet labels
  #   axis.text = element_text(size = 11),
  #   legend.position = "bottom"
  # )
  # 



