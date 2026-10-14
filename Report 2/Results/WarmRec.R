# Warm recruitment comparison

r_svysummary(by = "WarmRec2", 
             include = c('PHQ4_MH2WK_ANX', 'PHQ4_MH2WK_DEP', 'PHQ4_MH2WK_PSYCH',
                         'MATTRAUMA', 'PROBABLEPTSD', 'MATHOSPC3', 'BABYHOSP',
                         'MATERC3', 'BABYER', 'SELFCARE_A1', 'SELFCARE_A2', 
                         'SELFCARE_A3', 'SELFCARE_A4', 'SELFCARE_A5', 'SELFCARE_A6',
                         'BFGOAL', 'BFANYSAT',
                         'OTHERSUPP_A1', 'OTHERSUPP_A2', 'OTHERSUPP_A3',
                         'OTHERSUPP_A4', 'FFSUPP_A1', 'FFSUPP_A2', 'FFSUPP_A3',
                         'FFSUPP_A4', 'OTHERSUPPORTC1', 'OTHERSUPPORTC2', 
                         'OTHERSUPPORTC3', 'OTHERSUPPORTC4'))


tbl_svysummary(by = "WarmRec2", 
             include = c('RACE','PHQ4_MH2WK_ANX', 'PHQ4_MH2WK_DEP', 'PHQ4_MH2WK_PSYCH',
                         'MATTRAUMA', "DOULAANY", "BIRTHATTEND2", 
                         "PROVIDER2", "MH2WK_ANXDEP",  'PROBABLEPTSD',
                         'PHQ4_MH2WK_DEP', 'PHQ4_MH2WK_ANX', 'FEED1WEEK_ONLY',
                         'FEED1WEEKC1', 'FEED1WEEKC2', 'FEED1WEEKC3',
                         "BFEXCSAT",
                         'CURRFEEDC1', 'CURRFEEDC2', 'CURRFEEDC3', 'CURRFEEDC4'), 
             data = LTM_dsn, 
             statistic = list(all_categorical() ~ "{n_unweighted}, {p}%")) %>% 
  add_ci()
