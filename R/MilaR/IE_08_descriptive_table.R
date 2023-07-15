library(gtsummary)

#### prepare descriptive table ####

tbl_1 <-
  tbl_summary(data = gonogo, by = users, percent = "column",
                 include = c(dprime),
                 type = list(dprime ~ 'continuous'),
                 statistic = list(all_continuous() ~ "{mean} ({sd})"),
                 digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_summary(data = gonogo, by = users, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_1_2 <-
  tbl_summary(data = gonogo_subset, by = group, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_1_3 <-
  tbl_summary(data = matched_gonogo, by = group, percent = "column",
              include = c(dprime),
              type = list(dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 



tbl_2 <-
  tbl_summary(data = visualsearch, by = users, percent = "column",
              include = c(RT_6_absent, RT_12_absent, RT_18_absent,
                          RT_6_present, RT_12_present, RT_18_present),
              type = list(RT_6_absent ~ 'continuous', 
                          RT_12_absent ~ 'continuous', 
                          RT_18_absent ~ 'continuous',
                          RT_6_present ~ 'continuous', 
                          RT_12_present ~ 'continuous', 
                          RT_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_2_2 <-
  tbl_summary(data = visualsearch_subset, by = group, percent = "column",
              include = c(RT_6_absent, RT_12_absent, RT_18_absent,
                          RT_6_present, RT_12_present, RT_18_present),
              type = list(RT_6_absent ~ 'continuous', 
                          RT_12_absent ~ 'continuous', 
                          RT_18_absent ~ 'continuous',
                          RT_6_present ~ 'continuous', 
                          RT_12_present ~ 'continuous', 
                          RT_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_2_3 <-
  tbl_summary(data = matched_visualsearch, by = group, percent = "column",
              include = c(RT_6_absent, RT_12_absent, RT_18_absent,
                          RT_6_present, RT_12_present, RT_18_present),
              type = list(RT_6_absent ~ 'continuous', 
                          RT_12_absent ~ 'continuous', 
                          RT_18_absent ~ 'continuous',
                          RT_6_present ~ 'continuous', 
                          RT_12_present ~ 'continuous', 
                          RT_18_present ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_3 <-
  tbl_summary(data = taskswitching, by = users, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_3_2 <-
  tbl_summary(data = taskswitching_subset, by = group, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_3_3 <-
  tbl_summary(data = matched_taskswitching, by = group, percent = "column",
              include = c(singleblock_1_RT, switch_RT, congruent_RT),
              type = list(singleblock_1_RT ~ 'continuous', 
                          switch_RT ~ 'continuous', 
                          congruent_RT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4 <-
  tbl_summary(data = trailmaking, by = users, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4_2 <-
  tbl_summary(data = trailmaking_subset, by = group, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_4_3 <-
  tbl_summary(data = matched_trailmaking, by = group, percent = "column",
              include = c(MoveTime_1),
              type = list(MoveTime_1 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5 <-
  tbl_summary(data = nback, by = users, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5_2 <-
  tbl_summary(data = nback_subset, by = group, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_5_3 <-
  tbl_summary(data = matched_nback, by = group, percent = "column",
              include = c(N1_dprime, N2_dprime, N3_dprime),
              type = list(N1_dprime ~ 'continuous',
                          N2_dprime ~ 'continuous',
                          N3_dprime ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_6 <-
  tbl_summary(data = tunneling, by = users, percent = "column",
              include = c(MT_sc40, MT_sc60, MT_sc80, MT_sc100),
              type = list(MT_sc40 ~ 'continuous',
                          MT_sc60 ~ 'continuous',
                          MT_sc80 ~ 'continuous',
                          MT_sc100 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_6_2 <-
  tbl_summary(data = tunneling_subset, by = group, percent = "column",
              include = c(MT_sc40, MT_sc60, MT_sc80, MT_sc100),
              type = list(MT_sc40 ~ 'continuous',
                          MT_sc60 ~ 'continuous',
                          MT_sc80 ~ 'continuous',
                          MT_sc100 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_6_3 <-
  tbl_summary(data = matched_tunneling, by = group, percent = "column",
              include = c(MT_sc40, MT_sc60, MT_sc80, MT_sc100),
              type = list(MT_sc40 ~ 'continuous',
                          MT_sc60 ~ 'continuous',
                          MT_sc80 ~ 'continuous',
                          MT_sc100 ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


## add mirror (July 2023)
tbl_7 <-
  tbl_summary(data = mirror, by = users, percent = "column",
              include = c(meanMT),
              type = list(meanMT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_7_2 <-
  tbl_summary(data = mirror_subset, by = group, percent = "column",
              include = c(meanMT),
              type = list(meanMT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 

tbl_7_3 <-
  tbl_summary(data = matched_mirror, by = group, percent = "column",
              include = c(meanMT),
              type = list(meanMT ~ 'continuous'),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ 3)) %>%
  modify_caption("Table 1. Descriptive Statistics") 


tbl_combined <- tbl_merge(
    tbls = list(tbl_1, tbl_1_2, tbl_1_3,
                tbl_2, tbl_2_2, tbl_2_3,
                tbl_3, tbl_3_2, tbl_3_3,
                tbl_4, tbl_4_2, tbl_4_3,
                tbl_5, tbl_5_2, tbl_5_3,
                tbl_6, tbl_6_2, tbl_6_3,
                tbl_7, tbl_7_2, tbl_7_3)
  ) %>%
  as_flex_table()

tbl_combined

#### saving output in a Word document ####

save_as_docx(tbl_combined, path = "data/output/Table_1.docx")

