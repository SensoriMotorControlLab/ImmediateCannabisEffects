#### prepare descriptive table ####

gonogo$sex <- factor(gonogo$sex , labels = c("Female", "Male"))
gonogo$video_games <- factor(gonogo$video_games , labels = c("Yes", "No"))
gonogo$task <- "gonogo"
gonogo_dt <- data.table(gonogo)

visualsearch$sex <- factor(visualsearch$sex , labels = c("Female", "Male"))
visualsearch$video_games <- factor(visualsearch$video_games , labels = c("Yes", "No"))
visualsearch$task <- "visualsearch"
visualsearch_dt <- data.table(visualsearch)

taskswitching$sex <- factor(taskswitching$sex , labels = c("Female", "Male", "Other"))
taskswitching$video_games <- factor(taskswitching$video_games , labels = c("Yes", "No"))
taskswitching$task <- "taskswitching"
taskswitching_dt <- data.table(taskswitching)

trailmaking$sex <- factor(trailmaking$sex , labels = c("Female", "Male"))
trailmaking$video_games <- factor(trailmaking$video_games , labels = c("Yes", "No"))
trailmaking$task <- "trailmaking"
trailmaking_dt <- data.table(trailmaking)

tunneling$sex <- factor(tunneling$sex , labels = c("Female", "Male"))
tunneling$video_games <- factor(tunneling$video_games , labels = c("Yes", "No"))
tunneling$task <- "tunneling"
tunneling_dt <- data.table(tunneling)

nback$sex <- factor(nback$sex , labels = c("Female", "Male", "Other"))
nback$video_games <- factor(nback$video_games , labels = c("Yes", "No"))
nback$task <- "nback"
nback_dt <- data.table(nback)

mirror$sex <- factor(mirror$sex , labels = c("Female", "Male"))
mirror$video_games <- factor(mirror$video_games , labels = c("Yes", "No"))
mirror$task <- "mirror"
mirror_dt <- data.table(mirror)

combined_df <- rbindlist(list(gonogo_dt, visualsearch_dt, taskswitching_dt, 
                              trailmaking_dt, tunneling_dt, nback_dt, mirror_dt), fill = TRUE)
combined_df <- data.frame(combined_df)
combined_df <- combined_df[!duplicated(combined_df[c("id", "sex", "age", "users")]), ]

combined_df %>% 
  #filter(users == "High users") %>% 
  distinct(id) %>% 
  n_distinct()

tbl_1 <- tbl_summary(data = combined_df, by = users, percent = "column",
            include = c(age, sex),
            type = list(age ~ 'continuous'),
            statistic = list(all_continuous() ~ "{mean} ({sd}) [{min}, {max}]"),
            digits = list(all_continuous() ~ 3,
                          all_categorical() ~ 0),
    missing_text = "(Missing)") %>%
  add_overall() %>%
  modify_caption("Table 1. Descriptive Statistics") %>%
  as_flex_table()

tbl_1

#### saving output in a Word document ####

save_as_docx(tbl_1, path = "data/output/Table_2.docx")

