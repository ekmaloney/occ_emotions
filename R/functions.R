join_data <- function(cw, bi, gss){
  cross_block <- left_join(cw, bi, by = "term")
  
  #select variables I care about 
  cross_block <- cross_block %>% 
    select(term, isco88, blocks_4)
  
  #make sure the two variables to be joined on are the same type
  gss <- gss %>% dplyr::mutate(isco88 = as.integer(isco88))
  
  #join gss with the block_info
  gss_block <- dplyr::left_join(gss, cross_block, by = "isco88")
  
  return(gss_block)
}

gss_specific_year <- function(yr){
  data("gss_all")
  
  gss_year <- gss_all %>% filter(year == yr)
  return(gss_year)
}

#capwords function from Kieran's vignette
capwords <- function(x, strict = FALSE) {
  cap <- function(x) paste(toupper(substring(x, 1, 1)),
                           {x <- substring(x, 2); if(strict) tolower(x) else x},
                           sep = "", collapse = " " )
  sapply(strsplit(x, split = " "), cap, USE.NAMES = !is.null(names(x)))
}

recode_data <- function(d, emotion_vars, cont_vars, cat_vars){
  #combine different variables
  vars <- c(cont_vars, emotion_vars, cat_vars)
  
  #select the variables I am interested in 
  #and only keep people who are working full time
  d_new <- d %>% 
           select(all_of(vars)) %>% 
           filter(wrkstat == 1)
  
  d_new <- d_new %>% 
    modify_at(vars(), haven::zap_missing) %>% 
    modify_at(cat_vars, as.character) %>% 
    modify_at(emotion_vars, as.numeric) %>%
    modify_at(cat_vars, forcats::as_factor) %>% 
    modify_at(cat_vars, forcats::fct_relabel, capwords, strict = TRUE) %>% 
    mutate(spouse_work = case_when(spwrksta == 1 ~ "working full time",
                                   spwrksta == 2 ~ "working part time",
                                   spwrksta == 3 ~ "not working",
                                   spwrksta == 4 ~ "not working",
                                   spwrksta == 5 ~ "retired",
                                   spwrksta == 6 ~ "in school",
                                   spwrksta == 7 ~ "stay-at-home parent",
                                   spwrksta == 8 ~ "other"),
           religion = case_when(relig == 1 ~ "Protestant",
                                relig == 2 ~ "Catholic",
                                relig == 3 ~ "Jewish",
                                relig == 4 ~ "Not Religious",
                                relig > 4 & relig < 14 ~ "Other"),
           children = case_when(childs == 0 ~ "none",
                                childs > 0 & childs < 3 ~ "1-2",
                                childs >= 3 ~ "3+"),
           degree = case_when(degree == 0 ~ "Less than HS",
                              degree == 1 ~ "HS",
                              degree == 2 ~ "Junior College",
                              degree == 3 ~ "Bachelor",
                              degree == 4 ~ "Graduate"),
           race = case_when(race == 1 ~ "White",
                            race == 2 ~ "Black",
                            race == 3 ~ "Other"),
           sex = case_when(sex == 1 ~ "Male",
                           sex == 2 ~ "Female"),
           health = case_when(health == 1 ~"Excellent",
                              health == 2 ~ "Good",
                              health == 3 ~ "Fair",
                              health == 4 ~ "Poor"),
           health = factor(health, levels = c("Poor", "Fair", "Good", "Excellent")),
           children = factor(children, levels = c("none", "1-2", "3+")),
           religion = as.factor(religion),
           educ = as.integer(educ),
           dadsei = scale(pasei10, center = TRUE, scale = TRUE),
           log_income = log(realinc),
           log_income = as.numeric(log_income),
           occ = tolower(term),
           age_std = scale(age, center = TRUE, scale = TRUE),
           prestg80 = as.integer(prestg80), 
           race = as.factor(race),
           race = relevel(race, ref = "White"),
           race_bin = case_when(race == "White" ~ "White",
                                race == "Black" ~ "Not White",
                                race == "Other" ~ "Not White"),
           age_cat = case_when(age >= 18 & age <= 29 ~ "18-29",
                               age > 29 & age <= 44 ~ "30-44",
                               age > 44 & age <= 64 ~ "45-64",
                               age < 65 ~ "65+"))
  
  return(d_new)
}

calc_char_emotions <- function(occs, mods){
  
  #name variables so the joining process will work correctly
  occs_in_an <- occs %>% mutate(occ = term,
                                      o_e = E,
                                      o_p = P,
                                      o_a = A) %>% select(-c(term, E, P, A))
  
  #the gss emotion words
  gss_emotions <- c("calm",
                    "outraged",
                    "happy",
                    "sad",
                    "ashamed",
                    "excited",
                    "interested",
                    "lonely",
                    "fearful",
                    "overjoyed",
                    "worried",
                    "contented",
                    "anxious",
                    "tense",
                    "restless",
                    "mad",
                    "at ease",
                    "angry",
                    "embarassed",
                    "proud")
  
  #get the EPA values for the emotion words in the GSS
  gss_words_in_dictionary <- mods %>% 
    filter(term %in% gss_emotions) %>% 
    select(term, E, P, A)
  
  #need to create df to calculate the distance from each emotion word for every occupation
  #first multiply the list of emotions by 109 (the number of occupations)
  emotion_words <- tibble(term = rep(gss_words_in_dictionary$term, 109))
  #next join that list with the EPA values for each one
  emotion_words <- left_join(emotion_words, gss_words_in_dictionary, by = "term")
  #create list of occupations with each occ replicated the same # of times as the number of emotion words (18)
  occs <- tibble(occ = rep(occs_in_an$occ, 18)) %>% arrange(occ)
  #join that list with the EPA values for each occupational identity 
  occs <- left_join(occs, occs_in_an, by = "occ")
  
  #last, bind together
  occs_emotions <- cbind(emotion_words, occs)
  
  #calculate the euclidean distance from each emotion word for every occupation 
  #(once with the m char emotion and once with the f char emotion)
  occs_emotions <- occs_emotions %>% 
                   mutate(mdist = ((E - e_e)^2 + (P - e_p)^2 + (A - e_a)^2),
                   fdist = ((E - fe_e)^2 + (P - fe_p)^2 + (A - fe_a)^2)) %>% 
                   filter(!is.na(fe_e))
  
  return(occs_emotions)
}

make_hist_plot <- function(occs_emotions){
  
  occs_emotions <- occs_emotions %>% 
                   mutate(term = str_replace(term, "contentd", "contented"),
                          term = str_replace(term, "hapfeel", "happy"),
                          term = str_replace(term, "madat", "mad"),
                          term = str_replace(term, "ovrjoyed", "overjoyed")) %>% 
                   filter(term != "tense" & term != "interested") %>% 
                   pivot_longer(cols = mdist:fdist,
                   names_to = "gender", values_to = "distance")
  
  dist_hist <- ggplot(occs_emotions, mapping = aes(x = distance, fill = gender)) + 
    geom_histogram(binwidth = 2) + 
    facet_wrap(~term) + 
    theme_minimal() + scale_fill_manual(values = c(muted("pink"), "grey"),
                                        labels = c("female", "male")) +
    labs(title = "Distribution of Occ-Char-Emotion-Distances",
         subtitle = "across included emotions, both genders",
         y = "frequency")
  
  ggsave(here("output/dist_hist.png"), dist_hist)
}

join_gss_and_emotions <- function(gss_emotions, occs_emotions){
  
  gss_long <- gss_emotions %>% 
              pivot_longer(cols = angry:proud, 
                           names_to = "emotion", 
                           values_to = "count") %>% 
              mutate(occ = tolower(term))
  
  #fix the names of emotion words so the df with gss responses and the df of emotions & distances are joinable 
  occs_emotions_joining <- occs_emotions %>% 
    mutate(emotion = term,
           emotion = str_replace(emotion, "happy", "hapfeel"),
           emotion = str_replace(emotion, "overjoyed", "ovrjoyed"),
           emotion = str_replace(emotion, "contented", "contentd"),
           emotion = str_replace(emotion, "mad", "madat")) %>% 
    select(occ, emotion, mdist, fdist, o_e, o_p, o_a)
  
  #join the two dfs together
  gss_long_emotions <- left_join(gss_long, 
                                 occs_emotions_joining, 
                                 by = c("occ", "emotion"))
  
  gss_long_emotions <- gss_long_emotions %>% 
                       mutate(count_fact = as.factor(count),
                              count_frac = count/7,
                              count_group = case_when(count == 0 ~ "None",
                                                      count > 0 & count < 4 ~ "1-3",
                                                      count > 3 & count < 7 ~ "4-6",
                                                      count == 7 ~ "All"),
                              count_bin = case_when(count < 4 ~ 0,
                                                    count > 3 ~ 1),
                              emotion_type = case_when(emotion %in% c("hapfeel", "proud", "ovrjoyed", "contentd", 
                                                                      "excited", "calm") ~ "good_powerful",
                                                       emotion %in% c("lonely", "fearful", "ashamed", "worried", "sad", "madat", 
                                                                      "restless", "anxious") ~ "bad_weak",
                                                       emotion %in% c("outraged", "angry") ~ "bad_powerful"),
                              emotion_type = factor(emotion_type, levels = c("bad_powerful", "good_powerful", "bad_weak")))
  
  gss_long_emotions$count_fact <- ordered(gss_long_emotions$count_fact)
  gss_long_emotions$count_group <- factor(gss_long_emotions$count_group, levels = c("None", "1-3", "4-6", "All"))
  gss_long_emotions$count_group <- ordered(gss_long_emotions$count_group)
  
  return(gss_long_emotions)
  
}

remove_missing_cases <- function(gss_long_emotions, vars){
  
  #filter out missing variables on: DV (count), sex, age, race, and mat_power (IVs)
  #standardize the predictor variable of dist_emotion
  gss_analysis <- gss_long_emotions %>% 
                  filter(!(if_any(vars, is.na))) %>% 
                  mutate(distance = case_when(sex == "Female" ~ fdist,
                                sex == "Male" ~ mdist),
                  dist_emotion = scale(distance, center = TRUE, scale = TRUE),
                  income_std = scale(realinc, center = TRUE, scale = TRUE),
                  age_std = scale(age, center = TRUE, scale = TRUE),
                  prestg_std = scale(prestg80, center = TRUE, scale = TRUE)) %>% 
                  mutate(emotion = str_replace(emotion, "contentd", "contented"),
                         emotion = str_replace(emotion, "hapfeel", "happy"),
                         emotion = str_replace(emotion, "madat", "mad"),
                         emotion = str_replace(emotion, "ovrjoyed", "overjoyed"))
  
  return(gss_analysis)
}

run_binary_models <- function(family, d){
  
  if(family == "negbinom"){
    m <- glmmTMB(count ~ dist_emotion + emotion + (1 | id),
                  zi =~ emotion,
                  family = nbinom2, 
                  data= d)
  } else if(family == "logit"){
    m <- glmmTMB(count_bin ~ dist_emotion + emotion + (1 | id),
                   data = d, family = binomial)
  }
  
  return(m)
  
}

run_control_models <- function(family = c("negbinom", "logit"), d){
  
  if(family == "negbinom"){
    m1 <- glmmTMB(count ~ dist_emotion + income_std + degree + prestg_std + emotion + (1 | id),
                       zi =~ emotion,
                       family = nbinom2, 
                       data= d)
    
    m2 <- glmmTMB(count ~ dist_emotion + income_std + degree + sex + race + age_cat + prestg_std + 
                    emotion + (1 | id),
                  zi =~ emotion,
                  family = nbinom2, 
                  data= d)
    
    m3 <- glmmTMB(count ~ dist_emotion + income_std + degree + sex + race + age_cat + emotion + (1 | id),
                  zi =~ emotion,
                  family = nbinom2, 
                  data= d)  
    
    
  } else if(family == "logit"){
    m1 <- glmmTMB(count_bin ~ dist_emotion + income_std + degree + prestg_std + emotion + (1 | id),
                  data = d, family = binomial)
    
    m2 <- glmmTMB(count_bin ~ dist_emotion + income_std + degree + sex + race + age_cat + prestg_std + 
                    emotion + (1 | id),
                  data = d, family = binomial)
    
    m3 <- glmmTMB(count_bin ~ dist_emotion + degree + income_std + sex + race + age_cat + prestg_std + emotion + (1 | id),
                  data = d, family = binomial)
  }
  
  
  results <- list(m1, m2, m3)
  return(results)
  
}

run_int_models <- function(d){
  
  inc_int <- glmmTMB(count_bin ~ dist_emotion*income_std + degree + sex + 
                       race + age_cat + prestg_std + emotion + (1 + dist_emotion | id),
                     data = d, family = binomial)
  
  degree_int <- glmmTMB(count_bin ~ dist_emotion*degree + income_std + sex + 
                          race + age_cat + prestg_std + emotion + (1 + dist_emotion | id),
                        data = d, family = binomial)
  
  sex_int <- glmmTMB(count_bin ~ dist_emotion*sex + degree + income_std + 
                       race + age_cat + prestg_std + emotion + (1 + dist_emotion | id),
                     data = d, family = binomial)
  
  sex_int_child <- glmmTMB(count_bin ~ dist_emotion*sex + degree + income_std + 
                             race + age_cat+ prestg_std + emotion + children + 
                            (1 + dist_emotion | id),
                           data = d, family = binomial)
  
  age_int <- glmmTMB(count_bin ~ dist_emotion*age_cat + degree + income_std + 
                       race + sex + prestg_std + emotion + (1 + dist_emotion | id),
                     data = d, family = binomial)
  
  prstg_int <- glmmTMB(count_bin ~ dist_emotion*prestg_std + degree + income_std + 
                         race + sex + age_cat + emotion + (1 + dist_emotion | id),
                       data = d, family = binomial)
  
  
  int_models <- list(inc_int, 
                     degree_int,
                     sex_int, 
                     sex_int_child,
                     age_int,
                     prstg_int)
  
  return(int_models)
  
}

run_emotion_specific_int <- function(d, emo_type){
  
  d_subset <- d %>% 
              filter(emotion_type == emo_type) %>% 
              mutate(dist_emotion = scale(distance, center = TRUE, scale = TRUE),
                     income_std = scale(realinc, center = TRUE, scale = TRUE),
                     age_std = scale(age, center = TRUE, scale = TRUE),
                     prestg_std = scale(prestg80, center = TRUE, scale = TRUE))
  
  emo_control <- glmmTMB(count_bin ~ dist_emotion + degree + income_std + 
                           sex + race + age_cat + emotion + (1 | id),
                data = d_subset, family = binomial)
  
  emo_int_models <- run_int_models(d = d_subset)
  
  final_emo_int <- list(emo_control, emo_int_models)
  
  return(final_emo_int)
}
  
write_descriptive_table <- function(d, fn){
  descriptives <- d %>% 
                  group_by(id) %>% 
                  slice(1) %>% 
                  ungroup() %>% 
                  filter(!is.na(occ)) %>% 
                  select(age, 
                         age_cat,
                         realinc,
                         income_std,
                         sex,
                         race,
                         degree,
                         children,
                         prestg80) %>% 
                  mutate(realinc = as.integer(realinc)) %>% 
                  as.data.frame()
  
  d_table <- descriptives %>% tbl_summary(label = c(age_cat ~ "Age",
                                          realinc ~ "Income",
                                          race ~ "Race",
                                          sex ~ "Sex",
                                          degree ~ "Highest Degree",
                                          children ~ "Number of Children")) %>% 
          as_gt()
  
  gt::gtsave(d_table, paste(fn, ".html", sep = ""), path = here("output/"))
}

write_model_results <- function(m, fn, type = c("base", "int", "emotion")){
  if(type == "base"){
    htmlreg(l = list(m[[1]], m[[2]][[1]], m[[2]][[2]], m[[2]][[3]]),
            file = paste("output/", fn, ".html", sep = ""),
            custom.coef.map = list("dist_emotion" = "Distance from Emotion",
                                   "income_std" = "Income (std)",
                                   "degreeLess than HS" = "Less than HS",
                                   "degreeHS" = "High School",
                                   "degreeJunior College" = "Junior College",
                                   "degreeGraduate" = "Graduate Degree",
                                   "sexMale" = "Male",
                                   "raceBlack" = "Black",
                                   "raceOther" = "Other Race", 
                                   "age_cat30-44" = "30-44",
                                   "age_cat45-64" = "45-64",
                                   "prestg_std" = "Occupational Prestige (std)"))
  } else if(type == "int"){
    htmlreg(l = list(m[[1]], m[[2]], 
                     m[[3]], m[[4]], m[[5]]),
            paste("output/", fn, ".html", sep = ""),
            custom.coef.map = list("dist_emotion" = "Distance from Emotion",
                                   "income_std" = "Income (std)",
                                   "dist_emotion:income_std" = "Dist Emotion*Income",
                                   "degreeLess than HS" = "Less than HS",
                                   "degreeHS" = "High School",
                                   "degreeJunior College" = "Junior College",
                                   "degreeGraduate" = "Graduate Degree",
                                   "dist_emotion:degreeLess than HS" = "Dist*Less than HS",
                                   "dist_emotion:degreeHS" = "Dist*HS",
                                   "dist_emotion:degreeJunior College" = "Dist*Junior College",
                                   "dist_emotion:degreeGraduate" = "Dist*Graduate Degree",
                                   "sexMale" = "Male",
                                   "dist_emotion:sexMale" = "Dist*Male",
                                   "children1-2" = "1-2 Children",
                                   "children3+" = "3+ Children",
                                   "raceBlack" = "Black",
                                   "raceOther" = "Other Race", 
                                   "age_cat30-44" = "30-44",
                                   "age_cat45-64" = "45-64",
                                   "dist_emotion:age_cat30-44"= "Dist*30-44",
                                   "dist_emotion:age_cat45-64" = "Dist*45-64"))
  } else if(type == "emotion"){
    htmlreg(l = list(m[[1]], m[[2]][[1]], 
                     m[[2]][[2]], m[[2]][[3]],
                     m[[2]][[4]], m[[2]][[5]]),
            paste("output/", fn, ".html", sep = ""),
            custom.coef.map = list("dist_emotion" = "Distance from Emotion",
                                   "income_std" = "Income (std)",
                                   "dist_emotion:income_std" = "Dist Emotion*Income",
                                   "degreeLess than HS" = "Less than HS",
                                   "degreeHS" = "High School",
                                   "degreeJunior College" = "Junior College",
                                   "degreeGraduate" = "Graduate Degree",
                                   "dist_emotion:degreeLess than HS" = "Dist*Less than HS",
                                   "dist_emotion:degreeHS" = "Dist*HS",
                                   "dist_emotion:degreeJunior College" = "Dist*Junior College",
                                   "dist_emotion:degreeGraduate" = "Dist*Graduate Degree",
                                   "sexMale" = "Male",
                                   "dist_emotion:sexMale" = "Dist*Male",
                                   "raceBlack" = "Black",
                                   "raceOther" = "Other Race", 
                                   "age_cat30-44" = "30-44",
                                   "age_cat45-64" = "45-64",
                                   "children1-2" = "1-2 Children",
                                   "children3+" = "3+ Children"))
  }
}

make_dist_vis <- function(d, fn){
  
  dist_hist <-  ggplot(d, mapping = aes(x = count)) + 
                geom_histogram(fill = muted("pink"), binwidth = 1) + 
                facet_wrap(~emotion) + 
                theme_minimal() + labs(x = "num days in prior week",
                                       y = "frequency")
  
  collapsed_hist <- ggplot(d, mapping = aes(x = count)) + 
                    geom_histogram(fill = muted("pink"), binwidth = 1) + 
                    theme_minimal() + labs(x = "num days in prior week",
                                           y = "frequency")
  
  bin_hist <- ggplot(d, mapping = aes(x = count_bin)) + 
              geom_histogram(fill = muted("pink"), binwidth = 1) + 
              theme_minimal() + scale_x_continuous(breaks = c(0, 1),
                                                   labels=c("0-3 days", "4-7 days")) +
              labs(x = "num days in prior week",
                   y = "frequency")
  
  together <- (dist_hist)/(collapsed_hist + bin_hist) + 
              plot_annotation(title = "Distribution of Emotion Counts",
                              subtitle = "by emotion, aggregated, and binarized")
  
  fn <- toString(here(paste("output/", fn, ".png",sep = "")))
  
  ggsave(filename = fn, plot = together, device = "png", 
         width = 5, height = 7)
  
}

make_emotion_plot <- function(m, fn){
  
  emotion_eff <- plot_model(m[[3]], type = "pred", terms = "emotion", se = TRUE,
                            order.terms = c(7, 3, 4, 5, 15, 2, 6, 8, 9, 10, 11, 12, 13, 14)) +
    theme_minimal() + labs(title = "Predicted Probabilities",
                           subtitle = "of Reporting Emotional Experience more than Half the Week",
                           x = "specific emotion",
                           y = "probability") + coord_flip()
  
  new_plot_data <- emotion_eff[["data"]]
  new_plot_data <- new_plot_data %>% mutate(emotion_name = emotion_eff[["plot_env"]][["x_lab"]])
  
  emo_plot <- ggplot(data = new_plot_data, mapping = aes(x = reorder(emotion_name, predicted), y = predicted)) + 
    geom_point() + geom_linerange(aes(x = reorder(emotion_name, predicted), ymin = conf.low,
                                      ymax = conf.high)) + coord_flip() + theme_minimal() +
    labs(title = "Predicted Probabilities",
         subtitle = "of Reporting Emotional Experience more than Half the Week",
         x = "specific emotion",
         y = "probability") + coord_flip()
  
  fn <- toString(here(paste("output/", fn, ".png",sep = "")))
  
  ggsave(filename = fn, plot = emo_plot, device = "png")
}

calc_corr_test <- function(d, mod, m_number){
  emotion_eff <- get_model_data(mod[[m_number]], type = c("pred"), terms = "emotion")
  
  corr_test <- d %>% 
    group_by(term) %>% 
    summarise(a_m_dist = mean(mdist),
              a_f_dist = mean(fdist)) %>% 
    filter(term != "interested" & term != "tense") %>% 
    ungroup() %>% 
    mutate(eff = emotion_eff$predicted)
  
  m_test <- cor.test(corr_test$a_m_dist, corr_test$eff)
  f_test <- cor.test(corr_test$a_f_dist, corr_test$eff)  
  
  both_test <- list(m_test, f_test)
  return(both_test)
}

make_int_plots <- function(mod, m_number, var, subt, legend_label, fn){
  
  x <- plot_model(mod[[m_number]], type = "pred", 
                  terms = c("dist_emotion [all]", var))
  
  x <- x + labs(title = "Predicted Probabilities",
           subtitle = subt,
           x = "distance from emotion",
           y = "probability",
           color = legend_label) + theme_minimal()
  
  fn <- toString(here(paste("output/", fn, ".png",sep = "")))
  
  ggsave(filename = fn, plot = x, device = "png")
}

make_emotion_table <- function(d, fn){
  emo_desc <- d %>% select(distance, emotion)
  
  emo_desc <- emo_desc %>% 
    group_by(emotion) %>% 
    summarise(mean_dist = mean(distance, na.rm = TRUE),
              sd_dist = sd(distance, na.rm = TRUE))
  
  emo_table <- kable(emo_desc, 
              format = "html", 
              digits = 3, 
              caption = "Table 1: Distances from Characteristic Emotion by Emotion DV", 
              col.names = c("Emotion", "Mean", "Standard Deviation"))
  
  save_kable(emo_table, here(paste("output/", fn)))
}

