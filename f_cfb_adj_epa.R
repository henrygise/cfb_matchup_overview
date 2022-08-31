# getting opponent-adjusted EPA/game

# uses Bud Davis's method from https://blog.collegefootballdata.com/opponent-adjusted-stats-ridge-regression/

# requires api_key from https://collegefootballdata.com/key

api_key <- ""

f_cfb_adj_epa <- function(season, thru_week) {
  
  # SETUP ####
  
  # load packages
  require(tidyverse)
  require(glmnet) # for ridge regression
  require(ggimage) # for plotting logos
  require(fastDummies) # for creating dummy variables
  
  # cfbfastR setup
  require(cfbfastR)
  Sys.setenv(CFBD_API_KEY = api_key)
  
  # GET GAME DATA ####
  
  # load all fbs vs. fbs games
  dfGames <- cfbd_game_info(year = season) %>%
    filter(home_division == "fbs" & away_division == "fbs",
           week <= thru_week)
  
  # get game ids
  game_ids <- unique(dfGames$game_id)
  
  # GET PLAY-BY-BY DATA ####
  
  # load all pbp data
  dfPBP_all <- data.frame()
  
  for (i in unique(dfGames$week)) {
    temp_pbp <- cfbd_pbp_data(season, week = i) %>% # get week-specific pbp data
      filter(game_id %in% game_ids)
    
    dfPBP_all <- rbind(dfPBP_all, temp_pbp) # create large df of pbp data
  }
  
  # extract pass/run play types
  dfPBP <- dfPBP_all %>%
    filter(!is.na(ppa)) %>%
    mutate(lower_play_type = tolower(play_type),
           type = ifelse(str_detect(lower_play_type, "rush"), "rush",
                         ifelse(str_detect(lower_play_type, "pass") | str_detect(lower_play_type, "sack") | str_detect(lower_play_type, "interception"), "pass",
                                ifelse(str_detect(lower_play_type, "fumble") | str_detect(lower_play_type, "placeholder") | str_detect(lower_play_type, "safety"),
                                       ifelse(str_detect(play_text, "rush ") | str_detect(play_text, "rushing") | str_detect(play_text, "run "), "rush",
                                              ifelse(str_detect(play_text, "pass") | str_detect(play_text, "sack") | str_detect(play_text, "interception"), "pass", NA)), NA)))) %>%
    # filter out non pass/runs
    filter(!is.na(type)) %>%
    # rename some columns
    rename(
      posteam = offense_play,
      defteam = defense_play) %>%
    # change name of ppa to epa
    mutate(epa = as.numeric(ppa))
  
  # clean and create df for calculations
  df_pass <- dfPBP %>%
    filter(type == "pass") %>%
    select(posteam, defteam, epa) %>%
    as.data.frame()
  
  df_rush <- dfPBP %>%
    filter(type == "rush") %>%
    select(posteam, defteam, epa) %>%
    as.data.frame()
  
  df <- dfPBP %>%
    select(posteam, defteam, epa) %>%
    as.data.frame()
  
  # run pass distribution
  df_run_pass_dist_o <- dfPBP %>%
    group_by(posteam, type) %>%
    summarise(count = n()) %>%
    pivot_wider(id_cols = "posteam",
                names_from = type,
                values_from = count) %>%
    mutate(pass_perc_o = pass / (pass + rush),
           rush_perc_o = rush / (pass + rush)) %>%
    select(posteam, pass_perc_o, rush_perc_o) %>%
    ungroup() %>%
    rename(team = posteam)
  
  df_run_pass_dist_d <- dfPBP %>%
    group_by(defteam, type) %>%
    summarise(count = n()) %>%
    pivot_wider(id_cols = "defteam",
                names_from = type,
                values_from = count) %>%
    mutate(pass_perc_d = pass / (pass + rush),
           rush_perc_d = rush / (pass + rush)) %>%
    select(defteam, pass_perc_d, rush_perc_d) %>%
    ungroup() %>%
    rename(team = defteam)
  
  df_run_pass_dist <- left_join(df_run_pass_dist_o, df_run_pass_dist_d)
  
  # OVERALL opponent adjustment ####
  
  # create dummy columns for ridge regresion
  dfDummies <- df %>% dummy_cols(select_columns = c("posteam", "defteam")) %>% select(-posteam, -defteam)
  
  x_matrix <- data.matrix(dfDummies %>% select(-epa)) # make x values
  y_matrix <- data.matrix(dfDummies %>% select(epa)) # make y values
  lambda_seq <- 10^seq(2, -2, by = -.1) # set range of lambdas
  
  ridge_cv <- cv.glmnet(x_matrix, y_matrix, alpha = 0, lambda = lambda_seq) # cross validation
  best_lambda <- ridge_cv$lambda.min # get lambda value that offers minimum
  
  best_ridge <- glmnet(x_matrix, y_matrix, alpha = 0, lambda = best_lambda, intercept = TRUE) # build final model
  
  # create final df
  df_final <- as.data.frame(as.matrix(coef(best_ridge))) %>%
    # bring team row names into its own column
    rownames_to_column("team") %>%
    # filter out intercept column
    filter(team != "(Intercept)") %>%
    # add intercept to all values
    mutate(adjEPA = s0 + best_ridge$a0) %>% select(-s0) %>%
    # create neater team column
    mutate(new_team = ifelse(str_detect(team,"posteam_"),
                             str_remove(team,"posteam_"),
                             str_remove(team,"defteam_")),
           side = ifelse(str_detect(team,"posteam_"),
                         "adjOff",
                         "adjDef")) %>%
    mutate(team = new_team) %>% select(-new_team) %>%
    # give each team one individual row
    pivot_wider(id_cols = team, names_from = side, values_from = adjEPA) %>%
    # rawOff
    left_join(dfPBP %>% select(posteam, epa) %>%
                group_by(posteam) %>%
                rename(team = posteam) %>%
                summarise(rawOff = mean(as.numeric(epa)))) %>%
    # rawDef
    left_join(dfPBP %>% select(defteam, epa) %>%
                group_by(defteam) %>%
                rename(team = defteam) %>%
                summarise(rawDef = mean(as.numeric(epa)))) %>%
    # scaling
    mutate(adjOff = (adjOff - mean(adjOff)) / sd(adjOff) * sd(rawOff) +  mean(adjOff),
           adjOff_60 = adjOff * 60,
           adjDef = (adjDef - mean(adjDef)) / sd(adjDef) * sd(rawDef) +  mean(adjDef),
           adjDef_60 = adjDef * 60)
  
  # PASSING opponent adjustment ####
  
  # create dummy columns for ridge regresion
  dfDummies <- df_pass %>% dummy_cols(select_columns = c("posteam", "defteam")) %>% select(-posteam, -defteam)
  
  x_matrix <- data.matrix(dfDummies %>% select(-epa)) # make x values
  y_matrix <- data.matrix(dfDummies %>% select(epa)) # make y values
  lambda_seq <- 10^seq(2, -2, by = -.1) # set range of lambdas
  
  ridge_cv <- cv.glmnet(x_matrix, y_matrix, alpha = 0, lambda = lambda_seq) # cross validation
  best_lambda <- ridge_cv$lambda.min # get lambda value that offers minimum
  
  best_ridge <- glmnet(x_matrix, y_matrix, alpha = 0, lambda = best_lambda, intercept = TRUE) # build final model
  
  # create final df
  df_final_pass <- as.data.frame(as.matrix(coef(best_ridge))) %>%
    # bring team row names into its own column
    rownames_to_column("team") %>%
    # filter out intercept column
    filter(team != "(Intercept)") %>%
    # add intercept to all values
    mutate(adjEPA = s0 + best_ridge$a0) %>% select(-s0) %>%
    # create neater team column
    mutate(new_team = ifelse(str_detect(team,"posteam_"),
                             str_remove(team,"posteam_"),
                             str_remove(team,"defteam_")),
           side = ifelse(str_detect(team,"posteam_"),
                         "adjOff_pass",
                         "adjDef_pass")) %>%
    mutate(team = new_team) %>% select(-new_team) %>%
    # give each team one individual row
    pivot_wider(id_cols = team, names_from = side, values_from = adjEPA) %>%
    # get weighted values
    left_join(df_run_pass_dist) %>%
    # rawOff
    left_join(dfPBP %>%
                filter(type == "pass") %>%
                select(posteam, epa) %>%
                group_by(posteam) %>%
                rename(team = posteam) %>%
                summarise(rawOff = mean(as.numeric(epa)))) %>%
    # rawDef
    left_join(dfPBP %>%
                filter(type == "pass") %>%
                select(defteam, epa) %>%
                group_by(defteam) %>%
                rename(team = defteam) %>%
                summarise(rawDef = mean(as.numeric(epa)))) %>%
    # scaling
    mutate(adjOff_pass = (adjOff_pass - mean(adjOff_pass)) / sd(adjOff_pass) * sd(rawOff) +  mean(adjOff_pass),
           adjOff_pass_60 = adjOff_pass * 60 * pass_perc_o,
           adjDef_pass = (adjDef_pass - mean(adjDef_pass)) / sd(adjDef_pass) * sd(rawDef) +  mean(adjDef_pass),
           adjDef_pass_60 = adjDef_pass * 60 * pass_perc_d) %>%
    select(-c(pass_perc_o, pass_perc_d, rush_perc_o, rush_perc_d))

  # RUSHING opponent adjustment ####
  
  # create dummy columns for ridge regresion
  dfDummies <- df_rush %>% dummy_cols(select_columns = c("posteam", "defteam")) %>% select(-posteam, -defteam)
  
  x_matrix <- data.matrix(dfDummies %>% select(-epa)) # make x values
  y_matrix <- data.matrix(dfDummies %>% select(epa)) # make y values
  lambda_seq <- 10^seq(2, -2, by = -.1) # set range of lambdas
  
  ridge_cv <- cv.glmnet(x_matrix, y_matrix, alpha = 0, lambda = lambda_seq) # cross validation
  best_lambda <- ridge_cv$lambda.min # get lambda value that offers minimum
  
  best_ridge <- glmnet(x_matrix, y_matrix, alpha = 0, lambda = best_lambda, intercept = TRUE) # build final model
  
  # create final df
  df_final_rush <- as.data.frame(as.matrix(coef(best_ridge))) %>%
    # bring team row names into its own column
    rownames_to_column("team") %>%
    # filter out intercept column
    filter(team != "(Intercept)") %>%
    # add intercept to all values
    mutate(adjEPA = s0 + best_ridge$a0) %>% select(-s0) %>%
    # create neater team column
    mutate(new_team = ifelse(str_detect(team,"posteam_"),
                             str_remove(team,"posteam_"),
                             str_remove(team,"defteam_")),
           side = ifelse(str_detect(team,"posteam_"),
                         "adjOff_rush",
                         "adjDef_rush")) %>%
    mutate(team = new_team) %>% select(-new_team) %>%
    # give each team one individual row
    pivot_wider(id_cols = team, names_from = side, values_from = adjEPA) %>%
    # get weighted values
    left_join(df_run_pass_dist) %>%
    # rawOff
    left_join(dfPBP %>%
                filter(type == "rush") %>%
                select(posteam, epa) %>%
                group_by(posteam) %>%
                rename(team = posteam) %>%
                summarise(rawOff = mean(as.numeric(epa)))) %>%
    # rawDef
    left_join(dfPBP %>%
                filter(type == "rush") %>%
                select(defteam, epa) %>%
                group_by(defteam) %>%
                rename(team = defteam) %>%
                summarise(rawDef = mean(as.numeric(epa)))) %>%
    # scaling
    mutate(adjOff_rush = (adjOff_rush - mean(adjOff_rush)) / sd(adjOff_rush) * sd(rawOff) +  mean(adjOff_rush),
           adjOff_rush_60 = adjOff_rush * 60 * rush_perc_o,
           adjDef_rush = (adjDef_rush - mean(adjDef_rush)) / sd(adjDef_rush) * sd(rawDef) +  mean(adjDef_rush),
           adjDef_rush_60 = adjDef_rush * 60 * rush_perc_d) %>%
    select(-c(pass_perc_o, pass_perc_d, rush_perc_o, rush_perc_d))

  # COMBINE DATA FRAMES ####
  df_final %>% select(-rawOff, -rawDef) %>%
    left_join(df_final_pass) %>% select(-rawOff, -rawDef) %>%
    left_join(df_final_rush) %>% select(-rawOff, -rawDef) %>%
    left_join(df_run_pass_dist)
}
