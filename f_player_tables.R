# visualizing player usage and production

api_key <- ""

f_player_tables <- function(home = "Pittsburgh", away, season = 2022) {
  
  # SETUP ####
  require(tidyverse)
  require(gt)
  require(gtExtras)
  
  # cfbfastR setup ####
  require(cfbfastR)
  Sys.setenv(CFBD_API_KEY = api_key)
  
  # load team info / change colors/logos if necessary
  team_info <- cfbd_team_info(year = season) %>%
    mutate(alt_color = ifelse(is.na(alt_color),"black",alt_color)) %>%
    mutate(color = ifelse(school == "Pittsburgh","#003594",
                          ifelse(school == "West Virginia", "#002855",
                                 ifelse(school == "Utah", "#CC0000", color))),
           alt_color = ifelse(school == "Pittsburgh","#FFB81C",
                              ifelse(school == "Florida", "#FA4616",
                                     ifelse(school == "Utah", "#808080", alt_color))))
  
  # extract team-specific info
  home_info <- team_info %>% filter(school == home)
  away_info <- team_info %>% filter(school == away)
  
  # colors and logos
  home_color <- home_info$color
  home_alt_color <- home_info$alt_color
  home_logo <- home_info$logo
  
  away_color <- away_info$alt_color
  away_alt_color <- away_info$color
  away_logo <- away_info$logo
  
  # get data ####
  
  # get player usage and production data
  df_players <- data.frame(cfbd_player_usage(year = 2021, home) %>%
                             left_join(cfbd_metrics_ppa_players_season(2021, home)) %>%
                             rbind(
                               cfbd_player_usage(year = 2021, away) %>%
                                 left_join(cfbd_metrics_ppa_players_season(2021, away))) %>%
                             select(season, team, name, position, usg_overall, total_PPA_all, total_PPA_pass, total_PPA_rush))
  
  # set NA values to 0
  df_players[is.na(df_players)] <- 0
  
  # home table ####
  table_home_players <- gt((df_players %>% filter(team == home) %>%
                              arrange(-usg_overall) %>% select(-season, -team))[1:10,]) %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts())) %>%
    tab_header(
      title = paste0(home, " Offensive Contributors, 2021")) %>%
    cols_label(name = "Name",
               position = "Pos",
               usg_overall = "Usage",
               total_PPA_all = "Total EPA",
               total_PPA_pass = "Pass EPA",
               total_PPA_rush = "Rush EPA") %>%
    fmt_percent(columns = usg_overall, decimals = 0) %>%
    fmt_number(columns = c(total_PPA_all,total_PPA_pass,total_PPA_rush), decimals = 1) %>%
    #tab_source_note(source_note = "Red: not returning") %>%
    tab_options(
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = home_alt_color,
      table.border.top.width = px(3),
      table.border.top.color = home_alt_color,
      table.border.bottom.width = px(3),
      table.border.bottom.color = home_alt_color,
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      footnotes.font.size = 12,
      heading.align = "center",
      table.width =px(500)) %>%
    #gt_highlight_rows(
    #  columns = gt::everything(),
    #  rows = c(1, 4, 7, 8), # This row denotes the players that will not be returning, if applicable -- can be commented out
    #  fill = "#FF7F7F",
    #  alpha = 0.5,
    #  font_weight = "normal",
    #  target_col = c()) %>%
    tab_style(
      locations = cells_title(groups = c("title")),
      style = list(
        cell_fill(color = home_color),
        cell_text(weight = "bold", color = home_alt_color)))
  
  # away table ####
  table_away_players <- gt((df_players %>% filter(team == away) %>%
                              arrange(-usg_overall) %>% select(-season, -team))[1:10,]) %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts())) %>%
    tab_header(
      title = paste0(away," Offensive Contributors, 2021")) %>%
    cols_label(name = "Name",
               position = "Pos",
               usg_overall = "Usage",
               total_PPA_all = "Total EPA",
               total_PPA_pass = "Pass EPA",
               total_PPA_rush = "Rush EPA") %>%
    fmt_percent(columns = usg_overall, decimals = 0) %>%
    fmt_number(columns = c(total_PPA_all,total_PPA_pass,total_PPA_rush), decimals = 1) %>%
    #tab_source_note(source_note = "Red: not returning") %>%
    tab_options(
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = away_alt_color,
      table.border.top.width = px(3),
      table.border.top.color = away_alt_color,
      table.border.bottom.width = px(3),
      table.border.bottom.color = away_alt_color,
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      footnotes.font.size = 12,
      heading.align = "center",
      table.width =px(500)) %>%
    #gt_highlight_rows(
    #  columns = gt::everything(),
    #  rows = c(1, 2, 3, 7, 8), # This row denotes the players that will not be returning, if applicable -- can be commented out
    #  fill = "#FF7F7F",
    #  alpha = 0.5,
    #  font_weight = "normal",
    #  target_col = c()) %>%
    tab_style(
      locations = cells_title(groups = c("title")),
      style = list(
        cell_fill(color = away_color),
        cell_text(weight = "bold", color = away_alt_color)))
  
  # save tables ####
  gtsave(table_home_players, "pic_table_home_players.png")
  gtsave(table_away_players, "pic_table_away_players.png")
  
}
