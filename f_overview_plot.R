# advanced stats overview for college football matchups

api_key <- ""


f_overview_plot <- function(season = 2022, week = 1, home = "Pittsburgh", away = "West Virginia", data = NULL) {
  # SETUP ####
  
  # load packages
  require(tidyverse)
  require(ggplot2)
  require(ggrepel)
  require(ggimage)
  require(scales)
  require(glmnet)
  require(ggforce)
  require(magick)
  require(gt)
  require(fastDummies)
  require(showtext)
  font_add_google("Chivo")
  showtext_auto()
  theme_set(theme_gray(base_family = "Chivo"))
  require(ggthemes)
  
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
  
  away_color <- away_info$color
  away_alt_color <- away_info$alt_color
  away_logo <- away_info$logo
  
  # GET DATA ####
  
  # look at entirety of previous season if it's the first game of the season
  if (week < 2) {
    season = season - 1
    thru_week = 20
  # can only use data through the previous week
  } else {
      thru_week = week - 1
    }
  
  # if data already exists, use that
  if (!is.null(data)) { 
    df_adj_epa <- data
  } else {
    df_adj_epa <- cfb_adj_epa(season, thru_week)
  }
  
  df_plot <- df_adj_epa %>%
    
    # join with front 7 havoc data
    left_join(cfbd_stats_season_advanced(2021) %>%
                select(team, off_havoc_front_seven, def_havoc_front_seven)) %>%
    # standardize data
    mutate(
      o_60 = (adjOff_60 - mean(adjOff_60)) / sd(adjOff_60) + 2.5,
      d_60 = -(adjDef_60 - mean(adjDef_60)) / sd(adjDef_60) + 2.5,
      
      p_o_60 = (adjOff_pass_60 - mean(adjOff_pass_60)) / sd(adjOff_pass_60) + 2.5,
      p_d_60 = -(adjDef_pass_60 - mean(adjDef_pass_60)) / sd(adjDef_pass_60) + 2.5,
      
      r_o_60 = (adjOff_rush_60 - mean(adjOff_rush_60)) / sd(adjOff_rush_60) + 2.5,
      r_d_60 = -(adjDef_rush_60 - mean(adjDef_rush_60)) / sd(adjDef_rush_60) + 2.5,
      
      o_havoc = -(off_havoc_front_seven - mean(off_havoc_front_seven)) / sd(off_havoc_front_seven) + 2.5,
      d_havoc = (def_havoc_front_seven - mean(def_havoc_front_seven)) / sd(def_havoc_front_seven) + 2.5) %>%
    select(team, o_60, d_60, p_o_60, p_d_60, r_o_60, r_d_60, o_havoc, d_havoc) %>%
    # get percentiles
    mutate(
      o_60_pnorm = round(pnorm(o_60, mean(o_60), sd(o_60))*100),
      d_60_pnorm = round(pnorm(d_60, mean(d_60), sd(d_60))*100),
      p_o_60_pnorm = round(pnorm(p_o_60, mean(p_o_60), sd(p_o_60))*100),
      p_d_60_pnorm = round(pnorm(p_d_60, mean(p_d_60), sd(p_d_60))*100),
      r_o_60_pnorm = round(pnorm(r_o_60, mean(r_o_60), sd(r_o_60))*100),
      r_d_60_pnorm = round(pnorm(r_d_60, mean(r_d_60), sd(r_d_60))*100),
      o_havoc_pnorm = round(pnorm(o_havoc, mean(o_havoc), sd(o_havoc))*100),
      d_havoc_pnorm = round(pnorm(d_havoc, mean(d_havoc), sd(d_havoc))*100))
  
  # team specific plot data
  df_home_plot <- df_plot %>% filter(team == home)
  df_away_plot <- df_plot %>% filter(team == away)
  
  # density data frames for each variable
  
  # overall
  o_60_density <- with(density(df_plot$o_60), data.frame(x, y))
  d_60_density <- with(density(df_plot$d_60), data.frame(x, y))
  # passing
  p_o_60_density <- with(density(df_plot$p_o_60), data.frame(x, y))
  p_d_60_density <- with(density(df_plot$p_d_60), data.frame(x, y))
  # rushing
  r_o_60_density <- with(density(df_plot$r_o_60), data.frame(x, y))
  r_d_60_density <- with(density(df_plot$r_d_60), data.frame(x, y))
  # havoc
  o_havoc_density <- with(density(df_plot$o_havoc), data.frame(x, y))
  d_havoc_density <- with(density(df_plot$d_havoc), data.frame(x, y))
  
  # create transparent function for background logos
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.3*a", channel = "alpha")
  }
  
  # number endings ####
  endings <- data.frame(
    "number" = seq(0, 100)) %>%
    mutate(
      ending = ifelse((number + 9) %% 10 == 0, "st",
                      ifelse((number + 8) %% 10 == 0, "nd",
                             ifelse((number + 7) %% 10 == 0, "rd","th")))) %>%
    mutate(ending = ifelse(number %in% c(0, 11, 12, 13), "th", ending))
  
  # ggplot ####
  ggplot() +
    
    # background team logos ####
    # size can be adjusted
    geom_image(aes(x = 7.5, y = -.75), image = away_logo, size = .25, asp = 40/25, image_fun = transparent) +
    geom_image(aes(x = 7.5, y = -2.25), image = home_logo, size = .25, asp = 40/25, image_fun = transparent) +
    
    # offense ####
    geom_segment(data = o_60_density, mapping = aes(x = x, xend = x, y = 0, yend = y, color = x)) +
    
    geom_area(data = o_60_density %>% filter(x < df_home_plot$o_60 + .1, x > df_home_plot$o_60 - .1),
              mapping = aes(seq(df_home_plot$o_60 - .1, df_home_plot$o_60 + .1,
                                length = NROW(o_60_density %>% filter(x < df_home_plot$o_60 + .1, x > df_home_plot$o_60 - .1))), y),
              fill = home_alt_color) +
    geom_area(data = o_60_density %>% filter(x < df_home_plot$o_60 + .06, x > df_home_plot$o_60 - .06),
              mapping = aes(seq(df_home_plot$o_60 - .06, df_home_plot$o_60 + .06,
                                length = NROW(o_60_density %>% filter(x < df_home_plot$o_60 + .06, x > df_home_plot$o_60 - .06))), y),
              fill = home_color) +
    
    geom_area(data = o_60_density %>% filter(x < df_away_plot$o_60 + .1, x > df_away_plot$o_60 - .1),
              mapping = aes(seq(df_away_plot$o_60 - .1, df_away_plot$o_60 + .1,
                                length = NROW(o_60_density %>% filter(x < df_away_plot$o_60 + .1, x > df_away_plot$o_60 - .1))), y),
              fill = away_alt_color) +
    geom_area(data = o_60_density %>% filter(x < df_away_plot$o_60 + .06, x > df_away_plot$o_60 - .06),
              mapping = aes(seq(df_away_plot$o_60 - .06, df_away_plot$o_60 + .06,
                                length = NROW(o_60_density %>% filter(x < df_away_plot$o_60 + .06, x > df_away_plot$o_60 - .06))), y),
              fill = away_color) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(o_60 == max(o_60)),
                    aes(x = o_60, y = -.05, label = paste0(round(o_60_pnorm),
                                                           endings[round(o_60_pnorm) + 1,2],
                                                           " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(o_60 == min(o_60)),
                    aes(x = o_60, y = -.05, label = paste0(round(o_60_pnorm),
                                                           endings[round(o_60_pnorm) + 1,2],
                                                           " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = o_60_density, aes(x, y = y), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .5, label = "Total Offense"),
              hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .3, label = "(aEPA)"),
              hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # defense ####
    geom_segment(data = d_60_density, mapping = aes(x = x + 10, xend = x + 10, y = 0, yend = y, color = x)) +
    
    geom_area(data = d_60_density %>% filter(x < df_home_plot$d_60 + .1, x > df_home_plot$d_60 - .1),
              mapping = aes(seq(df_home_plot$d_60 - .1, df_home_plot$d_60 + .1,
                                length = NROW(d_60_density %>% filter(x < df_home_plot$d_60 + .1, x > df_home_plot$d_60 - .1))) + 10, y),
              fill = home_alt_color) +
    
    geom_area(data = d_60_density %>% filter(x < df_home_plot$d_60 + .06, x > df_home_plot$d_60 - .06),
              mapping = aes(seq(df_home_plot$d_60 - .06, df_home_plot$d_60 + .06,
                                length = NROW(d_60_density %>% filter(x < df_home_plot$d_60 + .06, x > df_home_plot$d_60 - .06))) + 10, y),
              fill = home_color) +
    
    geom_area(data = d_60_density %>% filter(x < df_away_plot$d_60 + .1, x > df_away_plot$d_60 - .1),
              mapping = aes(seq(df_away_plot$d_60 - .1, df_away_plot$d_60 + .1,
                                length = NROW(d_60_density %>% filter(x < df_away_plot$d_60 + .1, x > df_away_plot$d_60 - .1))) + 10, y),
              fill = away_alt_color) +
    
    geom_area(data = d_60_density %>% filter(x < df_away_plot$d_60 + .06, x > df_away_plot$d_60 - .06),
              mapping = aes(seq(df_away_plot$d_60 - .06, df_away_plot$d_60 + .06,
                                length = NROW(d_60_density %>% filter(x < df_away_plot$d_60 + .06, x > df_away_plot$d_60 - .06))) + 10, y),
              fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(d_60 == max(d_60)),
                    aes(x = d_60 + 10, y = -.05, label = paste0(round(d_60_pnorm),
                                                                endings[round(d_60_pnorm) + 1,2],
                                                                " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(d_60 == min(d_60)),
                    aes(x = d_60 + 10, y = -.05, label = paste0(round(d_60_pnorm),
                                                                endings[round(d_60_pnorm) + 1,2],
                                                                " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = d_60_density, aes(x + 10, y = y), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = max(d_60_density$x, p_d_60_density$x, r_d_60_density$x, d_havoc_density$x) + 10, y = .4, label = "Total Defense
                (aEPA)"), hjust = 1, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # passing offense ####
    geom_segment(data = p_o_60_density, mapping = aes(x = x, xend = x, y = -1, yend = y - 1, color = x)) +
    
    geom_ribbon(data = p_o_60_density %>% filter(x < df_home_plot$p_o_60 + .1, x > df_home_plot$p_o_60 - .1),
                mapping = aes(seq(df_home_plot$p_o_60 - .1, df_home_plot$p_o_60 + .1,
                                  length = NROW(p_o_60_density %>% filter(x < df_home_plot$p_o_60 + .1, x > df_home_plot$p_o_60 - .1))),
                              ymin = -1, ymax = y - 1),
                fill = home_alt_color) +
    geom_ribbon(data = p_o_60_density %>% filter(x < df_home_plot$p_o_60 + .06, x > df_home_plot$p_o_60 - .06),
                mapping = aes(seq(df_home_plot$p_o_60 - .06, df_home_plot$p_o_60 + .06,
                                  length = NROW(p_o_60_density %>% filter(x < df_home_plot$p_o_60 + .06, x > df_home_plot$p_o_60 - .06))),
                              ymin = -1, ymax = y - 1),
                fill = home_color) +
    geom_ribbon(data = p_o_60_density %>% filter(x < df_away_plot$p_o_60 + .1, x > df_away_plot$p_o_60 - .1),
                mapping = aes(seq(df_away_plot$p_o_60 - .1, df_away_plot$p_o_60 + .1,
                                  length = NROW(p_o_60_density %>% filter(x < df_away_plot$p_o_60 + .1, x > df_away_plot$p_o_60 - .1))),
                              ymin = -1, ymax = y - 1),
                fill = away_alt_color) +
    geom_ribbon(data = p_o_60_density %>% filter(x < df_away_plot$p_o_60 + .06, x > df_away_plot$p_o_60 - .06),
                mapping = aes(seq(df_away_plot$p_o_60 - .06, df_away_plot$p_o_60 + .06,
                                  length = NROW(p_o_60_density %>% filter(x < df_away_plot$p_o_60 + .06, x > df_away_plot$p_o_60 - .06))),
                              ymin = -1, ymax = y - 1),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(p_o_60 == max(p_o_60)),
                    aes(x = p_o_60, y = -1.05, label = paste0(round(p_o_60_pnorm),
                                                              endings[round(p_o_60_pnorm) + 1,2],
                                                              " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(p_o_60 == min(p_o_60)),
                    aes(x = p_o_60, y = -1.05, label = paste0(round(p_o_60_pnorm),
                                                              endings[round(p_o_60_pnorm) + 1,2],
                                                              " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = p_o_60_density, aes(x, y = y - 1), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .5 - 1, label = "Passing Offense"), hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .3 - 1, label = "(aEPA)"), hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    
    
    # passing defense ####
    geom_segment(data = p_d_60_density, mapping = aes(x = x + 10, xend = x + 10, y = -1, yend = y - 1, color = x)) +
    
    geom_ribbon(data = p_d_60_density %>% filter(x < df_home_plot$p_d_60 + .1, x > df_home_plot$p_d_60 - .1),
                mapping = aes(seq(df_home_plot$p_d_60 - .1, df_home_plot$p_d_60 + .1,
                                  length = NROW(p_d_60_density %>% filter(x < df_home_plot$p_d_60 + .1, x > df_home_plot$p_d_60 - .1))) + 10,
                              ymin = -1, ymax = y - 1),
                fill = home_alt_color) +
    geom_ribbon(data = p_d_60_density %>% filter(x < df_home_plot$p_d_60 + .06, x > df_home_plot$p_d_60 - .06),
                mapping = aes(seq(df_home_plot$p_d_60 - .06, df_home_plot$p_d_60 + .06,
                                  length = NROW(p_d_60_density %>% filter(x < df_home_plot$p_d_60 + .06, x > df_home_plot$p_d_60 - .06))) + 10,
                              ymin = -1, ymax = y - 1),
                fill = home_color) +
    geom_ribbon(data = p_d_60_density %>% filter(x < df_away_plot$p_d_60 + .1, x > df_away_plot$p_d_60 - .1),
                mapping = aes(seq(df_away_plot$p_d_60 - .1, df_away_plot$p_d_60 + .1,
                                  length = NROW(p_d_60_density %>% filter(x < df_away_plot$p_d_60 + .1, x > df_away_plot$p_d_60 - .1))) + 10,
                              ymin = -1, ymax = y - 1),
                fill = away_alt_color) +
    geom_ribbon(data = p_d_60_density %>% filter(x < df_away_plot$p_d_60 + .06, x > df_away_plot$p_d_60 - .06),
                mapping = aes(seq(df_away_plot$p_d_60 - .06, df_away_plot$p_d_60 + .06,
                                  length = NROW(p_d_60_density %>% filter(x < df_away_plot$p_d_60 + .06, x > df_away_plot$p_d_60 - .06))) + 10,
                              ymin = -1, ymax = y - 1),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(p_d_60 == max(p_d_60)),
                    aes(x = p_d_60 + 10, y = -1.05, label = paste0(round(p_d_60_pnorm),
                                                                   endings[round(p_d_60_pnorm) + 1,2],
                                                                   " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(p_d_60 == min(p_d_60)),
                    aes(x = p_d_60 + 10, y = -1.05, label = paste0(round(p_d_60_pnorm),
                                                                   endings[round(p_d_60_pnorm) + 1,2],
                                                                   " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = p_d_60_density, aes(x + 10, y = y - 1), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = max(d_60_density$x, p_d_60_density$x, r_d_60_density$x, d_havoc_density$x) + 10, y = .4 - 1, label = "Passing Defense
                (aEPA)"), hjust = 1, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # rushing offense ####
    geom_segment(data = r_o_60_density, mapping = aes(x = x , xend = x, y = -2, yend = y - 2, color = x)) +
    
    geom_ribbon(data = r_o_60_density %>% filter(x < df_home_plot$r_o_60 + .1, x > df_home_plot$r_o_60 - .1),
                mapping = aes(seq(df_home_plot$r_o_60 - .1, df_home_plot$r_o_60 + .1,
                                  length = NROW(r_o_60_density %>% filter(x < df_home_plot$r_o_60 + .1, x > df_home_plot$r_o_60 - .1))),
                              ymin = -2, ymax = y - 2),
                fill = home_alt_color) +
    geom_ribbon(data = r_o_60_density %>% filter(x < df_home_plot$r_o_60 + .06, x > df_home_plot$r_o_60 - .06),
                mapping = aes(seq(df_home_plot$r_o_60 - .06, df_home_plot$r_o_60 + .06,
                                  length = NROW(r_o_60_density %>% filter(x < df_home_plot$r_o_60 + .06, x > df_home_plot$r_o_60 - .06))),
                              ymin = -2, ymax = y - 2),
                fill = home_color) +
    geom_ribbon(data = r_o_60_density %>% filter(x < df_away_plot$r_o_60 + .1, x > df_away_plot$r_o_60 - .1),
                mapping = aes(seq(df_away_plot$r_o_60 - .1, df_away_plot$r_o_60 + .1,
                                  length = NROW(r_o_60_density %>% filter(x < df_away_plot$r_o_60 + .1, x > df_away_plot$r_o_60 - .1))),
                              ymin = -2, ymax = y - 2),
                fill = away_alt_color) +
    geom_ribbon(data = r_o_60_density %>% filter(x < df_away_plot$r_o_60 + .06, x > df_away_plot$r_o_60 - .06),
                mapping = aes(seq(df_away_plot$r_o_60 - .06, df_away_plot$r_o_60 + .06,
                                  length = NROW(r_o_60_density %>% filter(x < df_away_plot$r_o_60 + .06, x > df_away_plot$r_o_60 - .06))),
                              ymin = -2, ymax = y - 2),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(r_o_60 == max(r_o_60)),
                    aes(x = r_o_60, y = -2.05, label = paste0(round(r_o_60_pnorm),
                                                              endings[round(r_o_60_pnorm) + 1,2],
                                                              " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(r_o_60 == min(r_o_60)),
                    aes(x = r_o_60, y = -2.05, label = paste0(round(r_o_60_pnorm),
                                                              endings[round(r_o_60_pnorm) + 1,2],
                                                              " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = r_o_60_density, aes(x, y = y - 2), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .5 - 2, label = "Rushing Offense"), hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .3 - 2, label = "(aEPA)"), hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # rushing defense ####
    geom_segment(data = r_d_60_density, mapping = aes(x = x + 10, xend = x + 10, y = -2, yend = y - 2, color = x)) +
    
    geom_ribbon(data = r_d_60_density %>% filter(x < df_home_plot$r_d_60 + .1, x > df_home_plot$r_d_60 - .1),
                mapping = aes(seq(df_home_plot$r_d_60 - .1, df_home_plot$r_d_60 + .1,
                                  length = NROW(r_d_60_density %>% filter(x < df_home_plot$r_d_60 + .1, x > df_home_plot$r_d_60 - .1))) + 10,
                              ymin = -2, ymax = y - 2),
                fill = home_alt_color) +
    geom_ribbon(data = r_d_60_density %>% filter(x < df_home_plot$r_d_60 + .06, x > df_home_plot$r_d_60 - .06),
                mapping = aes(seq(df_home_plot$r_d_60 - .06, df_home_plot$r_d_60 + .06,
                                  length = NROW(r_d_60_density %>% filter(x < df_home_plot$r_d_60 + .06, x > df_home_plot$r_d_60 - .06))) + 10,
                              ymin = -2, ymax = y - 2),
                fill = home_color) +
    geom_ribbon(data = r_d_60_density %>% filter(x < df_away_plot$r_d_60 + .1, x > df_away_plot$r_d_60 - .1),
                mapping = aes(seq(df_away_plot$r_d_60 - .1, df_away_plot$r_d_60 + .1,
                                  length = NROW(r_d_60_density %>% filter(x < df_away_plot$r_d_60 + .1, x > df_away_plot$r_d_60 - .1))) + 10,
                              ymin = -2, ymax = y - 2),
                fill = away_alt_color) +
    geom_ribbon(data = r_d_60_density %>% filter(x < df_away_plot$r_d_60 + .06, x > df_away_plot$r_d_60 - .06),
                mapping = aes(seq(df_away_plot$r_d_60 - .06, df_away_plot$r_d_60 + .06,
                                  length = NROW(r_d_60_density %>% filter(x < df_away_plot$r_d_60 + .06, x > df_away_plot$r_d_60 - .06))) + 10,
                              ymin = -2, ymax = y - 2),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(r_d_60 == max(r_d_60)),
                    aes(x = r_d_60 + 10, y = -2.05, label = paste0(round(r_d_60_pnorm),
                                                                   endings[round(r_d_60_pnorm) + 1,2],
                                                                   " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(r_d_60 == min(r_d_60)),
                    aes(x = r_d_60 + 10, y = -2.05, label = paste0(round(r_d_60_pnorm),
                                                                   endings[round(r_d_60_pnorm) + 1,2],
                                                                   " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = r_d_60_density, aes(x + 10, y = y - 2), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = max(d_60_density$x, p_d_60_density$x, r_d_60_density$x, d_havoc_density$x) + 10, y = .4 - 2, label = "Rushing Defense
                (aEPA)"), hjust = 1, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # havoc allowed ####
    geom_segment(data = o_havoc_density, mapping = aes(x = x , xend = x, y = -3, yend = y - 3, color = x)) +
    
    geom_ribbon(data = o_havoc_density %>% filter(x < df_home_plot$o_havoc + .1, x > df_home_plot$o_havoc - .1),
                mapping = aes(seq(df_home_plot$o_havoc - .1, df_home_plot$o_havoc + .1,
                                  length = NROW(o_havoc_density %>% filter(x < df_home_plot$o_havoc + .1, x > df_home_plot$o_havoc - .1))),
                              ymin = -3, ymax = y - 3),
                fill = home_alt_color) +
    geom_ribbon(data = o_havoc_density %>% filter(x < df_home_plot$o_havoc + .06, x > df_home_plot$o_havoc - .06),
                mapping = aes(seq(df_home_plot$o_havoc - .06, df_home_plot$o_havoc + .06,
                                  length = NROW(o_havoc_density %>% filter(x < df_home_plot$o_havoc + .06, x > df_home_plot$o_havoc - .06))),
                              ymin = -3, ymax = y - 3),
                fill = home_color) +
    geom_ribbon(data = o_havoc_density %>% filter(x < df_away_plot$o_havoc + .1, x > df_away_plot$o_havoc - .1),
                mapping = aes(seq(df_away_plot$o_havoc - .1, df_away_plot$o_havoc + .1,
                                  length = NROW(o_havoc_density %>% filter(x < df_away_plot$o_havoc + .1, x > df_away_plot$o_havoc - .1))),
                              ymin = -3, ymax = y - 3),
                fill = away_alt_color) +
    geom_ribbon(data = o_havoc_density %>% filter(x < df_away_plot$o_havoc + .06, x > df_away_plot$o_havoc - .06),
                mapping = aes(seq(df_away_plot$o_havoc - .06, df_away_plot$o_havoc + .06,
                                  length = NROW(o_havoc_density %>% filter(x < df_away_plot$o_havoc + .06, x > df_away_plot$o_havoc - .06))),
                              ymin = -3, ymax = y - 3),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(o_havoc == max(o_havoc)),
                    aes(x = o_havoc, y = -3.05, label = paste0(round(o_havoc_pnorm),
                                                               endings[round(o_havoc_pnorm) + 1,2],
                                                               " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(o_havoc == min(o_havoc)),
                    aes(x = o_havoc, y = -3.05, label = paste0(round(o_havoc_pnorm),
                                                               endings[round(o_havoc_pnorm) + 1,2],
                                                               " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = o_havoc_density, aes(x, y = y - 3), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .5 - 3, label = "Front Seven"),
              hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    geom_text(aes(x = min(o_60_density$x, p_o_60_density$x, r_o_60_density$x, o_havoc_density$x), y = .3 - 3, label = "Havoc Allowed"), hjust = 0, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # havoc induced ####
    geom_segment(data = d_havoc_density, mapping = aes(x = x + 10, xend = x + 10, y = -3, yend = y - 3, color = x)) +
    
    geom_ribbon(data = d_havoc_density %>% filter(x < df_home_plot$d_havoc + .1, x > df_home_plot$d_havoc - .1),
                mapping = aes(seq(df_home_plot$d_havoc - .1, df_home_plot$d_havoc + .1,
                                  length = NROW(d_havoc_density %>% filter(x < df_home_plot$d_havoc + .1, x > df_home_plot$d_havoc - .1))) + 10,
                              ymin = -3, ymax = y - 3),
                fill = home_alt_color) +
    geom_ribbon(data = d_havoc_density %>% filter(x < df_home_plot$d_havoc + .06, x > df_home_plot$d_havoc - .06),
                mapping = aes(seq(df_home_plot$d_havoc - .06, df_home_plot$d_havoc + .06,
                                  length = NROW(d_havoc_density %>% filter(x < df_home_plot$d_havoc + .06, x > df_home_plot$d_havoc - .06))) + 10,
                              ymin = -3, ymax = y - 3),
                fill = home_color) +
    geom_ribbon(data = d_havoc_density %>% filter(x < df_away_plot$d_havoc + .1, x > df_away_plot$d_havoc - .1),
                mapping = aes(seq(df_away_plot$d_havoc - .1, df_away_plot$d_havoc + .1,
                                  length = NROW(d_havoc_density %>% filter(x < df_away_plot$d_havoc + .1, x > df_away_plot$d_havoc - .1))) + 10,
                              ymin = -3, ymax = y - 3),
                fill = away_alt_color) +
    geom_ribbon(data = d_havoc_density %>% filter(x < df_away_plot$d_havoc + .06, x > df_away_plot$d_havoc - .06),
                mapping = aes(seq(df_away_plot$d_havoc - .06, df_away_plot$d_havoc + .06,
                                  length = NROW(d_havoc_density %>% filter(x < df_away_plot$d_havoc + .06, x > df_away_plot$d_havoc - .06))) + 10,
                              ymin = -3, ymax = y - 3),
                fill = away_color) +
    
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(d_havoc == max(d_havoc)),
                    aes(x = d_havoc + 10, y = -3.05, label = paste0(round(d_havoc_pnorm),
                                                                    endings[round(d_havoc_pnorm) + 1,2],
                                                                    " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = 0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    geom_text_repel(data = rbind(df_home_plot, df_away_plot) %>% filter(d_havoc == min(d_havoc)),
                    aes(x = d_havoc + 10, y = -3.05, label = paste0(round(d_havoc_pnorm),
                                                                    endings[round(d_havoc_pnorm) + 1,2],
                                                                    " %tile")),
                    family = "Chivo", fontface = "bold", nudge_x = -0.6, nudge_y = -.01, min.segment.length = 0, size = 6) +
    
    geom_line(data = d_havoc_density, aes(x + 10, y = y - 3), color = "#4d4d4d", size = 1.5) +
    geom_text(aes(x = max(d_60_density$x, p_d_60_density$x, r_d_60_density$x, d_havoc_density$x) + 10, y = .4 - 3, label = "Front Seven
                Havoc"), hjust = 1, family = "Chivo", fontface = "bold", size = 7.5) +
    
    # legend ####
    geom_segment(aes(x = 6.35, xend = 7.4, y = .25, yend = .25), color = home_alt_color, size = 6) +
    geom_segment(aes(x = 6.35, xend = 7.4, y = .25, yend = .25), color = home_color, size = 4) +
    geom_text(aes(x = 7.5, y = .5), label = paste0("= ", away), size = 5, family = "Chivo", fontface = "bold", hjust = 0) +
    
    geom_segment(aes(x = 6.35, xend = 7.4, y = .5, yend = .5), color = away_alt_color, size = 6) +
    geom_segment(aes(x = 6.35, xend = 7.4, y = .5, yend = .5), color = away_color, size = 4) +
    geom_text(aes(x = 7.5, y = .25), label = paste0("= ", home), size = 5, family = "Chivo", fontface = "bold", hjust = 0) +
    
    # aesthetics ####
    scale_color_gradient2(high = "#FF7F7F", low = "#ADD8E6", mid = "white", aesthetics = "color", midpoint = 2.5, guide = "none") +
    # labs and theme ####
    labs(title = paste0(away, " vs. ", home, " Overview"),
         subtitle = paste0(season, " Performance vs. Rest of FBS")) +
    theme_void() +
    theme(plot.title = element_text(hjust = .5, size = 24, face = "bold", family = "Chivo"),
          plot.caption = element_text(size = 12, family = "Chivo"),
          plot.subtitle = element_text(hjust = .5, size = 16, family = "Chivo"),
          axis.text = element_blank(),
          plot.background = element_rect(fill = "white",color = "white"),
          aspect.ratio = 25/40)
  
}
