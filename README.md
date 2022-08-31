# CFB Matchup Overview
A method of using advanced stats to visualize college football matchups.

#### Getting opponent-adjsuted data
`f_cfb_adj_epa` is a function that uses [Bud Davis's](https://blog.collegefootballdata.com/opponent-adjusted-stats-ridge-regression/) ridge regression technique to adjust EPA for opponent.

```{r f_cfb_adj_epa}
df <- f_cfb_adj_epa(season = 2021, thru_week = 15)
```

#### Visualizing a matchup
`f_overview_plot` uses `cf_fb_adj_epa`, along with more advanced stats from https://collegefootballdata.com/, to visualize a specific college football matchup.

```{r f_overview_plot}
f_overview_plot(season = 2022, week = 1, home = "Pittsburgh", away = "West Virginia", data = df)
```

![pic_overview](https://user-images.githubusercontent.com/80285759/187586861-e04228d4-04ff-420b-9759-bb2790e3c285.png)

#### Showing player-specific metrics
`f_player_tables` displays and saves the top usage and production values for the top players on each team.
```{r f_player_tables}

f_player_tables(home = "Georgia", away = "Oregon", season = 2021)
```

![pic_table_home_players](https://user-images.githubusercontent.com/80285759/187586727-f169d154-b9c0-4aad-8f57-4b677d133e5f.png)
![pic_table_away_players](https://user-images.githubusercontent.com/80285759/187586729-1a7aa116-623d-4c56-9396-7faff974031e.png)
