A method of using advanced stats to visualize college football matchups.

`cfb_adj_epa` is a function that uses [Bud Davis's](https://blog.collegefootballdata.com/opponent-adjusted-stats-ridge-regression/) ridge regression technique to adjust EPA for opponent.

```{r f_cfb_adj_epa}

df <- f_cfb_adj_epa(season = 2021, thru_week = 15)

```
Stats ending in "60" are "per-game" stats that assume ~60 plays/game.

`f_overview_plot` uses `cf_fb_adj_epa`, along with more advanced stats from https://collegefootballdata.com/, to visualize a specific college football matchup.

```{r f_overview_plot}

f_overview_plot(season = 2022, week = 1, home = "Pittsburgh", away = "West Virginia", data = df)

```

`f_player_tables` displays and saves the top usage and production values for the top players on each team.

```{r f_player_tables}

f_player_tables(home = "Georgia", away = "Oregon", season = 2021)

```
