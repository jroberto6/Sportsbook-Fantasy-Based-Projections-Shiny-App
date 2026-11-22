# Props-Based Fantasy Football Projections

Weekly NFL fantasy projections built from sportsbook player props, shown in a Shiny app that ranks players by position under standard, half PPR, or PPR scoring.

**Idea:** Sportsbook lines are more accurate than free fantasy-site projections. This project turns those lines into expected fantasy points and compares them against ESPN and CBS.
![Rankings tab](images/app rankings.png)
![Lineup tool](images/my lineup.png)

## What the app does

- Rankings by QB, RB, WR, TE, and FLEX (RB/WR/TE), with a scoring toggle between PPR, Half PPR, and Standard
- Team logos, player search, and the underlying stat projections for each player
- A lineup tool: pick your roster and get the optimal starters for the chosen scoring format
- A "last updated" timestamp showing when the props were pulled

## Method

1. **Pull props** for each game from The Odds API (FanDuel for most markets, DraftKings for QB interceptions).
2. **Remove the vig.** Convert American odds to implied probabilities, then normalize each over/under pair so it sums to 1.
3. **Convert lines to means.** A posted line is closer to a median than a mean, and yardage is right-skewed. For yardage props, fit a gamma distribution and solve for the mean that makes P(over the line) match the de-vigged probability. For count props (receptions, passing TDs, interceptions), do the same with a Poisson.
4. **Anytime TD to expected TDs.** The market gives P(at least one TD). Under a Poisson, expected TDs = -log(1 - p).
5. **Score.** Expected stats go through the scoring formula (0.04 per passing yard, 4 per passing TD, -2 per INT, 0.1 per rush or receiving yard, 6 per TD, and 0, 0.5, or 1 per reception).
6. **Snapshot every week.** Historical props are not available afterward, so each pull is saved with a timestamp for later backtesting.

## Known limitations

- The gamma shape parameter (`k = 3`) is a placeholder shared by all positions. It is being estimated from game logs by position, since QB passing yards are steadier than WR receiving yards.
- Anytime TD prices carry vig that cannot be removed (the market only has a "Yes" side), so expected TDs run slightly high.
- Players need at least one yardage prop to be ranked. A missing secondary market (for example, a receptions line) counts as zero.
- Fumbles, 2-point conversions, kickers, and defenses are not projected.
- Projections reflect the last pull and do not update for later injury news.

## Backtest

I just made the app, so the backtest results will be coming soon. The plan: score projections from props, ESPN, and CBS with the same formula, compare each against actual fantasy points from nflreadr on the players all sources projected, and report MAE, bias, and rank accuracy by position. Sample sizes will be stated alongside every result.

## Files

- `functions.R`: odds math, API calls, projection building, consensus scoring, lineup optimizer
- `weekly_pull.R`: the weekly run (props, consensus, projections, saved snapshots)
- `fantasy_app.R`: the Shiny app
- `data/teams.csv`: team abbreviations and logo links, included in the repo

## Setup

1. Install the packages: `tidyverse`, `httr2`, `jsonlite`, `nflreadr`, `shiny`, `reactable`, `usethis`, and `ffanalytics` (from GitHub: `remotes::install_github("FantasyFootballAnalytics/ffanalytics")`).
2. Get a free API key from [the-odds-api.com](https://the-odds-api.com). Run `usethis::edit_r_environ()`, add the line `ODDS_API_KEY=your_key_here`, save, and restart R. Never put the key in a script.
3. Run `weekly_pull.R` from the project folder. It pulls props and consensus projections and writes the projections file to `data/proj/`.
4. Start the app with `shiny::runApp("fantasy_app.R")`. RStudio's Run App button also works.

`data/teams.csv` is already in the repo. To rebuild it: `nflreadr::load_teams() |> dplyr::select(team = team_abbr, team_logo = team_logo_espn) |> readr::write_csv("data/teams.csv")`.

The free Odds API plan allows 500 credits per month. A full slate costs about 90 credits, so plan on a few pulls a month.

## Data

Raw props and scraped projections are not included in this repo. They come from third-party sources with their own terms. Check those terms before redistributing any data.

NOT BETTING ADVICE!!
