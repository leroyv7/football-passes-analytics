# Elite Possession Teams Dashboard

An interactive football analytics dashboard exploring how elite
possession-based teams used vertical passing lanes and progressive
passing structures during the 2015/16 domestic season.

The dashboard compares tactical passing tendencies across teams from:
- Premier League
- Bundesliga
- Serie A
- Ligue 1
- La Liga

**Live dashboard:** https://leroyv7.github.io/football-passes-analytics/

Built with R, Shiny, ggplot2, Plotly, and `ggsoccer`, published to
GitHub Pages via [Shinylive](https://shinylive.io/r/) — the app runs
entirely in the visitor's browser, with no server.

## Data

Event-level football data sourced from StatsBomb open data via:
- `StatsBombR`
- `ggsoccer`
- `tidyverse`

The project analyses elite possession-oriented teams including:
- Bayern Munich
- Barcelona
- Manchester City
- Arsenal
- Juventus
- Paris Saint-Germain
- Real Madrid
- Roma
- Lyon
- Borussia Mönchengladbach

## Repository Layout

- `app/` — the Shiny app (`app.R` and cleaned `.rds` datasets)
- `docs/` — the Shinylive build served by GitHub Pages
- `scripts/` — data cleaning and preprocessing scripts
- `data/` — raw and cleaned football event data
