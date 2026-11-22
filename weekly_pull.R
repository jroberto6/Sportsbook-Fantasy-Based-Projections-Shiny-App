source("functions.R")

# ---- settings ----
N_GAMES      <- 14      # set to 2 for a cheap test run
WINDOW_HOURS <- 72      # only games starting within this window
stamp        <- format(Sys.time(), "%Y%m%d_%H%M%S")
pulled_at    <- Sys.time()

for (d in c("data/raw", "data/proj", "data/consensus")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- 1. consensus (free) ----
cons_raw <- pull_consensus()
saveRDS(cons_raw, paste0("data/consensus/cons_raw_", stamp, ".rds"))

cons_scored <- score_cons(bind_rows(cons_raw, .id = "position")) |>
  mutate(pulled_at = pulled_at)
write_csv(cons_scored, paste0("data/consensus/cons_scored_", stamp, ".csv"))

# ---- 2. pick this week's games (free) ----
events <- get_events()
games <- events |>
  keep(function(e) {
    t <- as.POSIXct(e$commence_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    t > Sys.time() & t < Sys.time() + WINDOW_HOURS * 3600
  }) |>
  head(N_GAMES)
length(games)

# ---- 3. props (COSTS CREDITS, about 7 per game) ----
fd_markets <- c("player_pass_yds", "player_pass_tds", "player_rush_yds",
                "player_receptions", "player_reception_yds", "player_anytime_td")

long_all <- map_dfr(games, function(g) {
  fd <- pull_props(g$id, fd_markets, "fanduel")
  dk <- pull_props(g$id, "player_pass_interceptions", "draftkings")
  write_json(list(fd = fd, dk = dk),
             paste0("data/raw/props_", g$id, "_", stamp, ".json"), auto_unbox = TRUE)
  Sys.sleep(1)
  bind_rows(flatten_props(fd), flatten_props(dk)) |> mutate(game_id = g$id)
})

# ---- 4. projections ----
projections <- build_projections(long_all) |>
  add_positions() |>
  mutate(pulled_at = pulled_at)
write_csv(projections, paste0("data/proj/projections_", stamp, ".csv"))

projections |>
  group_by(position) |>
  mutate(rank_ppr = row_number(desc(ppr))) |>
  arrange(position, rank_ppr) |>
  select(position, rank_ppr, player, team, standard, half, ppr) |>
  print(n = 60)