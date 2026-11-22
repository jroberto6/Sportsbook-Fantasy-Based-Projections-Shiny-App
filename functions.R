library(tidyverse)
library(httr2)
library(jsonlite)
library(nflreadr)
library(ffanalytics)

# ---- odds math (concepts 1 to 3) ----
american_to_prob <- function(odds) {
  if_else(odds < 0, -odds / (-odds + 100), 100 / (odds + 100))
}

devig <- function(o, u) {
  american_to_prob(o) / (american_to_prob(o) + american_to_prob(u))
}

mean_from_line_yds <- function(line, p_over, k = 3) {
  f <- function(mu) pgamma(line, shape = k, rate = k / mu, lower.tail = FALSE) - p_over
  tryCatch(uniroot(f, c(1, line * 4))$root, error = function(e) NA_real_)
}

mean_from_line_count <- function(line, p_over) {
  f <- function(lam) ppois(floor(line), lam, lower.tail = FALSE) - p_over
  tryCatch(uniroot(f, c(0.01, max(line * 4, 1)))$root, error = function(e) NA_real_)
}

# ---- Odds API ----
ODDS_BASE <- "https://api.the-odds-api.com/v4/sports/americanfootball_nfl"

get_events <- function() {
  request(paste0(ODDS_BASE, "/events")) |>
    req_url_query(apiKey = Sys.getenv("ODDS_API_KEY")) |>
    req_perform() |>
    resp_body_json()
}

pull_props <- function(game_id, markets, book) {
  r <- request(paste0(ODDS_BASE, "/events/", game_id, "/odds")) |>
    req_url_query(apiKey = Sys.getenv("ODDS_API_KEY"), regions = "us",
                  markets = paste(markets, collapse = ","),
                  oddsFormat = "american", bookmakers = book) |>
    req_perform()
  message(book, " | cost: ", resp_header(r, "x-requests-last"),
          " | remaining: ", resp_header(r, "x-requests-remaining"))
  resp_body_json(r)
}

flatten_props <- function(raw) {
  if (length(raw$bookmakers) == 0) return(tibble())
  map_dfr(raw$bookmakers, function(b) {
    map_dfr(b$markets, function(m) {
      map_dfr(m$outcomes, function(o) {
        tibble(book = b$key, market = m$key, player = o$description,
               side = o$name, line = o$point %||% NA_real_, price = o$price)
      })
    })
  })
}

# ---- props to projections ----
build_projections <- function(long) {
  ou <- long |>
    filter(market != "player_anytime_td") |>
    select(-book) |>
    pivot_wider(names_from = side, values_from = price, names_prefix = "price_") |>
    mutate(p_over = devig(price_Over, price_Under),
           mean = pmap_dbl(list(market, line, p_over), function(m, l, p) {
             if (str_detect(m, "yds")) mean_from_line_yds(l, p) else mean_from_line_count(l, p)
           }))
  
  td <- long |>
    filter(market == "player_anytime_td", side == "Yes") |>
    transmute(player, exp_tds = -log(1 - american_to_prob(price)))
  
  template <- tibble(pass_yds = double(), pass_tds = double(), pass_ints = double(),
                     rush_yds = double(), receptions = double(), rec_yds = double())
  
  wide <- ou |>
    select(player, market, mean) |>
    pivot_wider(names_from = market, values_from = mean) |>
    rename(any_of(c(pass_yds = "player_pass_yds", pass_tds = "player_pass_tds",
                    pass_ints = "player_pass_interceptions", rush_yds = "player_rush_yds",
                    receptions = "player_receptions", rec_yds = "player_reception_yds")))
  
  bind_rows(template, wide) |>
    full_join(td, by = "player") |>
    filter(!str_detect(player, "Defense|Special Teams")) |>
    mutate(
      has_primary  = !is.na(pass_yds) | !is.na(rush_yds) | !is.na(rec_yds),
      int_fallback = !is.na(pass_yds) & is.na(pass_ints),
      pass_ints    = if_else(int_fallback, 0.6, pass_ints),
      base = 0.04 * coalesce(pass_yds, 0) + 4 * coalesce(pass_tds, 0) -
        2 * coalesce(pass_ints, 0) + 0.1 * coalesce(rush_yds, 0) +
        0.1 * coalesce(rec_yds, 0) + 6 * coalesce(exp_tds, 0),
      standard = base,
      half     = base + 0.5 * coalesce(receptions, 0),
      ppr      = base + 1.0 * coalesce(receptions, 0)
    ) |>
    filter(has_primary) |>
    select(-base, -has_primary)
}

norm_name <- function(x) {
  x |>
    str_to_lower() |>
    str_remove_all("[.'’]") |>
    str_remove("\\s+(jr|sr|ii|iii|iv|v)$") |>
    str_squish()
}

add_positions <- function(proj, season = 2026) {
  rosters <- load_rosters(season) |>
    filter(position %in% c("QB", "RB", "WR", "TE")) |>
    transmute(key = norm_name(full_name), team, position) |>
    distinct(key, .keep_all = TRUE)
  
  out <- proj |>
    mutate(key = norm_name(player)) |>
    left_join(rosters, by = "key") |>
    select(-key)
  
  unmatched <- out |> filter(is.na(position)) |> pull(player)
  if (length(unmatched) > 0) message("UNMATCHED NAMES: ", paste(unmatched, collapse = ", "))
  out
}

# ---- consensus ----
CONS_SOURCES <- c("ESPN", "CBS")

name_fixes <- c("Zonovan Knight" = "Bam Knight")   # site name = our name

pull_consensus <- function() {
  clear_ffanalytics_cache()
  scrape_data(src = CONS_SOURCES, pos = c("QB", "RB", "WR", "TE"),
              season = 2026, week = NULL)
}

score_cons <- function(df) {
  df |>
    mutate(
      player = recode(player, !!!name_fixes),
      td     = coalesce(rush_tds, 0) + coalesce(rec_tds, 0),
      base   = 0.04 * coalesce(pass_yds, 0) + 4 * coalesce(pass_tds, 0) -
        2 * coalesce(pass_int, 0) + 0.1 * coalesce(rush_yds, 0) +
        0.1 * coalesce(rec_yds, 0) + 6 * td,
      standard = base,
      half     = base + 0.5 * coalesce(rec, 0),
      ppr      = base + 1.0 * coalesce(rec, 0)
    ) |>
    select(player, source = data_src, standard, half, ppr)
}

optimize_lineup <- function(roster, slots = c(QB = 1, RB = 2, WR = 2, TE = 1),
                            n_flex = 1, flex_pos = c("RB", "WR", "TE")) {
  roster <- roster |> arrange(desc(pts))
  
  starters <- imap_dfr(slots, function(n, pos) {
    roster |> filter(position == pos) |> slice_head(n = n) |> mutate(slot = pos)
  })
  
  flex <- roster |>
    anti_join(starters, by = "player") |>
    filter(position %in% flex_pos) |>
    slice_head(n = n_flex) |>
    mutate(slot = "FLEX")
  
  bind_rows(starters, flex)
}