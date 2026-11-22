library(shiny)
library(tidyverse)
library(reactable)
source("functions.R")

# read the newest projections snapshot
latest <- list.files("data/proj", pattern = "^projections_.*\\.csv$", full.names = TRUE) |>
  sort() |> tail(1)
proj <- read_csv(latest, show_col_types = FALSE)

teams <- read_csv("data/teams.csv", show_col_types = FALSE)
proj  <- proj |> left_join(teams, by = "team")

ui <- fluidPage(
  titlePanel("Sportsbook-Based Fantasy Projections"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("scoring", "Scoring",
                   choices = c("Standard" = "standard", "Half PPR" = "half", "PPR" = "ppr"),
                   selected = "ppr"),
      selectInput("pos", "Position",
                  choices = c("QB", "RB", "WR", "TE", "FLEX (RB/WR/TE)" = "FLEX")),
      textOutput("last_updated")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Rankings", reactableOutput("table")),
        tabPanel("My Lineup",
                 selectizeInput("my_players", "Your Roster", choices = sort(proj$player),
                                multiple = TRUE, width = "100%",
                                options = list(placeholder = "Type a name to add a player")),
                 fluidRow(
                   column(2, numericInput("n_qb", "QB", 1, min = 0, max = 3)),
                   column(2, numericInput("n_rb", "RB", 2, min = 0, max = 5)),
                   column(2, numericInput("n_wr", "WR", 2, min = 0, max = 5)),
                   column(2, numericInput("n_te", "TE", 1, min = 0, max = 3)),
                   column(2, numericInput("n_flex", "FLEX", 1, min = 0, max = 4))
                 ),
                 textOutput("lineup_total"),
                 h4("Starters"), reactableOutput("lineup"),
                 h4("Bench"),    reactableOutput("bench")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$last_updated <- renderText({
    paste("Last updated:",
          format(max(proj$pulled_at), "%a %b %d, %I:%M %p ET", tz = "America/New_York"))
  })
  
  output$table <- renderReactable({
    pool <- if (input$pos == "FLEX") {
      proj |> filter(position %in% c("RB", "WR", "TE"))
    } else {
      proj |> filter(position == input$pos)
    }
    
    d <- pool |>
      mutate(pts = .data[[input$scoring]]) |>
      arrange(desc(pts)) |>
      mutate(Rank = row_number()) |>
      transmute(
        Rank, Player = player, Pos = position,
        Team = team_logo, Abbr = team,
        `Proj pts` = round(pts, 1),
        `Pass yds` = round(pass_yds, 0), `Pass TD` = round(pass_tds, 2),
        `Rush yds` = round(rush_yds, 0),
        Rec = round(receptions, 1), `Rec yds` = round(rec_yds, 0),
        `Exp TD` = round(exp_tds, 2)
      )
    
    # QBs: drop receiving. Everyone else: drop passing.
    d <- if (input$pos == "QB") {
      d |> select(-Rec, -`Rec yds`)
    } else {
      d |> select(-`Pass yds`, -`Pass TD`)
    }
    
    reactable(
      d, defaultPageSize = 30, striped = TRUE,
      searchable = TRUE,
      defaultColDef = colDef(na = "-"),
      columns = list(
        Player = colDef(minWidth = 170),
        Team = colDef(
          width = 70, align = "center",
          cell = function(value, index) {
            if (is.na(value)) d$Abbr[index]
            else tags$img(src = value, height = 26,
                          alt = d$Abbr[index], title = d$Abbr[index])
          }
        ),
        Abbr = colDef(show = FALSE)
      )
    )
  })
  lineup_res <- reactive({
    req(input$my_players)
    roster <- proj |>
      filter(player %in% input$my_players) |>
      mutate(pts = .data[[input$scoring]])
    slots <- c(QB = input$n_qb, RB = input$n_rb, WR = input$n_wr, TE = input$n_te)
    starters <- optimize_lineup(roster, slots, input$n_flex)
    list(starters = starters,
         bench = roster |> anti_join(starters, by = "player") |> arrange(desc(pts)))
  })
  
  output$lineup_total <- renderText({
    paste("Projected total:", round(sum(lineup_res()$starters$pts), 1))
  })
  
  output$lineup <- renderReactable({
    lineup_res()$starters |>
      transmute(Slot = slot, Player = player, Pos = position, Team = team,
                `Proj pts` = round(pts, 1)) |>
      reactable(striped = TRUE)
  })
  
  output$bench <- renderReactable({
    lineup_res()$bench |>
      transmute(Player = player, Pos = position, Team = team, `Proj pts` = round(pts, 1)) |>
      reactable(striped = TRUE)
  })
}

shinyApp(ui, server)