#### March Madness Simulations (2026)
#
# Features:
#   1. Win probability uses pnorm((r1 - r2) / sigma) instead of an unscaled logistic.
#      This correctly calibrates probabilities — e.g., a 10-pt SRS advantage → ~84%
#      win probability rather than ~99.99% in the old model.
#   2. Removed the redundant uniform-noise perturbation on ratings; variance is
#      now captured once and properly via sigma.
#   3. Supports 68-team bracket with First Four play-in games (flag play-in teams
#      via a play_in = TRUE column in the ratings CSV).
#
# To update for 2026:
#   - Create ratings_2026.csv with columns: team_name, region, seed, rating, play_in
#   - Set play_in = TRUE for the 8 First Four participants (4 games × 2 teams each)
#   - Each First Four pair must share the same region AND seed in the CSV

library(tidyverse)
library(gt)
library(gtExtras)


# ----- Data -------------------------------------------------------------------------

# Read ratings file. Update URL/path for 2026 when bracket is announced.
ratings <- read_csv("https://raw.githubusercontent.com/steodose/March-Madness/main/ratings_2026.csv")

# Ensure play_in column exists. If absent (e.g. using 2025 data), assume all 64
# teams are main-draw participants with no First Four games.
if (!"play_in" %in% colnames(ratings)) {
    ratings$play_in <- FALSE
}


# ----- Model parameters -------------------------------------------------------------

# Standard deviation of college basketball game outcomes on a neutral court (~points).
# Calibrated so that around a 10-pt SRS advantage → ~84% win probability (pnorm(1)).
# Increase sigma to allow more upsets; decrease to tighten probabilities.
sigma <- 10.1 #same as Nate Silver

# Number of tournament simulations to run.
num_simulations <- 2000


# ----- Core functions ---------------------------------------------------------------

# Win probability for team1 given their SRS ratings.
# Model: actual margin ~ N(r1 - r2, sigma^2), so P(team1 wins) = Φ((r1 - r2) / sigma).
calculate_probability <- function(rating_team1, rating_team2, sigma) {
    pnorm((rating_team1 - rating_team2) / sigma)
}

# Simulate a single game; return the winning team's row.
simulate_game <- function(team1, team2, sigma) {
    prob_team1_wins <- calculate_probability(team1$rating, team2$rating, sigma)
    if (runif(1) < prob_team1_wins) team1 else team2
}

# Arrange teams for bracket seeding within each round.
prepare_teams_for_round <- function(teams) {
    teams <- as_tibble(teams)
    if (!"position" %in% colnames(teams)) {
        teams %>% arrange(region, seed) %>% mutate(position = row_number())
    } else {
        teams %>% arrange(region, position) %>% mutate(position = row_number())
    }
}

# Simulate First Four play-in games.
# Each pair of play-in teams must share the same region AND seed in the ratings CSV.
# Returns the 64-team bracket (main-draw teams + First Four winners).
simulate_first_four <- function(ratings, sigma) {
    ff_teams  <- ratings %>% filter(play_in == TRUE)
    main_teams <- ratings %>% filter(play_in == FALSE)

    if (nrow(ff_teams) == 0) return(ratings)  # No play-in games; pass through

    winners <- ff_teams %>%
        group_by(region, seed) %>%
        group_split() %>%
        lapply(function(pair) {
            if (nrow(pair) != 2) {
                stop(paste0("First Four error: ", pair$region[1], " seed-", pair$seed[1],
                            " has ", nrow(pair), " teams (expected 2)."))
            }
            simulate_game(pair[1, ], pair[2, ], sigma)
        })

    bind_rows(main_teams, bind_rows(winners))
}

# Simulate the full 64-team bracket across 6 rounds.
# Returns a list of 6 elements — the winners of each round.
simulate_tournament <- function(ratings, sigma) {
    ratings <- prepare_teams_for_round(ratings)
    round_results <- list()

    for (round in 1:6) {
        if (round < 5) {
            teams_in_round <- split(ratings, f = ratings$region)
        } else {
            teams_in_round <- list(all = ratings)  # Combine all regions for Final Four+
        }

        winners <- lapply(teams_in_round, function(region_teams) {
            new_winners <- tibble()

            if (round == 5) {  # Final Four: West vs East, Midwest vs South
                new_winners <- bind_rows(
                    simulate_game(
                        region_teams[region_teams$region == "West",  ],
                        region_teams[region_teams$region == "East",  ],
                        sigma
                    ),
                    simulate_game(
                        region_teams[region_teams$region == "Midwest", ],
                        region_teams[region_teams$region == "South",   ],
                        sigma
                    )
                )
            } else if (round == 6) {  # Championship
                new_winners <- simulate_game(region_teams[1, ], region_teams[2, ], sigma)
            } else {  # Rounds 1–4: standard bracket seeding (1 vs 16, 2 vs 15, ...)
                for (i in 1:(nrow(region_teams) / 2)) {
                    team1 <- region_teams[i, ]
                    team2 <- region_teams[nrow(region_teams) - i + 1, ]
                    new_winners <- bind_rows(new_winners, simulate_game(team1, team2, sigma))
                }
            }

            new_winners
        })

        ratings <- bind_rows(winners)
        if (round < 5) ratings <- prepare_teams_for_round(ratings)
        round_results[[round]] <- ratings
    }

    round_results
}


# ----- Simulation loop --------------------------------------------------------------

# Columns tracked: first_four (surviving play-in), then the 6 main-bracket rounds.
columns_to_add <- c("first_four", "second_round", "sweet_sixteen",
                    "elite_eight", "final_four", "championship_game", "champ")

for (col in columns_to_add) ratings[[col]] <- 0

set.seed(1234)

for (simulation in 1:num_simulations) {

    # Step 1: First Four — produces 64-team bracket.
    bracket_64 <- simulate_first_four(ratings, sigma)
    ratings[["first_four"]] <- ratings[["first_four"]] +
        ifelse(ratings$team_name %in% bracket_64$team_name, 1, 0)

    # Step 2: Main tournament (Rounds of 64 → Championship).
    round_results <- simulate_tournament(bracket_64, sigma)

    # Update advancement counts for rounds 1–6.
    for (round in 1:6) {
        current_stage <- columns_to_add[round + 1]  # skip first_four (index 1)
        winners <- round_results[[round]]
        if (!is.null(winners)) {
            ratings[[current_stage]] <- ratings[[current_stage]] +
                ifelse(ratings$team_name %in% winners$team_name, 1, 0)
        }
    }
}

# Convert counts to probabilities and round.
ratings[, columns_to_add] <- ratings[, columns_to_add] / num_simulations
ratings[columns_to_add] <- round(ratings[columns_to_add], 3)

sims <- ratings


# ----- Results table ----------------------------------------------------------------

team_mapping <- read_csv("https://raw.githubusercontent.com/steodose/March-Madness/main/ncaa_team_mapping.csv")

sims_gt <- sims %>%
    left_join(team_mapping, by = c("team_name" = "btp_team"))

sims_gt %>%
    select(espn_logo_url, team_name, seed, region, rating,
           first_four, second_round, sweet_sixteen,
           elite_eight, final_four, championship_game, champ) %>%
    arrange(-champ, -rating) %>%
    mutate(rank = row_number()) %>%
    relocate(rank) %>%
    gt() %>%
    cols_label(
        rank              = "Rank",
        espn_logo_url          = "",
        team_name         = "Team",
        seed              = "Seed",
        region            = "Region",
        rating            = "Rating",
        first_four        = "First Four",
        second_round      = "Second Round",
        sweet_sixteen     = "Sweet 16",
        elite_eight       = "Elite 8",
        final_four        = "Final Four",
        championship_game = "Reach Final",
        champ             = "Champ"
    ) %>%
    gt_img_rows(espn_logo_url, img_source = "web", height = 30) %>%
    data_color(
        columns = vars(first_four:champ),
        colors  = scales::col_numeric(
            palette = ggsci::rgb_material("amber", n = 100),
            domain  = c(0, 1.1)
        )
    ) %>%
    fmt_number(columns = rating, decimals = 1) %>% 
    fmt_percent(columns = vars(first_four:champ), decimals = 1) %>%
    tab_header(
        title    = md("**2026 March Madness Predictions**"),
        subtitle = "Chances to reach each round listed below. Based on 2,000 simulations of the NCAA Tournament."
    ) %>%
    tab_source_note(
        source_note = md("DATA: sports-reference.com<br>TABLE: @steodosescu")
    ) %>%
    opt_table_font(font = list(google_font("Outfit"), default_fonts())) %>%
    tab_options(
        column_labels.background.color    = "white",
        table.border.top.width            = px(3),
        table.border.top.color            = "transparent",
        table.border.bottom.color         = "transparent",
        table.border.bottom.width         = px(3),
        column_labels.border.top.width    = px(3),
        column_labels.border.top.color    = "transparent",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding                  = px(3),
        source_notes.font.size            = 12,
        table.font.size                   = 16,
        heading.title.font.size           = 28,
        heading.align                     = "center",
        heading.subtitle.font.size        = 14
    ) %>%
    gtsave("2026 March Madness Predictions.png")

write_csv(sims, "sims.csv")
