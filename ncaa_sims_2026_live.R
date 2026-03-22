#### March Madness Live Update Simulations (2026)
#
# Re-runs Monte Carlo forward from the current bracket state after factoring
# in actual results. Update actual_results.csv after each round, then source
# this script.
#
# actual_results.csv — round_eliminated values:
#   0  = Lost First Four
#   1  = Lost Round of 64
#   2  = Lost Round of 32
#   3  = Lost Sweet 16
#   4  = Lost Elite 8
#   5  = Lost Final Four
#   6  = Lost Championship (runner-up)
#   NA = Still alive (leave blank)

library(tidyverse)
library(gt)
library(gtExtras)


# ----- Parameters -----------------------------------------------------------

sigma           <- 10.1
num_simulations <- 2000

columns_to_add <- c("first_four", "second_round", "sweet_sixteen",
                    "elite_eight", "final_four", "championship_game", "champ")

# 0-indexed round → output column name
round_to_col <- setNames(columns_to_add, as.character(0:6))


# ----- Core functions (identical to ncaa_sims_2026.R) -----------------------

calculate_probability <- function(rating_team1, rating_team2, sigma) {
    pnorm((rating_team1 - rating_team2) / sigma)
}

simulate_game <- function(team1, team2, sigma) {
    p <- calculate_probability(team1$rating, team2$rating, sigma)
    if (runif(1) < p) team1 else team2
}

prepare_teams_for_round <- function(teams) {
    teams <- as_tibble(teams)
    if (!"position" %in% colnames(teams)) {
        teams %>% arrange(region, seed) %>% mutate(position = row_number())
    } else {
        teams %>% arrange(region, position) %>% mutate(position = row_number())
    }
}

# Compute correct bracket slot for a given seed when starting mid-tournament.
# For round 2: pmin(seed, 17-seed) maps seeds to their round-1 matchup slot (1-8).
# For round 3: applies the fold again to get Sweet 16 quadrant (1-4), etc.
# This preserves the correct bracket pairings even after upsets.
bracket_position <- function(seed, start_round) {
    pos <- as.integer(seed)
    for (i in seq_len(start_round - 1L)) {
        n   <- 16L %/% 2L^(i - 1L)
        pos <- pmin(pos, n + 1L - pos)
    }
    pos
}

simulate_first_four <- function(ratings, sigma) {
    ff_teams   <- ratings %>% filter(play_in == TRUE)
    main_teams <- ratings %>% filter(play_in == FALSE)
    if (nrow(ff_teams) == 0) return(ratings)
    winners <- ff_teams %>%
        group_by(region, seed) %>%
        group_split() %>%
        lapply(function(pair) {
            if (nrow(pair) != 2)
                stop(paste0("First Four error: ", pair$region[1], " seed-",
                            pair$seed[1], " has ", nrow(pair), " teams (expected 2)."))
            simulate_game(pair[1, ], pair[2, ], sigma)
        })
    bind_rows(main_teams, bind_rows(winners))
}

# Simulate rounds start_round:6 given a set of surviving teams.
# Returns a list of length 6; entries before start_round are NULL.
simulate_from_round <- function(survivors, start_round, sigma) {
    teams         <- prepare_teams_for_round(survivors)
    round_results <- vector("list", 6)

    for (round in start_round:6) {
        if (round < 5) {
            splits <- split(teams, f = teams$region)
        } else {
            splits <- list(all = teams)
        }

        winners <- lapply(splits, function(rt) {
            if (round == 5) {   # Final Four: West vs East, Midwest vs South
                bind_rows(
                    simulate_game(rt[rt$region == "West",    ], rt[rt$region == "East",  ], sigma),
                    simulate_game(rt[rt$region == "Midwest", ], rt[rt$region == "South", ], sigma)
                )
            } else if (round == 6) {   # Championship
                simulate_game(rt[1, ], rt[2, ], sigma)
            } else {   # Rounds 1–4: seed-based bracket pairing
                new_w <- tibble()
                for (i in 1:(nrow(rt) / 2))
                    new_w <- bind_rows(new_w,
                                       simulate_game(rt[i, ], rt[nrow(rt) - i + 1, ], sigma))
                new_w
            }
        })

        teams <- bind_rows(winners)
        if (round < 5) teams <- prepare_teams_for_round(teams)
        round_results[[round]] <- teams
    }

    round_results
}


# ----- Data -----------------------------------------------------------------

ratings_raw <- read_csv("https://raw.githubusercontent.com/steodose/March-Madness/main/ratings_2026.csv")
if (!"play_in" %in% colnames(ratings_raw)) ratings_raw$play_in <- FALSE

# Fill in actual_results.csv after each round (see schema at top of file)
actual_results <- read_csv("actual_results.csv")

# Merge results; 99 = still alive
ratings_live <- ratings_raw %>%
    left_join(select(actual_results, team_name, round_eliminated), by = "team_name") %>%
    mutate(round_eliminated = replace_na(as.integer(round_eliminated), 99L))

# Last fully completed round (-1 = no games played yet)
last_completed <- {
    done <- ratings_live$round_eliminated[ratings_live$round_eliminated < 99]
    if (length(done) == 0) -1L else max(done)
}

# Teams still alive; used for forward simulation
survivors <- ratings_live %>% filter(round_eliminated == 99)

# start_round: -1 = full tournament, 1–6 = simulate from this round onward
start_round <- if (last_completed < 0) -1L else last_completed + 1L

# Assign bracket-aware positions so simulate_from_round pairs teams correctly.
# Dense seed-rank (default) mismatches opponents when starting mid-tournament
# (e.g. pairs 3-seed vs 9-seed instead of 3-seed vs 11-seed in Round of 32).
if (start_round >= 2L) {
    survivors <- survivors %>%
        mutate(position = bracket_position(seed, start_round))
}

cat(sprintf(
    "Status: last completed round = %d | %d survivors | simulating from round %s\n",
    last_completed,
    nrow(survivors),
    if (start_round == -1) "First Four (full tournament)" else as.character(start_round)
))


# ----- Simulation loop ------------------------------------------------------

sims <- ratings_live
for (col in columns_to_add) sims[[col]] <- 0

set.seed(1234)

if (start_round == -1L) {
    # No results yet — identical to ncaa_sims_2026.R
    for (sim in 1:num_simulations) {
        bracket_64 <- simulate_first_four(ratings_raw, sigma)
        sims[["first_four"]] <- sims[["first_four"]] +
            ifelse(ratings_live$team_name %in% bracket_64$team_name, 1, 0)

        rr <- simulate_from_round(bracket_64, 1, sigma)
        for (round in 1:6) {
            col <- columns_to_add[round + 1]
            w   <- rr[[round]]
            if (!is.null(w))
                sims[[col]] <- sims[[col]] +
                    ifelse(ratings_live$team_name %in% w$team_name, 1, 0)
        }
    }
} else if (start_round <= 6 && nrow(survivors) > 0) {
    # Games have been played — simulate forward from current survivors only
    for (sim in 1:num_simulations) {
        rr <- simulate_from_round(survivors, start_round, sigma)
        for (round in start_round:6) {
            col <- columns_to_add[round + 1]
            w   <- rr[[round]]
            if (!is.null(w))
                sims[[col]] <- sims[[col]] +
                    ifelse(ratings_live$team_name %in% w$team_name, 1, 0)
        }
    }
}

# Convert counts to probabilities
sims[, columns_to_add] <- sims[, columns_to_add] / num_simulations


# ----- Lock in known results for completed rounds ---------------------------
# Replace simulation values with exact 1 / 0 based on actual_results:
#   • Still alive (99): 1 for all rounds up through last_completed
#   • Eliminated in round r: 1 for rounds before r, 0 for round r onward

if (last_completed >= 0) {
    sims <- sims %>%
        rowwise() %>%
        mutate(across(
            all_of(columns_to_add),
            ~ {
                col_round <- which(columns_to_add == cur_column()) - 1L  # 0-indexed

                if (round_eliminated == 99L) {
                    # Still alive: lock completed rounds to 1; keep sim value for future
                    if (col_round <= last_completed) 1 else .x
                } else {
                    # Eliminated: 1 before loss round, 0 at and after
                    if (col_round < round_eliminated) 1L else 0L
                }
            }
        )) %>%
        ungroup() %>%
        # Non-play-in teams automatically advance through the First Four
        mutate(first_four = if_else(!play_in, 1, first_four))
}

sims[columns_to_add] <- round(sims[columns_to_add], 3)


# ----- Results table --------------------------------------------------------

team_mapping <- read_csv("https://raw.githubusercontent.com/steodose/March-Madness/main/ncaa_team_mapping.csv")

sims_gt <- sims %>%
    left_join(team_mapping, by = c("team_name" = "btp_team"))

stage_label <- switch(
    as.character(last_completed),
    "-1" = "Pre-Tournament",
    "0"  = "After First Four",
    "1"  = "After Round of 64",
    "2"  = "After Round of 32",
    "3"  = "After Sweet 16",
    "4"  = "After Elite 8",
    "5"  = "After Final Four",
    "6"  = "Final Results"
)

sims_gt %>%
    select(espn_logo_url, team_name, seed, region, rating,
           first_four, second_round, sweet_sixteen,
           elite_eight, final_four, championship_game, champ) %>%
    filter(second_round == 1) %>% # update here
    arrange(-champ, -rating) %>%
    mutate(rank = row_number()) %>%
    relocate(rank) %>%
    gt() %>%
    cols_label(
        rank              = "Rank",
        espn_logo_url     = "",
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
        subtitle = paste0(stage_label, ". Remaining odds based on ",
                          num_simulations, " simulations.")
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
    gtsave(paste0("2026 March Madness Predictions - ", stage_label, ".png"))

write_csv(sims %>% select(-round_eliminated), "sims_live.csv")
