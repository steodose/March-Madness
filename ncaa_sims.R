#### March Madness Simulations

library(tidyverse)
library(gt)
library(gtExtras)


# read in Ratings File (using Sports Ref SRS)
ratings <- read_csv("/Users/Stephan/Desktop/R Projects/March-Madness/ratings.csv")


# Logistic regression on SRS ratings to calculate the probability of a team winning based on ratings
calculate_probability <- function(rating_team1, rating_team2) {
    1 / (1 + exp(-(rating_team1 - rating_team2)))
}

# Function to simulate a single game
simulate_game <- function(team1, team2) {
    prob_team1_wins <- calculate_probability(team1$rating, team2$rating)
    if (runif(1) < prob_team1_wins) {
        cat(paste(team1$team_name, "wins over", team2$team_name, "\n"))
        return(team1)
    } else {
        cat(paste(team2$team_name, "wins over", team1$team_name, "\n"))
        return(team2)
    }
}

# Function to prepare teams for a round based on seeding
prepare_teams_for_round <- function(teams) {
    teams <- as_tibble(teams) # Ensure it's a tibble for dplyr compatibility
    if (!"position" %in% colnames(teams)) {
        teams <- teams %>%
            arrange(region, seed) %>%
            mutate(position = row_number())
    } else {
        teams <- teams %>%
            arrange(region, position) %>%
            mutate(position = row_number())
    }
    return(teams)
}

# Function to simulate the tournament
simulate_tournament <- function(ratings) {
    ratings <- prepare_teams_for_round(ratings)
    round_results <- list()
    
    for (round in 1:6) {
        cat("Starting Round:", round, "\n")
        # Split teams by region only for the pairing logic
        teams_in_round <- split(ratings, f = ratings$region)
        
        winners <- lapply(teams_in_round, function(region_teams) {
            # If only one team is left, it's the region's winner
            if (nrow(region_teams) == 1) {
                return(region_teams) 
            }
            
            new_winners <- tibble()
            
            # Correct pairing logic for all rounds
            # After the first round, winners need to be re-seeded or sorted before pairing
            region_teams <- region_teams[order(region_teams$position), ]
            n <- nrow(region_teams)
            
            # For the first round, use the original seeding to create matchups
            # For subsequent rounds, pair teams based on their new "position" or progression
            if (round == 1) {
                matchups <- 1:(n/2)  # Original matchups based on seeding
            } else {
                # Re-calculate matchups based on remaining teams
                region_teams <- region_teams %>% arrange(desc(rating)) # Sort teams by their rating or another metric if needed
                matchups <- 1:(n/2)
            }
            
            for (i in matchups) {
                team1 <- region_teams[i, ]
                team2 <- region_teams[n - i + 1, ]
                winner <- simulate_game(team1, team2)
                new_winners <- bind_rows(new_winners, winner)
            }
            
            return(new_winners)
        })
        
        ratings <- bind_rows(winners)
        ratings <- prepare_teams_for_round(ratings)
        round_results[[round]] <- ratings
        cat("Winners of Round", round, ":\n")
        print(ratings$team_name)
    }
    
    return(round_results)
}

# Initialize the probabilities in the ratings dataframe
columns_to_add <- c("first_round", "second_round", "sweet_sixteen", "elite_eight", "final_four", "championship_game", "champ")
for (column in columns_to_add) {
    ratings[[column]] <- 0
}

num_simulations <- 10


# Run sims
set.seed(1234)
for (simulation in 1:num_simulations) {
    round_results <- simulate_tournament(ratings)
    for (round in 1:6) {
        winners <- round_results[[round]]
        ratings[,columns_to_add[round]] <- ratings[,columns_to_add[round]] + ifelse(ratings$team_name %in% winners$team_name, 1, 0)
    }
}


# The last round of winners is the champion
champions <- round_results[[length(round_results)]]
ratings$champ <- ratings$champ + ifelse(ratings$team_name %in% champions$team_name, 1, 0)


# Convert counts to probabilities
ratings[,5:11] <- ratings[,5:11] / num_simulations

# Adjust the number of decimals for all probability columns
probability_columns <- c("first_round", "second_round", "sweet_sixteen", "elite_eight", "final_four", "championship_game", "champ")
ratings[probability_columns] <- round(ratings[probability_columns], 3)  # Adjust to the desired number of decimals

# Rename the final dataframe
sims <- ratings



##### gt results table #####
team_mapping <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/ncaa_team_mapping.csv')

sims_gt <- sims %>%
    left_join(team_mapping, by = c("team_name" = "silver_team"))

sims_gt %>%
    select(logo_url, team_name:champ) %>%
    arrange(-champ, -rating) %>%
    mutate(rank = row_number()) %>%
    relocate(rank) %>%
    gt() %>%
    cols_label(rank = "Rank",
               logo_url = "",
               team_name = "Team",
               seed = "Seed",
               region = "Region",
               rating = "Rating",
               first_round = "First Round",
               second_round = "Second Round",
               sweet_sixteen = "Sweet 16",
               elite_eight = "Elite 8",
               final_four = "Final Four",
               championship_game = "Reach Final",
               champ = "Champ"
    ) %>% 
    gt_img_rows(logo_url, img_source = "web", height = 30) %>%
    data_color(columns = vars(first_round:champ),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = c(0,1)
               )) %>% 
    fmt_percent(
        columns = vars(first_round:champ),
        decimals = 1
    ) %>%
    tab_header(title = md("**2024 March Madness Predictions**"),
               subtitle ="Difference between experts' predictions and America's predictions. Only top 20 most picked teams shown.") %>%
    tab_source_note(
        source_note = md("DATA: sports-reference.com<br>TABLE: @steodosescu")) %>%
    opt_table_font(
        font = list(
            google_font("Outfit"),
            default_fonts()
        )
    ) %>%
    tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.title.font.size = 28,
        heading.align = "center",
        heading.subtitle.font.size = 14,
    ) %>%
    gtsave("2024 March Madness Predictions.png")

