#### March Madness Simulations

library(tidyverse)
library(gt)
library(gtExtras)


# read in Ratings File (using Sports Ref SRS)
ratings <- read_csv("https://raw.githubusercontent.com/steodose/March-Madness/main/ratings_2025.csv")

#calculate df of ratings
std_dev <- sd(ratings$rating)

# Decide on a fraction of the standard deviation to use as variance
variance_factor <- std_dev  # Adjust this fraction based on preference

# Old Logistic regression on SRS ratings to calculate the probability of a team winning based on ratings
# calculate_probability <- function(rating_team1, rating_team2) {
#     1 / (1 + exp(-(rating_team1 - rating_team2)))
# }

# New Logic
calculate_probability <- function(rating_team1, rating_team2, variance_factor) {
    
    adjusted_rating_team1 <- rating_team1 + runif(1, -variance_factor, variance_factor)
    adjusted_rating_team2 <- rating_team2 + runif(1, -variance_factor, variance_factor)
    
    # Calculate the probability using the adjusted ratings
    prob_team1_wins <- 1 / (1 + exp(-(adjusted_rating_team1 - adjusted_rating_team2)))
    
    return(prob_team1_wins)
}



# Function to simulate a single game
simulate_game <- function(team1, team2, variance_factor) {
    prob_team1_wins <- calculate_probability(team1$rating, team2$rating, variance_factor)
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


# function to simulate tournament
simulate_tournament <- function(ratings, variance_factor) {
    ratings <- prepare_teams_for_round(ratings)
    round_results <- list()
    
    for (round in 1:6) {
        cat("Starting Round:", round, "\n")
        
        if (round < 5) { # Rounds before the Final Four
            teams_in_round <- split(ratings, f = ratings$region)
        } else {
            teams_in_round <- list(all = ratings) # Combine all for Final Four and Championship
        }
        
        winners <- lapply(teams_in_round, function(region_teams) {
            new_winners <- tibble()
            
            if (round == 5) {  # Final Four
                # Manually set the matchups for the Final Four
                west_vs_east_winner <- simulate_game(region_teams[region_teams$region == "West",], region_teams[region_teams$region == "East",], variance_factor)
                midwest_vs_south_winner <- simulate_game(region_teams[region_teams$region == "Midwest",], region_teams[region_teams$region == "South",], variance_factor)
                new_winners <- bind_rows(new_winners, west_vs_east_winner, midwest_vs_south_winner)
            } else if (round == 6) { # Championship
                # Directly face off the two winners from the Final Four in the Champ game
                winner <- simulate_game(region_teams[1,], region_teams[2,], variance_factor)
                new_winners <- bind_rows(new_winners, winner)
            } else { # Rounds 1-4
                for (i in 1:(nrow(region_teams)/2)) {
                    team1 <- region_teams[i, ]
                    team2 <- region_teams[nrow(region_teams) - i + 1, ]
                    winner <- simulate_game(team1, team2, variance_factor)
                    new_winners <- bind_rows(new_winners, winner)
                }
            }
            
            return(new_winners)
        })
        
        ratings <- bind_rows(winners)
        if (round < 5) {
            ratings <- prepare_teams_for_round(ratings)
        }
        round_results[[round]] <- ratings  # Store the results of this round
        cat("Winners of Round", round, ":\n")
        print(ratings$team_name)
    }
    
    return(round_results)
}


# Initialize the probabilities in the ratings dataframe
columns_to_add <- c("second_round", "sweet_sixteen", "elite_eight", "final_four", "championship_game", "champ")

# Initialize the probabilities in the ratings dataframe
# Since we're skipping the 'first_round' (all teams are at 100%), start from 'second_round'
for (column in columns_to_add) {
    ratings[[column]] <- 0  # Initialize with 0; these will be updated during simulations
}

# number of sims to run
num_simulations <- 1000

# Run simulations
set.seed(1234)  # For reproducibility
for (simulation in 1:num_simulations) {
    round_results <- simulate_tournament(ratings, variance_factor)
    
    # Adjusted loop to correctly update probabilities starting from 'second_round'
    for (round in 1:6) {
        current_stage = columns_to_add[round] 
        winners <- round_results[[round]]
        # Update probabilities for reaching each stage of the tournament
        if (!is.null(winners)) {
            ratings[[current_stage]] <- ratings[[current_stage]] + ifelse(ratings$team_name %in% winners$team_name, 1, 0)
        }
    }
}

# Convert counts to probabilities (no need to adjust for 'first_round' as it's 100% for all)
ratings[,columns_to_add] <- ratings[,columns_to_add] / num_simulations

# Adjust the number of decimals for all probability columns
ratings[columns_to_add] <- round(ratings[columns_to_add], 3)
ratings$first_round <- 1 # first round probabilities are all 100%

# Final dataframe preparation
sims <- ratings



##### --------- Results table ---------- #####
team_mapping <- read_csv('https://raw.githubusercontent.com/steodose/March-Madness/main/ncaa_team_mapping.csv')

sims_gt <- sims %>%
    left_join(team_mapping, by = c("team_name" = "espn_team"))

sims_gt %>%
    select(-first_round) %>% 
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
               #first_round = "First Round",
               second_round = "Second Round",
               sweet_sixteen = "Sweet 16",
               elite_eight = "Elite 8",
               final_four = "Final Four",
               championship_game = "Reach Final",
               champ = "Champ"
    ) %>% 
    gt_img_rows(logo_url, img_source = "web", height = 30) %>%
    data_color(columns = vars(second_round:champ),
               colors = scales::col_numeric(palette = ggsci::rgb_material('amber', n = 100),
                                            domain = c(0,1.1)
               )) %>% 
    fmt_percent(
        columns = vars(second_round:champ),
        decimals = 1
    ) %>%
    tab_header(title = md("**2025 March Madness Predictions**"),
               subtitle ="Chances to reach each round listed below. Based on 10,000 simulations of the NCAA Tournament.") %>%
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
    gtsave("2025 March Madness Predictions.png")

#write sims output to working directory
write_csv(sims, "sims.csv")