library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(purrr)


# load team mappings file
team_mapping <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/ncaa_team_mapping.csv')

#load sims ratings
teams_2026 <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/teams_2026.csv')


# load ratings
srs_ratings <- read_csv('/Users/Stephan/Desktop/R Projects/College Basketball/srs_ratings.csv')
silver_ratings <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/silver_ratings_2026.csv')
kenpom_ratings <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/kenpom_ratings_2026.csv')
benz_ratings <- read_csv('/Users/Stephan/Desktop/R Projects/March-Madness/benz_ratings.csv')

# scrape Team Rankings
url <- "https://www.teamrankings.com/ncaa-basketball/ranking/predictive-by-other/"

page <- read_html(url)

# Pull all HTML tables
tbls <- page %>%
    html_elements("table") %>%
    html_table(fill = TRUE)

# Clean names for inspection
tbls_clean <- map(tbls, clean_names)

tr_ratings <- tbls_clean %>%
    keep(~ any(str_detect(names(.x), "team|school")) &&
             any(str_detect(names(.x), "rank|rk"))) %>%
    .[[1]]

tr_ratings


## ------------- Clean SRS Ratings --------------- ##
srs_ratings_clean <- srs_ratings %>% 
    select(school, srs) %>% 
    rename("team" = "school") %>% 
    mutate(
        team = case_when(
            team == "Connecticut" ~ "UConn",
            team == "St. John's (NY)" ~ "St. John's",
            team == "Michigan St." ~ "Michigan State",
            team == "Iowa St." ~ "Iowa State",
            team == "California Baptist" ~ "Cal Baptist",
            team == "Prairie View" ~ "Prairie View A&M",
            team == "McNeese State" ~ "McNeese",
            team == "Virginia Commonwealth" ~ "VCU",
            team == "Pennsylvania" ~ "Penn",
            team == "Saint Mary's (CA)" ~ "Saint Mary's",
            team == "Long Island University" ~ "Long Island",
            team == "Brigham Young" ~ "BYU",
            team == "Maryland-Baltimore County" ~ "UMBC",
            team == "Southern Methodist" ~ "SMU",
            team == "Texas Christian" ~ "TCU",
            TRUE ~ team
        )
    )



## ------------- Clean Silver Ratings --------------- ##
silver_ratings_clean <- silver_ratings %>%
    select(sb_name, b_netrating_n) %>% 
    rename("team" = "sb_name",
           "silver_rating" = "b_netrating_n") %>% 
    mutate(
        team = case_when(
            team == "Ohio St." ~ "Ohio State",
            team == "California Baptist" ~ "Cal Baptist",
            team == "St. John's (NY)" ~ "St. John's",
            team == "Michigan St." ~ "Michigan State",
            team == "Iowa St." ~ "Iowa State",
            team == "North Dakota St." ~ "North Dakota State",
            team == "Utah St." ~ "Utah State",
            team == "Kennesaw St." ~ "Kennesaw State",
            team == "Wright St." ~ "Wright State",
            team == "Tennessee St." ~ "Tennessee State",
            team == "California Baptist" ~ "Cal Baptist",
            team == "Prairie View" ~ "Prairie View A&M",
            team == "McNeese State" ~ "McNeese",
            team == "Virginia Commonwealth" ~ "VCU",
            team == "Pennsylvania" ~ "Penn",
            team == "Saint Mary's (CA)" ~ "Saint Mary's",
            team == "LIU" ~ "Long Island",
            team == "Brigham Young" ~ "BYU",
            team == "Maryland-Baltimore County" ~ "UMBC",
            team == "Southern Methodist" ~ "SMU",
            team == "U Miami (FL)" ~ "Miami (FL)",
            team == "Miami University (OH)" ~ "Miami (OH)",
            team == "Queens" ~ "Queens (NC)",
            TRUE ~ team
        )
    )



## ------------- Clean KenPom Ratings -------------- ##
kenpom_ratings_clean <- kenpom_ratings %>% 
    clean_names() %>% 
    select(team, net_rtg) %>% 
    rename("kenpom_rating" = "net_rtg")


## ------------- Clean Luke Benz Ratings -------------- ## 
benz_ratings_clean <- benz_ratings %>% 
    clean_names() %>% 
    select(team, conference, net_rating) %>% 
    rename("benz_rating" = "net_rating")


## ----- Join all ratings with team mappings ------ ##
all_ratings <- teams_2026 %>% 
    left_join(srs_ratings_clean, by = c("team_name" = "team")) %>% 
    select(-rating)
   
# join with Nate Silver's ratings
all_ratings <- all_ratings %>% 
    left_join(silver_ratings_clean, by = c("team_name" = "team")) 

# identify mean and sd of both datasets to normalize into composite ratings
srs_mean <- mean(all_ratings$srs, na.rm = TRUE)
srs_sd   <- sd(all_ratings$srs, na.rm = TRUE)

silver_mean <- mean(all_ratings$silver_rating, na.rm = TRUE)
silver_sd   <- sd(all_ratings$silver_rating, na.rm = TRUE)

# Shift Silver to be centered on the SRS mean but keep Silver's natural spread.
# Previously Silver was rescaled to SRS's SD, which forced both inputs to the same
# spread and made averaging them produce almost the same spread as SRS alone
# (two correlated distributions of equal SD average to ~96% of that SD — barely
# any compression). By only centering (not inflating Silver's SD), Silver's
# narrower spread will pull top teams back toward the mean in the composite.
all_ratings <- all_ratings %>%
    mutate(
        silver_scaled = silver_rating - silver_mean + srs_mean
    )

# final ratings df
all_ratings <- all_ratings %>%
    mutate(rating = (srs + silver_scaled) / 2
           ) %>%
    select(team_name, region, seed, rating, play_in)

#write to wd for model sims
write_csv(all_ratings, "ratings_2026.csv")




# maybe do these transformations later
all_ratings <- all_ratings %>% 
    left_join(silver_ratings_clean, by = c("team_name" = "team"))

all_ratings <- all_ratings %>% 
    left_join(kenpom_ratings_clean, by = c("team_name" = "team"))

all_ratings <- all_ratings %>% 
    left_join(benz_ratings_clean, by = c("team_name" = "team"))




