# -------------------------------------------------------------------
# Title: Data Collection of Political Twitter Content
# Author: Nikolina Klatt
# Date: April 27, 2024
# Description:
#   This script is designed to analyze the Twitter activity of US politicians, focusing on both the 
#   Senate and House of Representatives. It involves scraping Twitter data for these politicians,
#   processing the data to clean and structure it for analysis, and examining specific topics such
#   as mentions of "abortion". 
#
# Workflow:
#   1. Load necessary R packages and set up environment variables.
#   2. Import political data from an Excel file containing Twitter handles and other relevant details.
#   3. Combine Senate and House data into a unified dataframe and clean Twitter links to extract usernames.
#   4. Retrieve tweets from the collected usernames using the academictwitteR package and store them locally.
#   5. Clean and filter the retrieved tweets to exclude replies and retweets, and calculate engagement metrics.
#   6. Perform specific analysis to filter tweets mentioning "abortion".
#   7. Merge tweet data with politician information to facilitate comprehensive analysis.
#   8. Save the final cleaned and merged data for further analysis or reporting.
# -------------------------------------------------------------------

library(tidyverse)
library(readr)
library(XML)
library(tibble)
library(stringr)
library(rvest)
library(kableExtra)
library(readxl)
library(usethis)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(xml2)
library(janitor)
library(tidytext)  
library(lsa)  
library(SnowballC)  
library(wordcloud) 
library(skimr)  
library(pacman)
p_load('httr', 'academictwitteR', 'tidyverse', 'lubridate', 'rtweet', 'naniar')
paste0("bearer ", Sys.getenv("TWITTER_BEARER"))
bearer_token <- Sys.getenv("TWITTER_BEARER")

# Scraping Preparation 
# Read in the Senate data from the congress_twitter_120622.xlsx file
congress_senate <- read_excel("congress_twitter_120622.xlsx", sheet = "Senate")

# Add a "congress" column to the Senate data and set its value to "senate"
congress_senate$congress <- "senate"

# Read in the House data from the congress_twitter_120622.xlsx file
congress_house <- read_excel("congress_twitter_120622.xlsx", sheet = "House")

# Add a "congress" column to the House data and set its value to "house"
congress_house$congress <- "house"

# Combine the Senate and House data into one data frame called "politician"
politician <- rbind(congress_senate,congress_house)

# Extract the Twitter username from the "Link" column and store it in a new "user" column
politician$user <- str_remove(politician$Link, pattern = "https://twitter.com/")

# Create a list of the Twitter usernames in the "politician" data frame
list <- politician$user

# # Create a test list of the first six Twitter usernames in the "politician" data frame
# list_test <- head(politician$user)

# Create function to retrieve data
# Function to retrieve tweets via API 
retrieve_tweets <- function(user) { # user comes from previously created list 
  # Another option is to add "query" parameter
  df <- academictwitteR::get_all_tweets(
    users = user,
    start_tweets = "2022-01-01T00:00:00Z",
    end_tweets = "2022-12-31T00:00:00Z",
    bearer_token = get_bearer(),
    data_path = "data/raw",
    n = Inf,
    context_annotations = TRUE) %>% 
    mutate(username=user) %>% # create new column with the username from the list
    return(df)
}

# Create function to filter raw df
clean_df <- function(df) {
  df <- df %>% 
    select(author_id, username, created_at, text, conversation_id, public_metrics) %>% # relevant variables 
    filter(!str_detect(text, "^@")) %>% # filter replies to other tweets 
    filter(!str_detect(text, "^RT")) %>%  # filter retweets 
    unnest(public_metrics) %>% # unnest info from public_metrics including counts
    replace_with_na(replace = list(reply_count = 0, 
                                   retweet_count = 0, 
                                   like_count = 0,
                                   quote_count = 0)) %>% 
    separate(created_at, c("date","time"), sep = "T") %>%  # split the created_at variable in date and time
    relocate(username, author_id, date, time, text, conversation_id, retweet_count, reply_count,
             like_count, quote_count) %>% 
    mutate(ratio_reply_like = reply_count/like_count) %>% # create column with ratio on reply/like
    group_by(username) %>% # group by username for arranging
    arrange(desc(ratio_reply_like), .by_group = TRUE) # arrange according to ratio
  return(df)
}

# Retrieve tweets via API from handles included in list 
tweets <- 
  map_dfr(list, function(x) { # list is the previously created list 
    retrieve_tweets(x) %>% # previously created function to retrieve tweets
      return(df)
  })

# clean raw df into clean with function 
df_clean <- clean_df(tweets)
## save clean df locally 
# write.csv(df_clean, "tweets_df_clean.csv")

# Filter for tweets that mention abortion 
abortion_df <- df_clean %>%
  filter(str_detect(text, "(\\b\\w*abortion\\w*\\b)"))

# Merge and save dataset 
# merge abortion tweets df with politician info df
politician <- rename(politician, username = user)

abortion_pol_df <- abortion_df %>% 
  left_join(politician, by = )

# write_csv(abortion_pol_df, "abortion_pol_df.csv")



