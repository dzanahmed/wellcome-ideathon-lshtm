# Init

library(tidyverse)

# Load from raw and interim
vax_tweets <- read_csv('data/raw/vax_tweets.csv')
vax_tweets_v0 <- read_rds('data/interim/vax_tweets_v0.RDS')

#Provide filtering criteria
keywords <- c(
    # From doi:10.1016/j.jiph.2021.08.010.
    "COVID vaccine",
    "vaccine",
    "vaccination",
    "immune",
    "immunity",
    "COVID vaccination",
    "corona vaccine",
    "COVID19 vaccination",
    "COVID-19 vaccination",
    "coronavirus vaccination",
    "coronavirus vaccine",
    "COVID-19 vaccine",
    "Moderna",
    "Pfizer",
    "J&J",
    "Johnson & Johnson",
    "COVID vax",
    "corona vax",
    "covid-19 vax",
    "covid19 vax",
    "coronavirus vax",
    # Additional based on unfiltered_tweets review
    "vaccinated",
    "covid",
    "vaccinating"
)

# Apply the criteria
filtered_tweets <- vax_tweets_v0[grepl(paste(keywords, collapse = "|"),
                        vax_tweets_v0$text,
                        ignore.case = T), ]

# Check what didn't pass the filter
unfiltered_tweets <- vax_tweets_v0 |> anti_join(filtered_tweets, by = "tweet_id")

# Output the filtered dataset
filtered_tweets |> write_rds('data/interim/vax_tweets_v0_filtered.RDS')