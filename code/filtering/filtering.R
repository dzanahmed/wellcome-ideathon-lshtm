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
                        ignore.case = T), ] |> mutate(filtered = T) |> relocate(filtered, .before=text)

# Check what didn't pass the filter
unfiltered_tweets <- vax_tweets_v0 |> anti_join(filtered_tweets, by = "tweet_id") |> mutate(filtered = F) |> relocate(filtered, .before=text)

# Output the filtered dataset
filtered_tweets |> write_rds('data/interim/vax_tweets_v0_filtered.RDS')

# Rbind them
filtered_and_unfiltered <- filtered_tweets |> rbind(unfiltered_tweets) |> arrange(tweet_id)

# Lets assess and classify
set.seed(666)
sample <- filtered_and_unfiltered |> sample_n(size=200) |> select(tweet_id, user_description, filtered, text, hashtags)

sample$filtered |> summary()
# 179 passed the filter, 21 did not

write_csv(sample, file='code/filtering/sample.csv')


# And output to one
filtered_and_unfiltered |> write_rds('data/interim/vax_tweets_v0_filtered_unfiltered.RDS')

filtered_and_unfiltered |> write_csv('data/interim/vax_tweets_v0_filtered_unfiltered.csv')
