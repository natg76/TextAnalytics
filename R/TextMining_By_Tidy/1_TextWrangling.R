##-------------------------------------------------------------###
##
## Course: Text Analytics in R 
##
## Chapter 1 - Text Wrangling - Tidyverse
## Natarajan G
## 26/Dec/2019
##
##-------------------------------------------------------------###

# 0 Load required libraries

# Load the tidyverse packages
library(tidyverse)

# 1 Load data to analyze

# load("ch_1_twitter_data.rds")
review_data <- read.csv("Roomba Reviews.csv")
twitter_data <- readRDS("D:/NG_Learning/Datacamp/R Series/Marketing Analytics in R/Text Analytics in R/ch_1_twitter_data.rds")

# Print twitter_data
print(twitter_data)

# Print just the complaints in twitter_data
twitter_data %>% 
  filter(complaint_label == 'Complaint')

# So there are more non-complaints than complaints in twitter_data. 
# You might be starting to question whether or not this data is actually from Twitter! 
# There are a few other columns of interest in twitter_data that would be helpful to explore before you get to the tweets themselves. 
# Every tweet includes the number of followers that user has in the usr_followers_count column. 
# Do you expect those who complain to have more users or fewer users, on average, than those who don't complain? 
# You can use grouped summaries to quickly and easily provide an answer."

# Start with the data frame
twitter_data %>% 
  # Group the data by whether or not the tweet is a complaint
  group_by(complaint_label) %>% 
  # Compute the mean, min, and max follower counts
  summarize(
    avg_followers = mean(usr_followers_count),
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )


# The tweets that are complaints come from accounts with fewer followers, on average. 
# Is that what you expected?

# Counting on categorical data 

twitter_data %>% 
  # Filter for just the complaints
  filter(complaint_label == "Complaint") %>% 
  # Count the number of verified and non-verified users
  count(usr_verified)

# So verified Twitter users complain less often than non-verified Twitter users? Or are there just fewer verified users?

twitter_data %>% 
  # Group by whether or not a user is verified
  group_by(usr_verified) %>% 
  summarize(
    # Compute the average number of followers
    avg_followers = mean(usr_followers_count),
    # Count the number of users in each category
    n = n()
  )


# Okay, so there are fewer verified users. 
# We can also see that they, on average, have far more followers than non-verified users.

library(tidytext)

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) 

tidy_twitter %>% 
  # Compute word counts
  count(word) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))

# It's clear we haven't removed any stop words. Let's try this again!

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) %>% 
  # Remove stop words
  anti_join(stop_words)

tidy_twitter %>% 
  # Filter to keep complaints only
  filter(complaint_label == "Complaint") %>% 
  # Compute word counts and arrange in descending order
  count(word) %>% 
  arrange(desc(n))

# It looks like complaints include frequent references to time, delays, and service. 
# However, there are simply a lot of specific airlines referenced. 
# These could be considered as stop words specific to this data, and we'll see how to remove them in the next chapter.

