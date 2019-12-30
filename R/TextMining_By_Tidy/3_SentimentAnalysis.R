##-------------------------------------------------------------###
##
## Course: Text Analytics in R 
##
## Chapter 3 - Sentiment Dictionaries
## Natarajan G
## 27/Dec/2019
##
##-------------------------------------------------------------###


# 1 Load data to analyze
# 1 Load data to analyze

library(tidyverse)
library(tidytext)

twitter_data <- twitter_data <- readRDS("D:/NG_Learning/Datacamp/R Series/Marketing Analytics in R/Text Analytics in R/ch_1_twitter_data.rds")

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) %>% 
  # Remove stop words
  anti_join(stop_words2)


# Tidytext contains multiple sentiment dictionaries. Will explore one of them here.
# Count the number of words associated with each sentiment in nrc

get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))


# Pull in the nrc dictionary, count the sentiments and reorder them by count
sentiment_counts <- get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  mutate(sentiment2 = fct_reorder(sentiment, n))

# Visualize sentiment_counts using the new sentiment factor column
ggplot(sentiment_counts, aes(x=sentiment2, y=n)) +
  geom_col() +
  coord_flip() +
  # Change the title to "Sentiment Counts in NRC", x-axis to "Sentiment", and y-axis to "Counts"
  labs(
    title = "Sentiment Counts in NRC",
    x = "Sentiment",
    y = "Counts"
  )


# The tidy_twitter dataset has been loaded for you. 
# Let's see what sort of sentiments are most prevalent in our Twitter data.

# Join tidy_twitter and the NRC sentiment dictionary
sentiment_twitter <- tidy_twitter %>% 
  inner_join(get_sentiments("nrc"))

# Count the sentiments in sentiment_twitter
sentiment_twitter %>% 
  count(sentiment) %>% 
  # Arrange the sentiment counts in descending order
  arrange(desc(n))

# Let's explore which words are associated with each sentiment in our Twitter data.

word_counts1 <- tidy_twitter %>% 
  # Append the NRC dictionary and filter for positive, fear, and trust
  inner_join(get_sentiments("nrc") %>% 
               filter(sentiment %in% c("positive", "fear", "trust"))) %>%
  # Count by word and sentiment and keep the top 10 of each
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  # Create a factor called word2 that has each word ordered by the count
  mutate(word2 = fct_reorder(word, n))


word_counts <- tidy_twitter %>% 
  # Append the NRC dictionary and filter for positive, fear, and trust
  inner_join(get_sentiments("nrc")) %>% 
  filter(sentiment %in% c("positive", "fear", "trust")) %>%
  # Count by word and sentiment and keep the top 10 of each
  count(word, sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10, n) %>% 
  ungroup() %>% 
  # Create a factor called word2 that has each word ordered by the count
  mutate(word2 = fct_reorder(word, n))

word_counts

# Create a bar plot out of the word counts colored by sentiment
ggplot(word_counts1, aes(x=word2, y=n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  # Create a separate facet for each sentiment with free axes
  facet_wrap(~sentiment, scales="free") +
  coord_flip() +
  # Title the plot "Sentiment Word Counts" with "Words" for the x-axis
  labs(
    title = "Sentiment Word Counts",
    x = "Words"
  )


# These word counts by sentiment illustrate a possible mismatch with this particular sentiment dictionary. 
# For example, gate is listed under trust. Pay is listed under both trust and positive. 
# Remember, our sentiment analysis is conditioned on the dictionary we use. 
# It's a tall order, but finding or building a sentiment dictionary that is context-specific would be ideal.


# Improving Sentiment Visualizations - overall sentiment

tidy_twitter %>% 
  # Append the NRC sentiment dictionary
  inner_join(get_sentiments("nrc")) %>% 
  # Count by complaint label and sentiment
  count(complaint_label, sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment, n)


tidy_twitter %>% 
  # Append the afinn sentiment dictionary
  inner_join(get_sentiments("afinn")) %>% 
  # Group by both complaint label and whether or not the user is verified
  group_by(complaint_label, usr_verified) %>% 
  # Summarize the data with an aggregate_value = sum(value)
  summarize(aggregate_value = sum(value)) %>% 
  # Spread the complaint_label and aggregate_value columns
  spread(complaint_label, aggregate_value) %>% 
  mutate(overall_sentiment = Complaint + `Non-Complaint`)


sentiment_twitter <- tidy_twitter %>% 
  # Append the bing sentiment dictionary
  inner_join(get_sentiments("bing")) %>% 
  # Count by complaint label and sentiment
  count(complaint_label, sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment, n) %>% 
  # Compute overall_sentiment = positive - negative
  mutate(overall_sentiment = positive - negative)


# Create a bar plot out of overall sentiment by complaint level, colored by a complaint label factor
ggplot(
  sentiment_twitter, 
  aes(x=complaint_label, y=overall_sentiment, fill=as.factor(complaint_label))
) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  # Title the plot "Overall Sentiment by Complaint Type," with an "Airline Twitter Data" subtitle
  labs(
    title = "Overall Sentiment by Complaint Type",
    subtitle = "Airline Twitter Data"
  )
