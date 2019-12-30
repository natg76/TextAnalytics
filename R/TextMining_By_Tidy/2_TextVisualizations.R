##-------------------------------------------------------------###
##
## Course: Text Analytics in R 
##
## Chapter 2 - Visualizing Text
## Natarajan G
## 27/Dec/2019
##
##-------------------------------------------------------------###


# 1 Load data to analyze

library(tidyverse)
library(tidytext)

twitter_data <- twitter_data <- readRDS("D:/NG_Learning/Datacamp/R Series/Marketing Analytics in R/Text Analytics in R/ch_1_twitter_data.rds")

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) %>% 
  # Remove stop words
  anti_join(stop_words)

# 2 Visualize a word frequency plot. Filter only words occuring more than 100 times.

word_counts <- tidy_twitter %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word) %>% 
  # Keep words with count greater than 100
  filter(n > 100)

# Create a bar plot using word_counts with x = word
ggplot(word_counts, aes(x=word, y=n)) +
  geom_col() +
  # Flip the plot coordinates
  coord_flip()

# Like last time, its easy to see that the complaints include frequent references to time, delays, 
# and service, along with a number of specific airlines.

word_counts <- tidy_twitter %>% 
  # Only keep the non-complaints
  filter(complaint_label == 'Non-Complaint') %>% 
  count(word) %>% 
  filter(n > 150)

# Create a bar plot using the new word_counts
ggplot(word_counts, aes(x=word, y=n)) +
  geom_col() +
  coord_flip() +
  # Title the plot "Non-Complaint Word Counts"
  ggtitle("Non-Complaint Word Counts")

## The second plot still contains more stop words specific to the dataset.

# What is the structure of the default stop_words?

stop_words
str(stop_words)
stop_words %>% count(lexicon)

# It contains 3 types of stop words: Onix, SMART, snowball

# Now lets add new custom stopwords to the list
# custm stopwords is a triblle : i.e. Tidyverse dataframe

custom_stop_words <- tribble(
  # Column names should match stop_words
  ~word, ~lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "win", "CUSTOM",
  "t.co", "CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  rbind(custom_stop_words)

stop_words2 %>% count(lexicon)

#

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word, tweet_text) %>% 
  # Remove stop words
  anti_join(stop_words2)

#Now you can see 3 custom stopwords added to the list.

word_counts <- tidy_twitter %>% 
  filter(complaint_label == "Non-Complaint") %>% 
  count(word) %>% 
  # Keep terms that occur more than 100 times
  filter(n > 100) %>% 
  # Reorder word as an ordered factor by word counts
  mutate(word2 = fct_reorder(word, n))

# Plot the new word column with type factor
ggplot(word_counts, aes(x=word2, y=n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Non-Complaint Word Counts")

# Flight is still most commonly used. With entered and getaway, it appears that 
# there may have been some kind of sweepstake people were entering frequently in the non-complaints.

# visualize the differences in word counts based on complaints and non-complaints.

word_counts <- tidy_twitter %>%
  # Count words by whether or not its a complaint
  count(word, complaint_label) %>%
  # Group by whether or not its a complaint
  group_by(complaint_label) %>%
  # Keep the top 20 words
  top_n(20, n) %>%
  # Ungroup before reordering word as a factor by the count
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

# tidy_twitter has been tokenized and stop words, including custom stop words, have been removed. 
# You would like to visualize the differences in word counts based on complaints and non-complaints.

# Include a color aesthetic tied to whether or not its a complaint
ggplot(word_counts, aes(x = word2, y = n, fill=complaint_label)) +
  # Don't include the lengend for the column plot
  geom_col(show.legend = F) +
  # Facet by whether or not its a complaint and make the y-axis free
  facet_wrap(~complaint_label, scales = 'free_y') +
  # Flip the coordinates and add a title: "Twitter Word Counts"
  coord_flip() +
  ggtitle("Twitter Word Counts")

# bar plots are effective to look at frequently occuring words in each category.

# What is text visualization without a word cloud?

# Load the wordcloud package
library(wordcloud)

# Compute word counts and assign to word_counts
word_counts <- tidy_twitter %>% 
  count(word)

wordcloud(
  # Assign the word column to words
  words = word_counts$word, 
  # Assign the count column to freq
  freq = word_counts$n,
  max.words = 30
)

# The dominance of flight is readily apparent! 
# We should consider whether or not this is another custom stop word.

# Compute complaint word counts and assign to word_counts
word_counts <- tidy_twitter %>% 
  filter(complaint_label == 'Complaint') %>% 
  count(word)

# Create a complaint word cloud of the top 50 terms, colored red
wordcloud(
  words = word_counts$word, 
  freq = word_counts$n, 
  max.words = 50, 
  colors = "red"
)



