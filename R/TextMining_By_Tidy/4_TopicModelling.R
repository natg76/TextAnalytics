##-------------------------------------------------------------###
##
## Course: Text Analytics in R 
##
## Chapter 4 - Topic Modelling
## Natarajan G
## 27/Dec/2019
##
##-------------------------------------------------------------###


# 1 Load data to analyze
library(tm)
#library(topicmodels)


# Start with the topics output from the LDA run
lda_topics %>% 
  # Arrange the topics by word probabilities in descending order
  arrange(desc(beta))


# Produce a grouped summary of the LDA output by topic
lda_topics %>% 
  group_by(topic) %>% 
  summarize(
    # Calculate the sum of the word probabilities
    sum = sum(beta),
    # Count the number of terms
    n = n()
  )


# Note that since the topics are word probabilities, the sum of all probabilities for each topic equals 1. 
# Also, since each topic includes every term in the corpus, the counts are equal across topics.

word_probs <- lda_topics %>%
  # Keep the top 10 highest word probabilities by topic
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  # Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term, beta))

# Plot term2 and the word probabilities
ggplot(word_probs, aes(x=term2, y=beta)) +
  geom_col() +
  # Facet the bar plot by topic
  facet_wrap(~topic, scales = "free") +
  coord_flip()


# Given the terms occuring with high probability, the visualiation helps us interpret the topics. 
# For example, it seems likely that the first topic is related to complaints.

# Start with the tidied Twitter data
tidy_twitter %>% 
  # Count each word used in each tweet
  count(word, tweet_id) %>% 
  # Use the word counts by tweet to create a DTM
  cast_dtm(tweet_id, word, n)


# Assign the DTM to dtm_twitter
dtm_twitter <- tidy_twitter_subset %>% 
  count(word, tweet_id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(tweet_id, word, n)

# Coerce dtm_twitter into a matrix called matrix_twitter
matrix_twitter <- as.matrix(dtm_twitter)

# Print rows 1 through 5 and columns 90 through 95
print(matrix_twitter[1:5,90:95])

# The matrix is far too large to print all at once, but by indexing it we get a sense for its structure and sparsity.

library(topicmodels)


# Fitting an LDA
# It's time to run your first topic model! 
# As discussed, the three additional arguments of the LDA() function are critical for properly running a topic model. 
# Note that running the LDA() function could take about 10 seconds. 
# The tidyverse and tidytext packages along with the tidy_twitter dataset have been loaded for you.


# Load the topicmodels package
library(topicmodels)

# Cast the word counts by tweet into a DTM
dtm_twitter <- tidy_twitter %>% 
  count(word, tweet_id) %>% 
  cast_dtm(tweet_id, word, n)

# Run an LDA with 2 topics and a Gibbs sampler
lda_out <- LDA(
  dtm_twitter,
  k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)

# Remember that the Gibbs sampler can take some time to run, depending on the amount of data and the number of topics specified.

# Glimpse the topic model output
glimpse(lda_out)

# Tidy the matrix of word probabilities
lda_topics <- lda_out %>% 
  tidy(matrix = "beta")

# Arrange the topics by word probabilities in descending order
lda_topics %>% 
  arrange(desc(beta))


#  Casting from and into a tidy format makes it trivial for us to evaluate the output of one (or more) topic models.

# Run an LDA with 3 topics and a Gibbs sampler
lda_out2 <- LDA(
  dtm_twitter,
  k=3,
  method = "Gibbs",
  control = list(seed = 42)
)

# Tidy the matrix of word probabilities
lda_topics2 <- lda_out2 %>% 
  tidy(matrix = "beta")

# Arrange the topics by word probabilities in descending order
lda_topics2 %>% 
  arrange(desc(beta))

# From the top word probabilities, do the first two topics look the same? What does the third topic appear to be about?




# Select the top 15 terms by topic and reorder term
word_probs2 <- lda_topics2 %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word probs, color and facet based on topic
ggplot(
  word_probs2, 
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Topic 1 is clearly about flights. Topic 2 appears to be about the amenities surrounding flights. Topic 3 looks to be about the airlines. You may have interpreted this differently, which is just fine - just be clear as to why you've named them the way you have.
# Results are different..

# Now let's compare the previous solution with a four topic model, lda_topics3.

# Select the top 15 terms by topic and reorder term
word_probs3 <- lda_topics3 %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word_probs3, color and facet based on topic
ggplot(
  word_probs3, 
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Are these topics different from each other? Should you keep adding topics or is this enough? How does this compare to the previous solution? I interpreted these as follows: Topic 1 is about flights, topic 2 is about airlines, topic 3 is about service, and topic 4 is about planes

