options(stringsAsFactors = F)

# let's load some required packages
library(cleanNLP)
library(dplyr)


# now load the preprocessed data
load("data/speeches_processed.RData")

# ... and get the token table from it
token_table <- cnlp_get_token(speeches)


# Frequency over time
# ======================================

# We choose som terms for a time series plot
selected_terms <- c("war", "slave", "soviet", "terror", "hope")

# This block counts our selected terms per decade by combining metadata
counts_per_decade <- token_table %>%
  filter(lemma %in% selected_terms) %>%
  left_join(sotu, by = "id") %>%
  mutate(decade = paste0(substring(date, 0, 3), "0")) %>%
  group_by(decade) %>%
  count(lemma)

# We plot the time series
require(ggplot2)
ggplot(counts_per_decade, aes(x = decade, y = n, group = lemma, color = lemma)) +
  geom_line(size = 1) + theme(axis.text.x = element_text(angle = 90, hjust = 1))





# Key terms
# ===========================================

# We extract important vocabulary from Abe Lincoln based on log-likelihood statistics
source("src/calculateLogLikelihood.R")

# First, we count all terms in Lincolns speeches and only keep those occurring > 2
countsTargetTable <- token_table %>%
  left_join(sotu, by = "id") %>%
  filter(president == "Abraham Lincoln") %>%
  count(lemma) %>% 
  filter(n > 2)
  

# For statistical test of overuse of terms, we count the vocabulary in all other speeches
countsComparisonTable <- token_table %>%
  left_join(sotu, by = "id") %>%
  filter(president != "Abraham Lincoln") %>%
  count(lemma)

# Now, we transform the counts table into a named vector
termCountsTarget <- countsTargetTable$n
names(termCountsTarget) <- countsTargetTable$lemma
termCountsComparison <- countsComparisonTable$n
names(termCountsComparison) <- countsComparisonTable$lemma

# ... to use it in the function to determine the ll-significance of Abe's vocabulary
source("src/calculateLogLikelihood.R")
llSignificance <- calculateLogLikelihood(termCountsTarget, termCountsComparison)

# We extract the top 25 terms most distinct for Abe's speeches
top25 <- sort(llSignificance, decreasing = T)[1:25]

# And display them as a workdcloud
require(wordcloud2)
top25_df <- data.frame(
  word = names(top25),
  frq = top25
)
wordcloud2(top25_df)


# Cooccurrence analysis
# ================================================

# Statistical significance of co-ocurring terms can be determined by 
# functions provided in the widyr package
library(widyr)

# We extract nouns occurring > 10 times and compute their pearson's
# correlation of occurring together in sentences
word_cooccurrences <- token_table %>%
  left_join(sotu, by = "id") %>%
  filter(president == "Barack Obama") %>%
  group_by(lemma) %>%
  filter(upos == "NOUN") %>%
  filter(n() > 10) %>%
  mutate(d_s_id = paste0(id, "_", sid)) %>%
  pairwise_cor(lemma, d_s_id, sort = TRUE, method = "pearson")

# What are the most significant co-occurrences?
word_cooccurrences

# What are the most significant co-occurrences for the term "change"?
word_cooccurrences %>%
  filter(item1 == "change")

# Let's display co-occurrence as a graph network
require(igraph)
require(ggraph)
word_cooccurrences %>%
  top_n(150, correlation) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



# Named Entities
# =================================

# Spacy also annotates named entities
entity_table <- cnlp_get_entity(speeches)

# ... such as Persons, Locations, Languages, ...
unique(entity_table$entity_type)

# ... in sentences of all documents
entity_table

# With this, We can extract co-occurrence of person names
person_cooccurrences <- entity_table %>%
  filter(entity_type == "PERSON") %>%
  filter(grepl(" ", entity)) %>%
  group_by(entity) %>%
  filter(n() > 1) %>%
  mutate(d_s_id = paste0(id, "_", sid)) %>%
  pairwise_cor(entity, d_s_id, sort = TRUE, method = "pearson")

View(person_cooccurrences)

# and again draw a network of Persons co-occurring in sentences
person_cooccurrences %>%
  filter(correlation > 0) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()




# Topic models
# ==================================


library(topicmodels)
library(tidytext)

# First, we need to create a document-term-matrix as input for the LDA process
dtm <- token_table %>%
  filter(upos == "NOUN") %>%
  mutate(pseudoc_id = ceiling(seq_along(1:n()) / 200)) %>%
  mutate(count = 1) %>%
  cast_dtm(pseudoc_id, lemma, count)

class(dtm)
dim(dtm)
colnames(dtm)

# Compute the LDA model
topicmodel <- LDA(dtm, k = 16, method = "Gibbs", control = list(alpha = 0.1, iter = 250, seed = 1234, verbose = 1))

topicmodel

# Extract the topic-term-distributions (beta) and bring them into a tidy format
chapter_topics <- tidy(topicmodel, matrix = "beta")
chapter_topics

# Now extract the top 10 terms for each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# ... and plot them as bar plot.
require(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
