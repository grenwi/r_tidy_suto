
options(stringsAsFactors = F)

# load readtext package for data import
require(readtext)

# read in speeches of US presidents
sotu <- readtext("data/sotu", 
                 docvarsfrom = "filepaths", 
                 dvsep = "/", 
                 docvarnames = c("dir1", "dir2", "president", "date", "id"))

# Notice: paths structures are appended as metadata-colums to document texts
View(sotu)

# Now we initiate the preprocessing with cleanNLP / spaCy
require(dplyr)
require(cleanNLP)

# Load the english space model
cnlp_init_spacy()

# Annotate documents with sentence, token, lemma, POs, NE and dependency information
# This may take a while ...
speeches <- cnlp_annotate(sotu$text, as_strings = TRUE, doc_ids = sotu$id)

# What annotations do we have?
names(speeches)

# Let's keep this preprocessing result for later use!
save(speeches, sotu, file = "data/speeches_processed.RData")


# How to extract information from annotated text documents
# ========================================================

# documents table
cnlp_get_document(speeches)

# toke table: tidy format with one token per row + annotations + metadata
token_table <- cnlp_get_token(speeches)
View(token_table)


# This tidy format can be easily manipulated with dplyr functions chained by the %>% operator
# e.g. to count words and display the top 20
token_table %>%
  count(word, sort = TRUE) %>%
  top_n(20)


# or count POS-tags
token_table %>%
  count(upos, sort = TRUE) 


# or count nouns only
token_table %>%
  filter(upos == "NOUN") %>%
  count(word, sort = TRUE) 


# or their lemma
token_table %>%
  filter(upos == "NOUN") %>%
  count(lemma, sort = TRUE) 


# or proper nouns
token_table %>%
  filter(upos == "PROPN") %>%
  count(word, sort = TRUE) 


# by joining a metadata table to the token table, we can obtain countings grouped by president
token_table %>%
  filter(upos == "NOUN") %>%
  left_join(sotu) %>%
  group_by(president) %>%
  count(lemma) %>%
  top_n(10) %>%
  arrange(president, desc(n))


# or even select complex adjectve-noun patterns per president
token_table %>%
  left_join(sotu) %>%
  mutate(prev_tag = lag(upos), prev_word = lag(word)) %>%
  filter(upos == "NOUN", prev_tag %in% c("ADJ", "ADV")) %>%
  group_by(president) %>%
  count(prev_word, word) %>%
  top_n(5) %>%
  arrange(president, desc(n))



# or noun-noun patterns
token_table %>%
  left_join(sotu) %>%
  mutate(prev_tag = lag(upos), prev_word = lag(word)) %>%
  filter(upos %in% c("NOUN"), prev_tag %in% c("NOUN")) %>%
  group_by(president) %>%
  count(prev_word, word) %>%
  top_n(5) %>%
  arrange(president, desc(n))


