#Loading required packages for basic Summary:

library(dplyr)
library(ggplot2)
library(stringr)

#Read data into R

manual <- read.delim("D:/Portfolio/Data Engineer Track/Python/Uzaki chan Text Analytics/Transcriptions/S1_S2_combined/S1_S2_combined.txt", header = T, stringsAsFactors = F)

#Remove 's and replace with empty space. 's was considered a verb by udpipe. Not very useful for analysis. 

manual$Now.there.s.a.sight.for.sore.eyes..Nothing.says.spring.quite.like.this..Oh..hey..Uzaki..right.<- str_replace(manual$Now.there.s.a.sight.for.sore.eyes..Nothing.says.spring.quite.like.this..Oh..hey..Uzaki..right., "'s", "")

library(udpipe)
model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')

#Annotate Input Text Data 

s <- udpipe_annotate(udmodel_english, manual$Now.there.s.a.sight.for.sore.eyes..Nothing.says.spring.quite.like.this..Oh..hey..Uzaki..right.)

x <- data.frame(s)

#Universal part-of-speech tags from the given text

library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "#6497bf", 
         main = "Universal Parts of Speech", xlab = "Freq"
)

# Most Occurring Nouns

stats <- subset(x, upos %in% c("NOUN"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "#9fcbee",
         main = "Most Occurring Nouns", xlab = "Freq")


# Most Occurring Adjectives
stats <- subset(x, upos %in% c("ADJ"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "#01016f",
         main = "Most Occurring Adjectives", xlab = "Freq")     

# Verbs
stats <- subset(x, upos %in% c("VERB"))
stats <- txt_freq(stats$token)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 20), col = "#d8031c",
         main = "Most Occurring Verbs", xlab = "Freq")

# Using RAKE - Keywords Extraction 

stats<- keywords_rake(x = x, term = "lemma", group = "doc_id",
                      relevant = x$upos %in% c("NOUN", "ADJ"))

stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ rake, data = head(subset(stats, freq > 3), 20), col = "#5a5a5a",
         main = "Entire Series Keyword Extraction",
         xlab = "RAKE")

# Top Noun-Verb Pairs as Keyword Pairs / Using a sequence of POS tags (noun/phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token),
                          pattern = "(A|N)*N(P+D*(A|N)*N)*",
                          is_regex = TRUE, detailed = FALSE)

stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "#FFF08D",
         main = "Keywords - Simple Noun Phrases", xlab = "Frequency")                          

stats <- merge(x, x, 
               by.x = c("doc_id", "paragraph_id", "sentence_id", "head_token_id"),
               by.y = c("doc_id", "paragraph_id", "sentence_id", "token_id"),
               all.x = TRUE, all.y = FALSE, 
               suffixes = c("", "_parent"), sort = FALSE)

#Word Cloud 
stats <- subset(stats, dep_rel %in% "nsubj" & upos %in% c("NOUN") & upos_parent %in% c("ADJ"))
stats$term <- paste(stats$lemma_parent, stats$lemma, sep = " ")
stats <- txt_freq(stats$term)
library(wordcloud)
wordcloud(words = stats$key, freq = stats$freq, min.freq = 1, max.words = 100,
          random.order = FALSE, colors = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"))


## Using Pointwise Mutual Information Collocations
x$word <- tolower(x$token)
stats <- keywords_collocation(x = x, term = "word", group = "doc_id")
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ pmi, data = head(subset(stats, freq > 3), 20), col = "#9fcbee", 
         main = "Entire Series Keyword", 
         xlab = "PMI (Pointwise Mutual Information)")


## Keyword Correlations

x$id <- unique_identifier(x, fields = c("sentence_id", "doc_id"))
dtm <- subset(x, upos %in% c("NOUN", "ADJ"))
dtm <- document_term_frequencies(dtm, document = "id", term = "lemma")
dtm <- document_term_matrix(dtm)
dtm <- dtm_remove_lowfreq(dtm, minfreq = 5)
termcorrelations <- dtm_cor(dtm)
y <- as_cooccurrence(termcorrelations)
y <- subset(y, term1 < term2 & abs(cooc) > 0.2)
y <- y[order(abs(y$cooc), decreasing = TRUE), ]
head(y)



# Co-occurrences - Nouns / adjectives used in same sentence

cooc <- cooccurrence(x = subset(x, upos %in% c("NOUN", "ADJ")), 
                     term = "lemma", 
                     group = c("doc_id", "paragraph_id", "sentence_id"))
head(cooc)

# Co-occurrences - Plots 

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "pink") +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  theme(legend.position = "none") +
  labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")




# Nouns / adjectives which follow one another

cooc <- cooccurrence(x$lemma, relevant = x$upos %in% c("NOUN", "ADJ"), skipgram = 1)
head(cooc)

library(igraph)
library(ggraph)
library(ggplot2)
wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc)) +
  geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
  theme_graph(base_family = "Arial Narrow") +
  labs(title = "Words following one another", subtitle = "Nouns & Adjective")






