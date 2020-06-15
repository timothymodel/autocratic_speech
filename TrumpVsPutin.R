##### If you do not see below Russian cyrillic,
##### file must be opened with UTF-8 Encoding

##### Load Relevant Packages #####

library(tidyr)
library(dplyr)
library(ggplot2)
library(quanteda)
library(pals)
library(tidytext)

setwd("C:/Users/model/OneDrive/Teaching/Text as Data/Analysis/TrumpVsPutin/")

trump <- readtext::readtext("./trump_sotu/*.txt")
putin <- readtext::readtext("./putin_address/*.txt")

##### Pre-Process Text #####

## Trump ##

trump_corpus <- corpus(trump$text)

trump_corpus <- trump_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english")) %>%
  tokens_wordstem(language = "english")

trump_dtm <- trump_corpus %>%
  dfm()

textstat_frequency(trump_dtm, n = 10) %>% 
  head(., 10)

## Putin ##

putin_corpus <- corpus(putin$text)

putin_corpus <- putin_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))

putin_dtm <- putin_corpus %>%
  dfm()

textstat_frequency(putin_dtm, n = 10) %>% 
  head(., 10)

##### Sentiment #####

trump_corpus <- corpus(trump$text)

trump_corpus <- trump_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))

dictfile <- tempfile()
download.file("https://provalisresearch.com/Download/LaverGarry.zip",
              dictfile, mode = "wb")
unzip(dictfile, exdir = (td <- tempdir()))
dictlg <- dictionary(file = paste(td, "LaverGarry.cat", sep = "/"))

trump_negsum <-
  dfm(trump_corpus, dictionary =  data_dictionary_LSD2015[1]) %>%
  sum()

trump_possum <-
  dfm(trump_corpus, dictionary =  data_dictionary_LSD2015[2]) %>%
  sum()

trump_negsum/trump_possum

putin_negsum <-
  dfm(putin_corpus, dictionary =  data_dictionary_LSD2015[1]) %>%
  sum()

putin_possum <-
  dfm(putin_corpus, dictionary =  data_dictionary_LSD2015[2]) %>%
  sum()

putin_negsum/putin_possum

(1/3)/(1/2)


##### Topic Models #####

library(topicmodels)

trump_corpustm <- trump_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE,
    remove_url = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("english"),
                  "applause", 
                  "american",
                  "americans",
                  "america",
                  "thank",
                  "year",
                  "must",
                  "states",
                  "united"), 
                case_insensitive = TRUE) %>%
  tokens_wordstem(language = "english") %>% 
  tokens_remove(c("peopl", 
                  "can", 
                  "work", 
                  "develop", 
                  "countri",
                  "tonight",
                  "also",
                  "just",
                  "new",
                  "one",
                  "us",
                  "get"))

putin_corpustm <- putin_corpus %>%
  tokens(
    remove_punct = TRUE,
    remove_numbers = TRUE,
    remove_symbols = TRUE
  ) %>%
  tokens_tolower() %>%
  tokens_remove(c(stopwords("english"),
                  "year",
                  "russia",
                  "russians",
                  "also",
                  "must",
                  "need")) %>%
  tokens_wordstem(language = "english") %>% 
  tokens_remove(c("peopl", 
                  "can", 
                  "work", 
                  "develop", 
                  "countri",
                  "region"))

trump_tmdtm <- trump_corpustm %>%
  dfm() %>%
  convert(to = "topicmodels")

putin_tmdtm <- putin_corpustm %>%
  dfm() %>%
  convert(to = "topicmodels")

trump_lda <- LDA(trump_tmdtm, k = 3)
putin_lda <- LDA(putin_tmdtm, k = 3)

terms(trump_lda, 10)
terms(putin_lda, 10)

##### Network #####

set.seed(100)

## Trump

trump_corpus <- corpus(trump$text)

toks <- trump_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(c("americans", "america's"), 
                 c("american", "america")) %>% 
  tokens_remove(pattern = c(stopwords("english"),
                "thank", "you", "tonight", "applause",
                "can", "must", "much", "many", "united",
                "states", "two", "just",  "year", "like",
                "also", "finally", "come", "years", "ago",
                "want", "done", "now", "since", "get", 
                "every", "always", "ever", "time", "last",
                "know"),
                padding = FALSE)

fcmat <- fcm(toks, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 40))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = 0.25,
                   edge_alpha = .6,
                   edge_color = "#f4a582",
                   vertex_size = 1,
                   vertex_labelsize = 5
                   )

## Putin

putin_corpus <- corpus(putin$text)

toks <- putin_corpus %>%
  tokens(remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_symbols = TRUE) %>%
  tokens_tolower() %>%
  tokens_replace(c("russia's", "economic"), 
                 c("russia", "economy")) %>% 
  tokens_remove(pattern = c(stopwords("english"),
                            "make", "next", "must", "time",
                            "even", "can", "know", "now",
                            "also", "every", "need", 
                            "including", "like", 
                            "today", "want",
                            "important", "say", "well",
                            "years", "just", "year",
                            "let", "many", "necessary",
                            "see", "already",
                            "use", "take", "said"),
                padding = FALSE)

fcmat <- fcm(toks, context = "window", tri = FALSE)
feat <- names(topfeatures(fcmat, 40))
fcm_select(fcmat, pattern = feat) %>%
  textplot_network(min_freq = 0.25,
                   edge_alpha = .6,
                   edge_color = "#a6cee3",
                   vertex_size = 1,
                   vertex_labelsize = 5
  )

