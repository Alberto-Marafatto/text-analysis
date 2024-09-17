# import the needed libraries
library(tidyverse)
library(tidytext)
library(textstem)
library(dundermifflin)

# import the lines from The Office + data manipulation
setwd("####")
data <- dundermifflin::office_quotes[,-1]
names(data) <- c("season", "episode", "title", "scene", "speaker", "line")
episodes<-data %>% select(season,episode) %>% distinct()
episodes$n_episode<-1:nrow(episodes)

data<-data %>% left_join(episodes,by=c("season","episode"))

data<-data %>% filter(season < 8)

character_names<-c("Michael","Dwight","Jim","Pam","Andy","Angela","Kevin","Oscar")

# Introduction to the sitcom thorugh text analysis (the most frequent couple of words)

sentiment_data <- data %>% 
  mutate(n_line = row_number()) %>% 
  group_by(season, n_episode, n_line, speaker) %>% 
  unnest_tokens(word, line) # unnest the code

sentiment_data <- sentiment_data %>% 
  mutate(n_word = row_number()) %>% 
  mutate(word = lemmatize_words(word))  # lemmatize the words


sentiment_data <- sentiment_data %>% 
  filter(!(word %in% c('uh', 'lot', 'alright', 'um',  'yeah', 'gonna', 'hey')) & # remove common words that are not significant
           !(word %in% tolower(unique(data$speaker))) & # we remove the characters
           !(word %in% stop_words$word)) # remove stop words

save(sentiment_data, file = "Sentiment_data.RData")
save(data, file = "data.RData")
