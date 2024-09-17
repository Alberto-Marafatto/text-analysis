# import the needed libraries
library(tidyverse)
library(tidytext)
library(textstem)

# load data
setwd("####")
load("Sentiment_data.RData")
load("data.RData")
character_names<-c("Michael","Dwight","Jim","Pam","Andy","Angela","Kevin","Oscar")

# BRIEF SENTIMENT ANALYSIS

#Get the AFINN sentiment dictionary
afinn <- get_sentiments(lexicon = c("afinn"))
sentiment_data <- sentiment_data %>% 
  group_by(n_episode, speaker) %>% 
  count(word, sort = T) %>% 
  ungroup() %>% 
  inner_join(afinn, by = join_by(word)) %>% 
  mutate(sentiment_value = n * value)

sentiment_df <- sentiment_data %>% group_by(n_episode, speaker) %>% 
  mutate(sentiment_value_acc = sum(sentiment_value)) %>% 
  select(n_episode, speaker, sentiment_value_acc) %>% 
  distinct() %>% 
  filter(speaker %in% character_names) %>% 
  ungroup()

# Lemmatize the words and Score the words

sentiment_df <- sentiment_df %>% group_by(speaker) %>% 
  arrange(n_episode, .by_group=TRUE) %>% 
  ungroup()

# create the list
dt_data <- list()
n <- 1
for (i in character_names) {
  dt_data[[n]] <- sentiment_df %>% filter(speaker == i)
  n <- n + 1
}

# substitute the missing data with 0 (because the character is missing)
missing_fix <- tibble(n_episode = 1:186, sentiment_value_acc_fix = 0)

sub_miss <- function(sent_data){
  speak <- sent_data[[2,2]]
  fix_name <- paste("fixed", speak, sep = "_")
  sent_data <- sent_data %>% full_join(missing_fix, by = join_by(n_episode)) %>% 
    mutate(fix = case_when(is.na(sentiment_value_acc) ~ "YES",
                           !is.na(sentiment_value_acc) ~ "NO"),
           sentiment_value_acc = case_when(is.na(sentiment_value_acc) ~ sentiment_value_acc_fix,
                                           !is.na(sentiment_value_acc) ~ sentiment_value_acc)) %>% 
    select(-sentiment_value_acc_fix) %>% 
    arrange(n_episode) %>% 
    select(-speaker)
  names(sent_data)[2] <- speak
  names(sent_data)[3] <- fix_name
  return(sent_data)
}

dt_data1 <- lapply(dt_data, sub_miss)

# merge data 
dt_tot <- dt_data1[[1]] %>%  full_join(dt_data1[[2]], 
                                       by = join_by(n_episode)) %>% 
  full_join(dt_data1[[3]], 
            by = join_by(n_episode)) %>% 
  full_join(dt_data1[[4]], 
            by = join_by(n_episode)) %>%
  full_join(dt_data1[[5]], 
            by = join_by(n_episode)) %>%
  full_join(dt_data1[[6]], 
            by = join_by(n_episode)) %>%
  full_join(dt_data1[[7]], 
            by = join_by(n_episode)) %>%
  full_join(dt_data1[[8]], 
            by = join_by(n_episode))

# plot sentiment score over episodes
sentiment_df %>% filter(n_episode <= 139)  %>% 
  ggplot(aes(x = n_episode,
             y = sentiment_value_acc)) +
  geom_point() +
  facet_wrap(~ factor(speaker, 
                      c("Michael", "Dwight", "Jim", "Pam", "Andy", "Angela", "Oscar", "Kevin"))) + 
  theme_bw() +
  ylab("sentiment score") +
  xlab("episode number") +
  labs(caption = "Figure: Sentiment score over the first seven seasons") +
  theme(plot.caption = element_text(size=10,
                                    hjust=0,
                                    color="darkblue", 
                                    face="italic"))

