# import the needed libraries
library(tidyverse)
library(tidytext)
library(igraph)

# load data
setwd("####")
load("Sentiment_data.RData")
load("data.RData")
character_names<-c("Michael","Dwight","Jim","Pam","Andy","Angela","Kevin","Oscar")


# TEXT ANALYSIS

sentiment_data1 <- sentiment_data %>% filter(!grepl("[[:digit:]]", word)) %>%   # here we remove all characters that contain numbers 
  ungroup()

#here we generate the couple of words 

sntm1 <- sentiment_data1 %>% mutate(word2 = word) %>% 
  group_by(season, n_episode, n_line) %>% 
  mutate(word2 = lead(word2)) %>% 
  mutate(bin = paste(word,word2)) %>% 
  na.omit()

#count them
tab <- table(sntm1$bin)

# Sort table in ascending order
sorted_tab <- tab[order(tab, decreasing = TRUE)] 

#plot
pltdt <- data.frame(head(sorted_tab, 20))
names(pltdt) <- c("expression", "frequency")

ggplot(pltdt) +
  geom_col(aes(x = frequency, 
               y = fct_rev(fct_infreq(expression)),
               fill = frequency)) +
  theme_bw() +
  ylab("expressions") +
  labs(caption = "Figure: most common expression in the sitcom") +
  labs(caption = "Figure: most common expression in the sitcom") +
  theme(plot.caption = element_text(size=10,
                                    hjust=0,
                                    color="darkblue", 
                                    face="italic"))

# Percentage of lines for each character

dt_lines_fr <- data.frame(sort(table(sentiment_data1$speaker),
                               decreasing = T)[1:9][-8])
names(dt_lines_fr) <- c("character", "frequency")
ggplot(dt_lines_fr) +
  geom_col(aes(x = frequency,
               y = fct_rev(fct_infreq(character)),
               fill = frequency)) +
  theme_bw() +
  ylab("character")

# Number of times a specific character get nominated during the sitcom

nominatesdt <- data %>%
  group_by(season, episode) %>% 
  unnest_tokens(word, line) %>% 
  mutate(word = tolower(word)) %>% 
  mutate(
    michael = grepl("michael", word),
    jim = grepl("jim", word),
    pam = grepl("pam", word),
    dwight = grepl("dwight", word),
    angela = grepl("angela", word),
    kevin = grepl("kevin", word),
    oscar = grepl("oscar", word),
    andy = grepl("andy", word)
  ) %>% 
  group_by(season) %>% 
  mutate(michael = sum(michael),
         jim = sum(jim),
         pam = sum(pam),
         dwight = sum(dwight),
         angela = sum(angela),
         kevin = sum(kevin),
         oscar = sum(oscar),
         andy = sum(andy)) %>% 
  pivot_longer(cols = 4:11) %>% 
  select(-word) %>% 
  distinct()

nominatesdt %>% group_by(name) %>% ggplot() +
  geom_line(aes(x = season,
                y = value,
                color = name)) +
  theme_bw() +
  ylab("how many times characters are nominated") +
  labs(caption = "Figure:  how much other characters talk about a specific character") +
  theme(plot.caption = element_text(size=10,
                                    hjust=0,
                                    color="darkblue", 
                                    face="italic")) 

# undirected social network

## Identify the interactions
data_net <- data %>%  filter(season <= 7) %>% 
  select(season, episode, scene, speaker) %>% 
  mutate(speaker2 = speaker) %>% 
  group_by(season, episode, scene) %>% 
  mutate(speaker2 = lead(speaker2)) %>% 
  ungroup() %>% 
  select(speaker, speaker2)

# construct the relationship matrix and the number of interaction
data_net <- data_net %>% 
  na.omit() %>% 
  mutate(check = case_when(speaker == speaker2 ~ "REMOVE",
                           !(speaker == speaker2) ~ "KEEP")) %>% 
  filter(check == "KEEP") %>% 
  select(-check) %>% 
  mutate(number = 1)

data_net <- data_net %>% group_by(speaker, speaker2) %>% mutate(number = sum(number)) %>% 
  distinct() %>% 
  arrange(-number) %>% 
  filter(speaker %in% character_names & speaker2 %in% character_names) %>% 
  group_by(speaker) %>% 
  arrange(- number, .by_group = TRUE) %>% 
  ungroup()

data_net1 <- data_net

names(data_net)[2:3] <- c("interlocutor", "discussions")

data_net <- data_net %>% 
  group_by(speaker) %>% 
  arrange(interlocutor, .by_group = TRUE) %>%  
  mutate(discussions = (discussions * 10) /max(data_net1$discussions))

data_net2 <- data_net

data_net <- data_net %>% mutate(discussions = 1) %>% 
  pivot_wider(values_from = discussions,names_from = interlocutor) %>% 
  relocate(Andy, .before = Angela)

row.names(data_net) <- as.character(data_net$speaker)
matrix_data <- as.matrix(data_net)[,-1]
class(matrix_data) <- "numeric"
network <- graph_from_adjacency_matrix(matrix_data , mode='directed', diag=F )
E(network)$width <- data_net2$discussions 

network_just_to_show <- graph_from_adjacency_matrix(matrix_data , mode='undirected',diag=F)
network_just_to_show 

plot(network,
     mode = "kamadakawai",
     vertex.label.font = 2,
     vertex.label.color= "black",
     vertex.color = "grey",
     edge.arrow.size= 0)

# Show ho much character X interact with character Y

data_net <- data %>%  filter(season <= 7) %>% 
  select(season, episode, scene, speaker) %>% 
  mutate(speaker2 = speaker) %>% 
  group_by(season, episode, scene) %>% 
  mutate(speaker2 = lead(speaker2)) %>% 
  ungroup() %>% 
  select(speaker, speaker2)

data_net2 <- data_net %>% 
  na.omit() %>% 
  mutate(check = case_when(speaker == speaker2 ~ "REMOVE",
                           !(speaker == speaker2) ~ "KEEP")) %>% 
  filter(check == "KEEP") %>% 
  select(-check) %>% 
  mutate(number = 1)

data_net3 <- data_net2 %>% group_by(speaker, speaker2) %>% mutate(number = sum(number)) %>% 
  distinct() %>% 
  arrange(-number) %>% 
  filter(speaker %in% character_names & speaker2 %in% character_names) %>% 
  group_by(speaker2) %>% 
  arrange(- number, .by_group = TRUE) %>% 
  ungroup()

names(data_net3)[2:3] <- c("interlocutor", "discussions")

ggplot(data_net3, aes(fill = interlocutor)) +
  geom_col(aes(discussions, speaker)) +
  facet_wrap(~ factor(interlocutor, 
                      c("Michael", "Dwight", "Jim", "Pam", "Andy", "Angela", "Oscar", "Kevin"))) +
  theme_bw() +
  labs(caption = "Figure: to whom people talk most") +
  theme(plot.caption = element_text(size=10,
                                    hjust=0,
                                    color="darkblue", 
                                    face="italic"))
