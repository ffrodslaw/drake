library(tidyverse)
devtools::install_github('josiahparry/geniusR')
library(genius)      
library(wordcloud2)
library(tidytext)
library(data.table)
library(rvest)
library(jcolors)

source("script/genius_mod.R")


# download all full length records

record_titles <- c("Drake Demo Disk", "Room for Improvement", "Comeback Season", "So Far Gone", "Thank Me Later"
                   , "Young Sweet Jones", "Young Sweet Jones 2", "Take Care", "Nothing Was the Same"
                   , "If You're Reading This It's Too Late", "Views", "More Life", "Scary Hours", "Scorpion"
                   , "The Best in the World Pack", "Care Package", "Dark Lane Demo Tapes", "Scary Hours 2"
                   , "Certified Lover Boy", "Honestly, Nevermind", "Scary Hours 3*")

studio_titles <- c("Thank Me Later", "Take Care", "Nothing Was the Same", "Views", "Scorpion", "Certified Lover Boy", "Honestly, Nevermind")
studio_years <- c(2010, 2011, 2013, 2016, 2018, 2021, 2022)
studio_records <- data.frame(album = studio_titles, year = studio_years)

mixtape_titles <- c("Room for Improvement", "Comeback Season", "So Far Gone", "If You're Reading This It's Too Late"
                    , "More Life", "Dark Lane Demo Tapes")
mixtape_years <- c(2006, 2007, 2009, 2015, 2017, 2020)
mixtape_records <- data.frame(album = mixtape_titles, year = mixtape_years)


# get track names

studio_tracks <- studio_titles |> 
  map(genius_tracklist, artist = "Drake") |> 
  rbindlist() 

mixtape_tracks <- mixtape_titles |> 
  map(genius_tracklist, artist = "Drake") |> 
  rbindlist() 


studio_lyrics = data.frame()
for (i in 1:nrow(studio_tracks)){
  print(i)
  out = genius_url_mod(studio_tracks[i,])
  studio_lyrics  <- rbind(studio_lyrics, out)
}

mixtape_lyrics = data.frame()
for (i in 1:nrow(mixtape_tracks)){
  print(i)
  out = genius_url_mod(mixtape_tracks[i,])
  mixtape_lyrics  <- rbind(mixtape_lyrics, out)
}

save(studio_lyrics, file = "data/studio_lyrics_verses.Rdata")
save(mixtape_lyrics, file = "data/mixtape_lyrics_verses.Rdata")


###############

# Data exploration

# tokenize and remove stop words, get years
studio_words <- unnest_tokens(studio_lyrics, word, lyric) |>
  anti_join(stop_words) |> 
  inner_join(studio_records)

# most commonly used words (totals)
most_used_words <- studio_words |> 
  group_by(word) |> 
  summarize(n = n()) |> 
  arrange(desc(n)) |> 
  head(10) |> 
  mutate(word = forcats::fct_rev(forcats::fct_inorder(word))) |> 
  ggplot(aes(x=n, y=word, fill = word) ) + geom_col() + 
  geom_text(aes(label = n), hjust = 1.25, nudge_x = -.5, size = 4, fontface = "bold", family = "Fira Sans", color="white")  +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_jcolors("pal11") +
  theme_void() + 
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"), plot.margin = margin(rep(15, 4)), 
    plot.title = element_text(size = 20, hjust = 0.5, family = "Fira Sans",margin = margin(0,0,15,0))) +
  ggtitle("Most commonly used words in Drake's studio albums") +
  guides(color = "none", fill = "none")

most_used_words

# most commonly used words per song
most_used_words_per_song <- studio_words |> 
  group_by(word) |> 
  summarize(n = n()/133) |> 
  arrange(desc(n)) |> 
  head(10) |> 
  mutate(n = round(n,2)) |> 
  mutate(label_t = ifelse(row_number() == 1, paste(n, "average uses per song",sep=" "), n)) |> 
  mutate(word = forcats::fct_rev(forcats::fct_inorder(word))) |> 
  ggplot(aes(x=n, y=word, fill = word) ) + geom_col() + 
  geom_text(aes(label = label_t), hjust = 1, nudge_x = -.5, size = 4, fontface = "bold", family = "Fira Sans", color="white")  +
  scale_x_continuous(expand = c(.01, .01)) +
  scale_fill_jcolors("pal11") +
  theme_void() + 
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"), plot.margin = margin(rep(15, 4)), 
        plot.title = element_text(size = 20, hjust = 0.5, family = "Fira Sans",margin = margin(0,0,15,0))) +
  ggtitle("Most commonly used words in Drake's studio albums") +
  guides(color = "none", fill = "none")

most_used_words_per_song

words_by_album <- studio_words |> 
  group_by(album) |> 
  dplyr::filter(word == "yeah") |> 
  summarize(n_yeah = n())

############################

# sentiments

positive <- get_sentiments("bing") |> 
  dplyr::filter(sentiment == "positive")

studio_words |> 
  #dplyr::filter(album == "Take Care") |> 
  semi_join(positive) |> 
  count(word, sort = TRUE)
  
negative <- get_sentiments("bing") |> 
  dplyr::filter(sentiment == "negative")

studio_words |> 
  semi_join(negative) |> 
  count(word, sort = TRUE)


bing <- get_sentiments("bing")

# sentiment per album
studio_words |> 
  inner_join(bing) |> 
  count(album, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  mutate(sentiment = positive - negative)

# sentiment per track and album
studio_sentiment <- studio_words |> 
  mutate(index = as.numeric(paste(track_n, line, sep=""))) |> 
  inner_join(bing) |> 
  count(album, year, track_n, sentiment) |> 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |> 
  mutate(sentiment = positive - negative)

studio_sentiment$album <- reorder(studio_sentiment$album, studio_sentiment$year)

studio_sentiment |> 
  ggplot(aes(track_n, sentiment)) +
  geom_col(show.legend = FALSE, fill = "cadetblue") +
  geom_col(data = . %>% dplyr::filter(sentiment < 0), show.legend = FALSE, fill= "firebrick" ) +
  geom_hline(yintercept = 0, color= "goldenrod") +
  facet_wrap(~ album, ncol = 2, scales = "free_x")

####
# use nrc dictionary

nrc <- get_sentiments("nrc")

nrc_albums <- studio_words |> 
  inner_join(nrc) |> 
  count(album, year, sentiment) |> 
  arrange(year, desc(n))

# sad words score per album:

nrc_albums |> 
  group_by(album) |> 
  dplyr::filter(sentiment == "sadness") |> 
  arrange(desc(n)) |> 
  ggplot(aes(n, album,fill = n)) +
  geom_col() +
  scale_fill_jcolors_contin("pal11", reverse=TRUE) +
  theme_void() +
  geom_text(aes(label = n), hjust = 1, nudge_x = -1, size = 4, fontface = "bold", family = "Fira Sans", color="grey60")  +
  scale_x_continuous(expand = c(.01, .01)) +
  theme(axis.text.y = element_text(size = 14, hjust = 1, family = "Fira Sans"), plot.margin = margin(rep(15, 4)), 
        plot.title = element_text(size = 20, hjust = 0.5, family = "Fira Sans",margin = margin(0,0,15,0))) +
  ggtitle("Number of words classified as sad per album") +
  guides(color = "none", fill = "none")
  

#################

# next:

# most positive song
# negative

# how many unique words: changes over time

# network graph for words

# d3 for something

# R markdown for all code
