# changes to genius package's function to update to current genius website


genius_lyrics_mod <- function (artist = NULL, song = NULL, info = "title") 
{
  song_url <- gen_song_url(artist, song)
  genius_url_mod(song_url, info)
}

cleaning <- function() {
  # putting randomblackdude in here because I can't figure out a regex for him and he's throwing me off
  clean_vec <- c("([^RandomBlackDude][a-z0-9]{2,})([[:upper:]])" = "\\1\n\\2", # turn camel case into new lines
                 "(\\]|\\))([[:upper:]])" = "\\1\n\\2", # letters immediately after closing brackets new lines
                 # brackets with producer info into new lines
                 "(\\[.{2,100}\\])" ="\n\\1\n",
                 # rip smart quotes
                 "\u2019" = "'",
                 # if quotes follow or precede brackets fix lines
                 "(\\])(\")" = "\\1\n\\2",
                 "(\")(\\[)" = "\\1\n\\2",
                 # if a question mark directly touches a word or number make new lines
                 "(\\?)([[:alpha:]])" = "\\1\n\\2",
                 # roger waters, you're a pain: comfortably numb, issue # 4
                 # https://github.com/JosiahParry/genius/issues/4
                 "(\\])(\\[)" = "\\1\n\\2")
  
  return(clean_vec)
}


genius_url_mod <- function (data){
  
  url <- data$track_url
  album <- data$album_name
  song <- data$track_title
  track_n <- data$track_n
  
  genius_session <- session(url)

  lyrics <-  genius_session %>% html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]') %>%
    html_text(trim = TRUE) %>% str_replace_all(cleaning()) %>% 
    strsplit(split = "\n") %>%  unlist() %>% .[str_detect(., "[[:alnum:]]")] %>% str_trim() %>%
    tibble(lyric = .)  %>%
    mutate(artist = "Drake", track_title = song, album = album, track_n = track_n) %>%
    mutate(line = row_number()) %>%
    bind_rows(tibble(lyric = c("", "[]"))) %>%
    mutate(type = case_when(str_detect(lyric, "\\[|\\]") ~ "meta", TRUE ~ "lyric")) %>%
    pivot_wider(names_from = type, values_from = lyric) %>%
    dplyr::filter(!is.na(line)) %>%
    fill(meta, .direction = "down") %>%
    mutate(meta = str_extract(meta,"[^\\[].*[^\\]]")) %>%
    separate(meta, into = c("element", "element_artist"), sep = ": ", fill = "right") %>%
    mutate(element_artist = replace_na(element_artist,  artist[1])) %>%
    group_by(element) %>%
    dplyr::filter(ifelse(is.na(lyric) & n() > 1, FALSE, TRUE)) %>%
    ungroup() %>%
    mutate(line = row_number()) |> 
    dplyr::filter(!is.na(lyric))
  
  lyrics
}

add_genius_mod <- function (data, artist, title, type = c("album", "track", "lyrics")) 
{
  genius_funcs <- list(album = possible_album, lyrics = possible_lyrics)
  artist <- enquo(artist)
  title <- enquo(title)
  type <- enquo(type)
  songs <- dplyr::filter(data, !!type %in% c("lyrics", "track"))
  albums <- dplyr::filter(data, !!type == "album")
  song_lyrics <- mutate(songs, lyrics = map2(.x = !!artist, 
                                             .y = !!title, genius_funcs[["lyrics"]]))
  album_lyrics <- mutate(albums, lyrics = map2(.x = !!artist, 
                                               .y = !!title, genius_funcs[["album"]]))
  bind_rows(album_lyrics %>% unnest(lyrics), song_lyrics %>% 
              unnest(lyrics)) %>% inner_join(data) %>% as_tibble()
}

genius_album_mod <- function (artist, album_name)                   # all new
{
  tracks <- genius_tracklist(artist, album_name) |> pull(track_url)
  lyrics <- lapply(tracks, function(x) genius_url_mod(x))
  album <- lyrics |> 
    mutate(album = album_name)
  album
}
