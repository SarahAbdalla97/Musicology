library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)
library(plotly)

options(scipen=999)
options(digits = 3)
# my playlists
partymusc <- get_playlist_audio_features('', '4aY6uMZsFEtbxJIAoJItnt?si=1eeb0bcc4af247ad') %>%
  mutate(
    track.album.release_date =
      map_int(
        track.album.release_date,
        \(x) as.integer(str_sub(x, start = 1, end = 4))
      )
  )
studymusc <- get_playlist_audio_features('', '663NfS5wple5xvRUy0aj7s?si=a4503edff29941e1') %>%
  mutate(
    track.album.release_date =
      map_int(
        track.album.release_date,
        \(x) as.integer(str_sub(x, start = 1, end = 4))
      )
  )

Party <- partymusc %>% summarise(
  mean_speechiness = mean(speechiness),
  mean_acousticness = mean(acousticness),
  mean_liveness = mean(liveness),
  mean_mode = mean(mode),
  mean_danceability = mean(danceability),
  mean_energy = mean(energy),
  mean_key = mean(key),
  mean_valence = mean(valence),
  mean_tempo = mean(tempo),
  mean_pop = mean(track.popularity),
  sd_speechiness = sd(speechiness),
  sd_acousticness = sd(acousticness),
  sd_liveness = sd(liveness),
  sd_mode = sd(mode),
  sd_danceability = sd(danceability),
  sd_energy = sd(energy),
  sd_key = sd(key),
  sd_valence = sd(valence),
  sd_tempo = sd(tempo),
  sd_pop = sd(track.popularity),
  median_speechiness = median(speechiness),
  median_acousticness = median(acousticness),
  median_liveness = median(liveness),
  median_mode = median(mode),
  median_danceability = median(danceability),
  median_energy = median(energy),
  median_key = median(key),
  median_valence = median(valence),
  median_tempo = median(tempo),
  median_pop = median(track.popularity),
  mad_speechiness = mad(speechiness),
  mad_acousticness = mad(acousticness),
  mad_liveness = mad(liveness),
  mad_mode = mad(mode),
  mad_danceability = mad(danceability),
  mad_energy = mad(energy),
  mad_key = mad(key),
  mad_valence = mad(valence),
  mad_tempo = mad(tempo),
  mad_pop = mad(track.popularity)
)
Study <- studymusc %>% summarise(
  mean_speechiness = mean(speechiness),
  mean_acousticness = mean(acousticness),
  mean_liveness = mean(liveness),
  mean_mode = mean(mode),
  mean_danceability = mean(danceability),
  mean_energy = mean(energy),
  mean_key = mean(key),
  mean_valence = mean(valence),
  mean_tempo = mean(tempo),
  mean_pop = mean(track.popularity),
  sd_speechiness = sd(speechiness),
  sd_acousticness = sd(acousticness),
  sd_liveness = sd(liveness),
  sd_mode = sd(mode),
  sd_danceability = sd(danceability),
  sd_energy = sd(energy),
  sd_key = sd(key),
  sd_valence = sd(valence),
  sd_tempo = sd(tempo),
  sd_pop = sd(track.popularity),
  median_speechiness = median(speechiness),
  median_acousticness = median(acousticness),
  median_liveness = median(liveness),
  median_mode = median(mode),
  median_danceability = median(danceability),
  median_energy = median(energy),
  median_key = median(key),
  median_valence = median(valence),
  median_tempo = median(tempo),
  median_pop = median(track.popularity),
  mad_speechiness = mad(speechiness),
  mad_acousticness = mad(acousticness),
  mad_liveness = mad(liveness),
  mad_mode = mad(mode),
  mad_danceability = mad(danceability),
  mad_energy = mad(energy),
  mad_key = mad(key),
  mad_valence = mad(valence),
  mad_tempo = mad(tempo),
  mad_pop = mad(track.popularity)
)
Comparison1 <-
  bind_rows(
    partymusc %>% mutate(category = "Party Music"),
    studymusc %>% mutate(category = "Study Music"),
  )
# creating a color blind friendly color palette
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                         "#CC79A7","#000000")
                         gray <- gray.colors(5, start = 0, end = 1, gamma = 2.2, rev = TRUE)
                         safe_palette <- c("#007B71", "#B6231B", "#977400", "#a9dea9", "#332288", "#AA4499", 
                                                    "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
                                                    palette2 <- c("#B6231B", "#977400", "#a9dea9", "#332288", "#AA4499", 
                                                                           "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
                                                                           
                                                    plot1 <- ggplot(Comparison1, aes(x = tempo, y = danceability, fill = category, color = as.factor(time_signature))) +
                                                      geom_point(shape = 21, size = 3.5, stroke = 1) +
                                                      labs(y = 'Danceability', x = 'Tempo', fill = 'Artist', color = 'Beats per Measure') +
                                                      scale_fill_manual(values=alpha((safe_palette), 0.6)) +
                                                      scale_color_manual(values=gray) +
                                                      scale_y_continuous(limits = c(0,1)) +
                                                      geom_label(label="Yeah - Queen", x=25, y=0.005, label.padding = unit(0.20, "lines"), color = "black", fill = "white") +
                                                      theme_minimal() +
                                                      theme(legend.position = c(0.15, 0.6), axis.text.y = element_blank())                                                                                                                      "Artist: ", category, "<br>",
                                                    "Popularity: ", popularity, "<br>",
                                                    "Danceability: ", danceability, "<br>",
                                                    "Valence: ", valence, "<br>",
                                                    "Instrumentalness: ", instrumentalness))) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c() +
  labs(title = "The Danceability of Songs by Queen and Members of the Band", x = "Danceability", y = "Track Popularity", size = "Instrumentalness", color = "Valence") +
  facet_wrap(~category) +
  theme_bw() +
  theme(strip.background = element_rect(fill="#CC79A7"))
DanceabilityPlotly <- ggplotly(plot3, tooltip = "text")
DanceabilityPlotly


