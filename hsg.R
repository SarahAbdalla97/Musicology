remotes::install_github('jaburgoyne/compmus')
library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)
library(plotly)
colors <- c("#332288", "#AA4499")

KeysRatio <- music %>%
  group_by(category, key_mode) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()
KeysPlotRatio <- ggplot(KeysRatio, aes(key_mode, freq, fill = category, text = paste("Artist: ", category, "<br>",
                                                                                     "Key: ", key_mode, "<br>",
                                                                                     "Percentage: ", freq, "<br>",
                                                                                     "Actual frequency:", n))) +
  geom_col(position = position_dodge(preserve = "single", width = 0.8), alpha = 0.8, width = 1) +
  scale_fill_manual(values=colors) +
  labs(title = "Keys for Party Music and Study Music features", x = "Key", y = "Frequency", fill = "Artist", fontface = "bold", angle = 90, size = 2) +
  theme_bw() +
  theme(legend.position = c(0.76, 0.23), axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

partymusc <-
  get_playlist_audio_features(
    "",
    "7eYTx9OtJPrKBst4bWr3PD"
  ) |>
  add_audio_analysis()
studymusc <-
  get_playlist_audio_features(
    "",
    "2Oou3UXp7fHZsHHxQ9jQnC"
  ) |>
  add_audio_analysis()
comp2 <-
  bind_rows(studymusc |> mutate(category = "Study Music"), partymusc |> mutate(category = "Party Music"))
KeysPlot <- ggplot(KeysRatio, aes(key_mode, n, fill = category, text = paste("Artist: ", category, "<br>",
                                                                             "Key: ", key_mode, "<br>",
                                                                             "Actual frequency:", n))) +
  geom_col(width = 1) +
  scale_fill_manual(values=colors) +
  labs(title = "Keys for Party Music and Study Music features", x = "Key", y = "Frequency", fill = "Artist", fontface = "bold", angle = 90, size = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x = element_blank(), axis.ticks.y = element_blank(), strip.background = element_rect(fill="#000")) +
  facet_wrap(~category)
KeysPlotlyRatio <- ggplotly(KeysPlotRatio, tooltip = "text")
KeysPlotlyRatio
NTracks <- music %>% group_by(category) %>% summarise(Tracks = n()) %>% rename(Artist = category)

