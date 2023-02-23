remotes::install_github('jaburgoyne/compmus')
library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)

playlist_studymusic <- "37i9dQZF1DWZeS4XzRgJG0?si=95ccd97c08744837"
playlist_partymusic <- "37i9dQZF1DWSOb6VfKLO9H?si=452dac80d76948b1"
studymusicfeatures <- get_playlist_audio_features("", playlist_studymusic)
partymusicfeatures <- get_playlist_audio_features("", playlist_partymusic)

music <-
  bind_rows(
    studymusicfeatures |> mutate(category = "Study Music Features"),
    partymusicfeatures |> mutate(category = "Party Music Features")
  )

music <-
  get_tidy_audio_analysis("6IQILcYkN2S2eSu5IHoPEH") |>
  select(segments) |>
  unnest(segments) |>
  select(start, duration, pitches)

music |>
  mutate(pitches = map(pitches, compmus_normalise, "euclidean")) |>
  compmus_gather_chroma() |> 
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = pitch_class,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

