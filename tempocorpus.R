library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)

studymsc <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "663NfS5wple5xvRUy0aj7s?si=d3f4c71bff824310"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
partymsc <-
  get_playlist_audio_features(
    "thesoundsofspotify",
    "4aY6uMZsFEtbxJIAoJItnt?si=91c2c80ef9534759"
  ) |>
  slice(1:30) |>
  add_audio_analysis()
corpus <-
  studymsc |>
  mutate(genre = "Study Music") |>
  bind_rows(partymsc |> mutate(genre = "Party Music"))

corpus |>
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) |>
  unnest(sections) |>
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

