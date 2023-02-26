remotes::install_github('jaburgoyne/compmus')
library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)

music <-
  get_tidy_audio_analysis("4ZLzoOkj0MPWrTLvooIuaa?si=100d76873adf4534") |>
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
  ) + ggtitle("Chromagram for party music outlier") + 
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  theme_minimal() +
  scale_fill_viridis_c()

