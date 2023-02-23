library(tidyverse)
library(spotifyr)
library(dplyr)
library(plotly)

playlist_studymusic <- "37i9dQZF1DWZeS4XzRgJG0?si=95ccd97c08744837"
playlist_partymusic <- "37i9dQZF1DWSOb6VfKLO9H?si=452dac80d76948b1"
studymusicfeatures <- get_playlist_audio_features("", playlist_studymusic)
partymusicfeatures <- get_playlist_audio_features("", playlist_partymusic)

music <-
  bind_rows(
    studymusicfeatures |> mutate(category = "Study Music Features"),
    partymusicfeatures |> mutate(category = "Party Music Features")
  )

music |>
  ggplot(aes(x = energy)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~category)


