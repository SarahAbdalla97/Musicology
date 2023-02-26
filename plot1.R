library(tidyverse)
library(spotifyr)
library(dplyr)
library(plotly)

playlist_studymusic <- "663NfS5wple5xvRUy0aj7s?si=d0a2d31141e64d1f"
playlist_partymusic <- "4aY6uMZsFEtbxJIAoJItnt?si=1b003f9614044890"
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
  facet_wrap(~category) + 
  ggtitle("Energy against count histogram for study and party music")


