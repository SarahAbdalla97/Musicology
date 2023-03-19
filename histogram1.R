remotes::install_github('jaburgoyne/compmus')
library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)
playlist_id <- "4aY6uMZsFEtbxJIAoJItnt"

# install and load required packages
install.packages("spotifyr")
library(spotifyr)
library(tidyverse)

# get audio features for playlist
playlist_id <- "4aY6uMZsFEtbxJIAoJItnt"
playlist_audio_features <- get_playlist_audio_features_all(playlist_id)


# create a separate histogram for each group
ggplot(playlist_audio_features, aes(x = key)) +
  geom_histogram() +
  facet_wrap(~ artist_name)


# authenticate with Spotify API
# you will need to obtain your own credentials to use this code
# see https://github.com/charlie86/spotifyr#authentication for details
spotifyr::set_credentials(client_id = "239b94521e814f92891b93465c76c8d9", client_secret = "51d5be40f19a4feb8a37f3938c118b34")

# function to get audio features for a playlist with more than 100 tracks
get_playlist_audio_features_all <- function(playlist_id) {
  audio_features_all <- list()
  offset <- 0
  limit <- 100
  while (TRUE) {
    playlist_audio_features <- get_playlist_audio_features(playlist_id, offset = offset, limit = limit, market = "US")
    audio_features_all <- c(audio_features_all, playlist_audio_features)
    offset <- offset + limit
    if (nrow(playlist_audio_features) < limit) break
  }
  do.call(rbind, audio_features_all)
}

# get audio features for playlist
playlist_id <- "4aY6uMZsFEtbxJIAoJItnt"
playlist_audio_features <- get_playlist_audio_features_all(playlist_id)

# create a separate histogram for each group
ggplot(playlist_audio_features, aes(x = key)) +
  geom_histogram() +
  facet_wrap(~ artist_name)

