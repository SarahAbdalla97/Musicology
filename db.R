library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)
library(plotly)

partymusc <- get_playlist_audio_features("", "4aY6uMZsFEtbxJIAoJItnt?si=1eeb0bcc4af247ad")
studymusc <- get_playlist_audio_features("", "663NfS5wple5xvRUy0aj7s?si=a4503edff29941e1")
corpus <-
  partymusc |>
  mutate(country = "Party Music") |>
  bind_rows(studymusc |> mutate(country = "Study Music")) |>
  mutate(
    country = fct_relevel(country, "Study Music", "Party Music")
  )
plot <-
  corpus |>
  ggplot(                          # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = track.popularity,
      colour = danceability,
      label = track.name           # Labels will be interactively visible.
    )
  ) +
  geom_point() +                   # Scatter plot.
  geom_rug(size = 0.1) +           # Add 'fringes' to show data distribution.
  facet_wrap(~country) +           # Separate charts per country.
  scale_x_continuous(              # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),        # Use grid-lines for quadrants only.
    minor_breaks = NULL            # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(              # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_viridis_c(          # Use the cividis palette
    option = "E",                  # Qualitative set.
    alpha = 0.8,                   # Include some transparency
    guide = "none"
  ) +
  scale_size_continuous(           # Fine-tune the sizes of each point.
    guide = "none"                 # Remove the legend for size.
  ) +
  theme_light() +                  # Use a simpler theme.
  labs(                            # Make the titles nice.
    x = "Valence",
    y = "Energy"
  )
ggplotly(plot)

