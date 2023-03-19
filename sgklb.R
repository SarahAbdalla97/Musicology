library(tidyverse)
library(spotifyr)
library(dplyr)
library(ggplot2)
library(compmus)
library(plotly)
tempo <- get_tidy_audio_analysis("1almCHdsfikRPfVB9VrEdT?si=df354dad1e3d4e12")
tempo |>
    tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) |>
    ggplot(aes(x = time, y = bpm, fill = power)) + ggtitle("Tempogram for 'Little Do You Know' by 'Alex & Sierra'") +
    geom_raster() +
    scale_fill_viridis_c(guide = "none") +
    labs(x = "Time (s)", y = "Tempo (BPM)")
    theme_classic()

    