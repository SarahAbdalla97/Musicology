music <-
  get_tidy_audio_analysis("4TsmezEQVSZNNPv5RJ65Ov?si=468bdbdd8e9143a1") |>
  compmus_align(bars, segments) |>
  select(bars) |>
  unnest(bars) |>
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "acentre", norm = "manhattan"
      )
  ) |>
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "mean"
      )
  )
bind_rows(
  music |> 
    compmus_self_similarity(pitches, "aitchison") |> 
    mutate(d = d / max(d), type = "Chroma"),
  music |> 
    compmus_self_similarity(timbre, "euclidean") |> 
    mutate(d = d / max(d), type = "Timbre")
) |>
  mutate() |> 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) + ggtitle("Chroma vs timber based matrices for 'Pon de Replay - Rihanna'") + 
  geom_tile() +
  coord_fixed() +
  facet_wrap(~type) +
  scale_fill_viridis_c(option = "E", guide = "none") +
  theme_classic() + 
  labs(x = "", y = "")

