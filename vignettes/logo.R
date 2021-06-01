# Make RFA package logo

library(hexSticker)
library(UCSCXenaTools)
library(tidyverse)

dt <-
  tibble(
    x = rnorm(100),
    y = x + rnorm(100)
  )
p <-
  ggplot(dt) +
  aes(x, y) +
  geom_point(
    col = "lightgrey"
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_transparent() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

sticker(
  p,
  package = "RFA",
  p_size = 42,
  s_x = 0.9,
  s_y=1,
  s_width=1.7,
  s_height = 1.3,
  p_x = 1,
  p_y = 1,
  p_color = "darkblue",
  h_fill = "darkorange",
  h_color = "darkblue",
  url = "https://github.com/milesdwilliams15/RFA",
  filename = "inst/logo.png"
)
