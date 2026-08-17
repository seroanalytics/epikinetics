# Shared defaults for package vignettes. Individual figures should override
# dimensions only when their content requires a different aspect ratio.
options(width = 80)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "svg",
  dpi = 144,
  fig.width = 6.4,
  fig.height = 4,
  fig.align = "center",
  out.width = "88%"
)
