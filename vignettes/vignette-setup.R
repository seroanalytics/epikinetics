# Shared defaults for package vignettes. Individual figures should override
# dimensions only when their content requires a different aspect ratio.
options(width = 80)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  # Use a headless device rather than grDevices::svg(), whose Cairo backend
  # requires XQuartz on macOS. At 144 dpi, these plots remain sharp at their
  # intended HTML display size without adding an X11 dependency to builds.
  dev = "ragg_png",
  dpi = 144,
  fig.width = 6.4,
  fig.height = 4,
  fig.align = "center",
  out.width = "88%"
)
