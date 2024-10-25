raw_numeric_input <- function(inputId, value, min = NA, max = NA, step = NA) {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "number", class = "shiny-input-number form-control", value = shiny:::formatNoSci(value))
  if (!is.na(min)) inputTag$attribs$min = min
  if (!is.na(max)) inputTag$attribs$max = max
  if (!is.na(step)) inputTag$attribs$step = step
  inputTag
}
