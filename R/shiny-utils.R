raw_numeric_input <- function(inputId, value, min = NA, max = NA, step = NA) {
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- shiny::tags$input(id = inputId, type = "number", class = "shiny-input-number form-control", value = shiny:::formatNoSci(value))
  if (!is.na(min)) inputTag$attribs$min <- min
  if (!is.na(max)) inputTag$attribs$max <- max
  if (!is.na(step)) inputTag$attribs$step <- step
  inputTag
}

prior_code <- function(input) {
  deparse(substitute(biokinetics_priors(mu_t0 = a, mu_tp = b,
                                mu_ts = c, mu_m1 = d,
                                mu_m2 = e, mu_m3 = f,
                                sigma_t0 = g, sigma_tp = h,
                                sigma_ts = i, sigma_m1 = j,
                                sigma_m2 = k, sigma_m3 = l),
             list(a = input$mu_t0, b = input$mu_tp,
                  c = input$mu_ts, d = input$mu_m1,
                  e = input$mu_m2, f = input$mu_m3,
                  g = input$sigma_t0, h = input$sigma_tp,
                  i = input$sigma_ts, j = input$sigma_m1,
                  k = input$sigma_m2, l = input$sigma_m3)), width.cutoff = 500L)
}
