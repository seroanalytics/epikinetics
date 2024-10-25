inspect_model <- function(mod, private) {

  prior_inputs <- function(name, description) {
    mu <- paste("mu", name, sep = "_")
    sigma <- paste("sigma", name, sep = "_")
    div(fluidRow(column(12, shiny::h4(paste(name, description, sep = ": ")))),
       fluidRow(
            column(6,
                   fluidRow(
                     shiny::tags$label(paste0("mean (", mu, ")"), class = "col-sm-4 col-form-label"),
                     column(6, raw_numeric_input(mu, value = private$priors[[mu]]))
                   )
            ),
            column(6,
                   fluidRow(
                     shiny::tags$label(paste0("std (", sigma, ")"), class = "col-sm-4 col-form-label"),
                     column(6, raw_numeric_input(sigma, value = private$priors[[sigma]])),
                   )
            )
        ))
  }

  ui <- shiny::fluidPage(style = "margin: 0.5em",
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h3("Prior predictive checks"),
                                         plotly::plotlyOutput("prior_predicted"),
                                         prior_inputs("t0", "Baseline titre value"),
                                         prior_inputs("tp", "Time to peak titre"),
                                         prior_inputs("ts", "Time to start of waning"),
                                         prior_inputs("m1", "Boosting rate"),
                                         prior_inputs("m2", "Plateue rate"),
                                         prior_inputs("m3", "Waning rate")
                           ),
                           shiny::column(6,
                                         shiny::h3("Model input data"),
                                         plotly::plotlyOutput("data"),
                                         shiny::numericInput("ncol", "Number of columns", 2)
                           )
                         ),
                         shiny::fluidRow(
                           column(12,
                                  shiny::h3(shiny::textOutput("fitted"))
                           )
                         )
  )

  server <- function(input, output, session) {
    output$prior_predicted <- plotly::renderPlotly({
      plotly::ggplotly(mod$plot_prior_predictive())
    })
    output$prior_mu <- shiny::renderTable(
      as.data.frame(private$priors)[
        c("mu_t0", "mu_tp", "mu_ts", "mu_m1", "mu_m2", "mu_m3")])
    output$prior_sigma <- shiny::renderTable(
      as.data.frame(private$priors)[
        c("sigma_t0", "sigma_tp", "sigma_ts", "sigma_m1", "sigma_m2", "sigma_m3")])
    cols <- shiny::reactive({ input$ncol })
    plot_inputs <- shiny::reactive({
      mod$plot_model_inputs(ncol = cols()) +
        theme(plot.margin = unit(c(1, 0, 0, 0), "cm"))
    })
    output$data <- plotly::renderPlotly({
      gp <- plotly::ggplotly(plot_inputs())
      facet_strip_bigger(gp, 30)
    })
    output$fitted <- shiny::renderText({
      if (is.null(private$fitted)) {
        "Model has not been fitted yet. Once fitted, inspect the model again to see posterior predictions."
      }
    })
  }

  logger::log_info(
    "Starting Shiny app for model review; use Ctrl + C to quit"
  )
  shiny::runApp(
    shiny::shinyApp(ui, server),
    quiet = TRUE,
    launch.browser = shiny::paneViewer()
  )
  invisible()
}

facet_strip_bigger <- function(gp, size) {

  n_facets <- c(1:length(gp[["x"]][["layout"]][["shapes"]]))

  for (i in n_facets) {
    gp[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- +as.numeric(size)
    gp[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
  }

  return(gp)
}
