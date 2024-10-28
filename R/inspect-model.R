inspect_model <- function(mod, private) {

  prior_inputs <- function(name, description) {
    mu <- paste("mu", name, sep = "_")
    sigma <- paste("sigma", name, sep = "_")
    shiny::div(shiny::fluidRow(
      shiny::column(4,
                    paste(name, description, sep = ": ")),
      shiny::column(4,
                    shiny::fluidRow(class = "form-group",
                                    shiny::tags$label(paste0("mean (", mu, ")"), class = "col-sm-6 col-form-label text-right"),
                                    shiny::column(6, raw_numeric_input(mu, value = private$priors[[mu]]))
                    )
      ),
      shiny::column(4,
                    shiny::fluidRow(class = "form-group",
                                    shiny::tags$label(paste0("SD (", sigma, ")"), class = "col-sm-6 col-form-label text-right"),
                                    shiny::column(6, raw_numeric_input(sigma, value = private$priors[[sigma]])),
                    )
      ))
    )
  }

  ui <- shiny::fluidPage(style = "margin: 0.5em",
                         shiny::fluidRow(
                           shiny::column(6,
                                         shiny::h3("Prior predictive check"),
                                         plotly::plotlyOutput("prior_predicted"),
                                         shiny::tags$pre(style = "overflow: hidden; text-wrap: auto; word-break: keep-all; white-space: pre-line; margin-top: 20px;",
                                                         shiny::textOutput("prior_code", inline = TRUE)
                                         ),
                                         prior_inputs("t0", "Baseline titre value"),
                                         prior_inputs("tp", "Time to peak titre"),
                                         prior_inputs("ts", "Time to start of waning"),
                                         prior_inputs("m1", "Boosting rate"),
                                         prior_inputs("m2", "Plateau rate"),
                                         prior_inputs("m3", "Waning rate")
                           ),
                           shiny::column(6,
                                         shiny::h3("Model input data"),
                                         plotly::plotlyOutput("data"),
                                         shiny::div(style = "margin-top: 20px;",
                                                    shiny::fluidRow(class = "form-group",
                                                                    shiny::tags$label("Number of columns", class = "col-sm-3 col-form-label"),
                                                                    shiny::column(3, raw_numeric_input("ncol", value = NULL))
                                                    )
                                         )
                           )
                         ),
                         shiny::fluidRow(style = "margin-top: 20px;",
                                         shiny::column(12,
                                                       shiny::h3(shiny::textOutput("fitted"))
                                         )
                         )
  )

  server <- function(input, output, session) {
    prior <- shiny::reactive(
      biokinetics_priors(mu_t0 = input$mu_t0, mu_tp = input$mu_tp,
                         mu_ts = input$mu_ts, mu_m1 = input$mu_m1,
                         mu_m2 = input$mu_m2, mu_m3 = input$mu_m3,
                         sigma_t0 = input$sigma_t0, sigma_tp = input$sigma_tp,
                         sigma_ts = input$sigma_ts, sigma_m1 = input$sigma_m1,
                         sigma_m2 = input$sigma_m2, sigma_m3 = input$sigma_m3)
    )
    output$prior_code <- shiny::renderText({
      prior_code(input)
    })
    output$prior_predicted <- plotly::renderPlotly({
      plotly::ggplotly(plot(prior()))
    })
    output$prior_mu <- shiny::renderTable(
      as.data.frame(private$priors)[
        c("mu_t0", "mu_tp", "mu_ts", "mu_m1", "mu_m2", "mu_m3")])
    output$prior_sigma <- shiny::renderTable(
      as.data.frame(private$priors)[
        c("sigma_t0", "sigma_tp", "sigma_ts", "sigma_m1", "sigma_m2", "sigma_m3")])
    cols <- shiny::reactive({ switch(is.na(input$ncol), NULL, input$ncol) })
    plot_inputs <- shiny::reactive({
      mod$plot_model_inputs(ncol = cols()) +
        theme(plot.margin = unit(c(1, 0, 0, 0), "cm"))
    })
    output$data <- plotly::renderPlotly({
      gp <- plotly::ggplotly(plot_inputs())
      if (length(private$all_formula_vars) > 0) {
        facet_strip_bigger(gp, 30)
      } else {
        gp
      }
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

# plotly can't handle multi-line facet titles, so manually make
# the facet titles a little bigger when there are covariates
facet_strip_bigger <- function(gp, size) {

  n_facets <- c(1:length(gp[["x"]][["layout"]][["shapes"]]))

  for (i in n_facets) {
    gp[["x"]][["layout"]][["shapes"]][[i]][["y0"]] <- +as.numeric(size)
    gp[["x"]][["layout"]][["shapes"]][[i]][["y1"]] <- 0
  }

  return(gp)
}
