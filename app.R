library(shiny)
library(tsibble)
library(dplyr)
library(lubridate)
library(fable)
library(feasts)
library(ggplot2)
library(plotly)
library(tidyr)
library(readr)
library(shinycssloaders)
library(purrr)
library(stringr)
library(knitr)



# ---- Load & wrangle data (project-relative path) ----
# Assumes AustralianWines.csv with columns:
# Month, Fortified, Red, Rose, Sparkling, Sweet.white, Dry.white, Total, etc.

wines <- read_csv("AustralianWines.csv", na = "*", show_col_types = FALSE) |>
  # Fill Rose downward exactly as you had
  tidyr::fill(Rose, .direction = "down") |>
  # Convert "Jan-80" style Month to Date then to yearmonth
  mutate(
    Month_date = mdy(stringr::str_replace(Month, "-", "-01-")),
    Date       = yearmonth(Month_date)
  ) |>
  select(-Month, -Month_date) |>
  # Pivot all varietal columns long into Varietal / Value
  pivot_longer(
    cols = -Date,
    names_to  = "Varietal",
    values_to = "Value"
  ) |>
  as_tsibble(index = Date, key = Varietal)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Australian Wines — Forecasting by Varietal"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select varietal(s), training end date, and forecast horizon."),

      selectInput(
        "varietals",
        "Varietal(s):",
        choices  = unique(wines$Varietal),
        selected = unique(wines$Varietal)[1],
        multiple = TRUE
      ),

      dateRangeInput(
        "daterange", "Plot date range (visible):",
        start = min(wines$Date) |> as.Date(),
        end   = max(wines$Date) |> as.Date(),
        min   = min(wines$Date) |> as.Date(),
        max   = max(wines$Date) |> as.Date()
      ),

      sliderInput(
        "train_end",
        "Training end (month index):",
        min   = 1,
        max   = n_distinct(wines$Date) - 1,
        value = 167
      ),

      selectInput(
    "show_models", "Models to view:",
    choices  = c("TSLM", "ETS", "ARIMA"),
    selected = c("TSLM", "ETS", "ARIMA"),
    multiple = TRUE
  ),


      numericInput("h", "Forecast horizon (months):", value = 12, min = 1),

      actionButton("fit_models", "Fit models", class = "btn-primary"),

      checkboxInput("show_decomp", "Show decomposition (Overview tab)", value = FALSE),

      width = 3
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          h4("Time series overview"),
          withSpinner(plotlyOutput("overview_plot", height = "420px")),
          br(),
          h4("STL decomposition (per selected varietal)"),
            conditionalPanel(
              condition = "input.show_decomp == true",
              withSpinner(plotOutput("decomp_plot", height = "420px"))
            )
        ),

        tabPanel(
          "Parameters",
          br(),
          h4("Model specifications (MVP: ETS form & ARIMA orders)"),
          p("Select model types to view using the sidebar 'Models to view' dropdown."),
          checkboxInput("show_model_details", "Show additional details (full report / tidy)", value = FALSE),
          br(),
          # renders a compact table with ETS form / ARIMA orders (one row per fitted object)
          withSpinner(tableOutput("model_spec_table")),
          br(),
          # if user checked details, show verbose panels below (report/tidy fallback)
          conditionalPanel(
            condition = "input.show_model_details == true",
            h4("Additional model details"),
            withSpinner(uiOutput("model_params_ui"))
          )
        ),

        tabPanel(
          "Accuracy",
          br(),
          # checkbox inside the Accuracy tab to toggle train accuracy visibility
          checkboxInput("show_train_acc", "Show training (in-sample) accuracy", value = FALSE),
          p("Validation accuracy (forecast vs actual) shown below when a validation window exists."),
          br(),
          h4("Validation accuracy"),
          withSpinner(tableOutput("acc_val_only")),
          br(),
          conditionalPanel(
            condition = "input.show_train_acc == true",
            h4("Training (in-sample) accuracy"),
            withSpinner(tableOutput("acc_train_only"))
          )
        ),



        tabPanel(
          "Forecast",
          br(),
          h4("Forecast comparison (with intervals)"),
          withSpinner(plotOutput("forecast_plot", height = "600px"))
        ),
        tabPanel(
          "About",
          br(),
          h4("Forecast comparison (with intervals)"),
          withSpinner(plotOutput("feature_list", height = "600px"))
        )
      )
    )

  )
)

# ---- Server ----
server <- function(input, output, session) {

  extract_spec_from_report <- function(report_text) {
  # report_text: single string (possibly multi-line)
  # look for ETS(...) pattern
  ets_pat <- regexpr("ETS\\([^\\)]+\\)", report_text)
  arima_pat <- regexpr("ARIMA\\([^\\)]+\\)", report_text)
  ets <- if (ets_pat[1] != -1) regmatches(report_text, ets_pat) else NA_character_
  arima <- if (arima_pat[1] != -1) regmatches(report_text, arima_pat) else NA_character_
  list(ETS = ets, ARIMA = arima)
}


  # Filtered data for plotting (by varietal + date range)
  filtered <- reactive({
    req(input$varietals, input$daterange)

    start_month <- yearmonth(as.Date(input$daterange[1]))
    end_month   <- yearmonth(as.Date(input$daterange[2]))

    wines |>
      filter(
        Varietal %in% input$varietals,
        Date >= start_month,
        Date <= end_month
      )
  })

  # All data for selected varietals (full history, for modeling)
  selection_full <- reactive({
    req(input$varietals)
    wines |>
      filter(Varietal %in% input$varietals)
  })

  # Overview plot (interactive)
  output$overview_plot <- renderPlotly({
    d <- filtered() |> as_tibble()
    p <- ggplot(d, aes(x = as.Date(Date), y = Value, color = Varietal)) +
      geom_line() + geom_point(size = 0.6) +
      labs(x = "Month", y = "Value", title = "Selected varietals — series") +
      theme_minimal()
    ggplotly(p)
  })

  # Decomposition — only in Overview tab; show one varietal at a time for clarity
  output$decomp_plot <- renderPlot({
    req(input$show_decomp)         
    d <- filtered()
    req(nrow(d) > 0)

    # if multiple varietals selected, choose the first for decomposition (keeps plot readable)
    var_to_decomp <- unique(d$Varietal)[1]

    comps <- d %>%
      filter(Varietal == var_to_decomp) %>%
      model(stl = STL(Value ~ season(window = "periodic"))) %>%
      components()

    autoplot(comps) +
      ggtitle(paste("STL decomposition for", var_to_decomp)) +
      theme_minimal()
  })

  # compute training end date (from index slider)
  train_end_date <- reactive({
    all_dates <- sort(unique(selection_full()$Date))
    idx <- pmin(pmax(1, round(input$train_end)), length(all_dates) - 1)
    all_dates[idx]
  })

  train_tbl <- reactive({
    re <- selection_full()
    req(nrow(re) > 0)
    te <- train_end_date()
    re %>% filter(Date <= te)
  })

  validation_tbl <- reactive({
    re <- selection_full()
    req(nrow(re) > 0)
    te <- train_end_date()
    re %>% filter(Date > te)
  })

  # Fit models per varietal
  # ---------- REPLACE your existing fits reactive with this ----------

  arima_error_msg <- reactiveVal(NULL)

  fits <- eventReactive(input$fit_models, {
  req(nrow(train_tbl()) > 0)
  train <- train_tbl()

  # drop accidental duplicate Varietal.* columns if present (from prior joins etc.)
  train <- train %>% select(-matches("^Varietal\\.\\.\\.\\d+$"), everything())

  # Try to fit all three in one model() call. If ARIMA errors, try again without ARIMA.
  out <- tryCatch({
    train %>%
      model(
        TSLM  = TSLM(Value ~ trend() + season()),
        ETS   = ETS(Value),
        ARIMA = ARIMA(Value)
      )
  }, error = function(e1) {
    # If ARIMA failed, attempt fitting TSLM + ETS only and inform the user
    message("Primary model() failed (likely ARIMA). Retrying without ARIMA: ", e1$message)
    showNotification("ARIMA fit failed; fitting TSLM and ETS only.", type = "warning", duration = 6)
    tryCatch({
      train %>%
        model(
          TSLM = TSLM(Value ~ trend() + season()),
          ETS  = ETS(Value)
        )
    }, error = function(e2) {
      # If even this fails, return NULL and inform
      message("Fallback model() also failed: ", e2$message)
      showNotification(paste("Model fit error:", e2$message), type = "error")
      NULL
    })
  })

  out
})


  # Helper: fallback ARIMA forecasts using forecast::auto.arima (returns a tsibble-like forecast tibble)
  arima_fallback_forecast <- function(train_tsibble, h, conf_levels = c(80,95)) {
    # train_tsibble: tsibble with index Date and key Varietal, and column Value
    # returns a tibble with columns: Varietal, Date (forecast index), Point_Forecast, Lo80, Hi80, Lo95, Hi95, .model
    requireNamespace("forecast", quietly = TRUE) || stop("Install forecast package")
    out_rows <- list()

    keys <- unique(train_tsibble$Varietal)
    for (k in keys) {
      sub <- train_tsibble %>% filter(Varietal == k) %>% as_tibble()
      # convert to ts (monthly)
      start_year <- year(min(sub$Date))
      start_month <- month(min(sub$Date))
      tsdata <- ts(sub$Value, start = c(start_year, start_month), frequency = 12)
      fit <- forecast::auto.arima(tsdata)
      fc <- forecast::forecast(fit, h = h, level = c(80,95))
      fc_times <- seq(as.Date(max(sub$Date)) %m+% months(1), by = "1 month", length.out = h)
      df <- tibble(
        Varietal = k,
        Date = fc_times,
        Point_Forecast = as.numeric(fc$mean),
        Lo80 = as.numeric(fc$lower[1,]),
        Hi80 = as.numeric(fc$upper[1,]),
        Lo95 = as.numeric(fc$lower[2,]),
        Hi95 = as.numeric(fc$upper[2,]),
        .model = "ARIMA_fallback"
      )
      out_rows[[length(out_rows)+1]] <- df
    }
    bind_rows(out_rows)
  }

  fc_h <- reactive({
    req(fits())
    h <- as.integer(pmax(1, input$h))

    # Try fable forecast; if it errors (unexpected), return NULL and log
    f_h <- tryCatch({
      fits() %>% forecast(h = h)
    }, error = function(e) {
      message("fc_h forecast error: ", e$message)
      NULL
    })

    # If f_h is NULL, try fallback ARIMA per-series using forecast::auto.arima
    if (is.null(f_h)) {
      message("Using ARIMA fallback forecasts (forecast::auto.arima) for each series.")
      train_for_fallback <- selection_full() %>% group_by(Varietal) %>% index_by(Date = Date) %>% as_tsibble()
      fallback_df <- tryCatch({
        arima_fallback_forecast(train_for_fallback, h = h)
      }, error = function(e) {
        message("arima_fallback_forecast error: ", e$message)
        NULL
      })
      # mark fallback result
      attr(fallback_df, "fallback") <- TRUE
      return(fallback_df)
    }

    f_h
  })

  # ---------- Add an observer to print debug info to console when Fit models is clicked ----------
  observeEvent(input$fit_models, {
    message("---- Fit models clicked ----")
    if (is.null(selection_full())) {
      message("DEBUG: selection_full() is NULL")
    } else {
      message("DEBUG: selection_full rows: ", nrow(selection_full()))
      message("DEBUG: unique varietals selected: ", paste(unique(selection_full()$Varietal), collapse = ", ")) # nolint
      message("DEBUG: date range in selection_full: ", min(selection_full()$Date), " to ", max(selection_full()$Date)) # nolint
    }
    message("DEBUG: train_tbl rows: ", ifelse(is.null(train_tbl()), NA, nrow(train_tbl()))) # nolint: line_length_linter.
    message("----------------------------")
  })

  fc_val <- reactive({
    val <- validation_tbl()
    if (is.null(val) || nrow(val) == 0) return(NULL)
    h_val <- n_distinct(val$Date)
    req(fits())

    tryCatch({
      fits() %>% forecast(h = h_val)
    }, error = function(e) {
      message("fc_val error:", e$message)
      # try fallback per-series
      train_for_fallback <- selection_full() %>% group_by(Varietal) %>% index_by(Date = Date) %>% as_tsibble()
      tryCatch({
        arima_fallback_forecast(train_for_fallback, h = h_val)
      }, error = function(e2) {
        message("fc_val fallback also failed: ", e2$message)
        NULL
      })
    })
  })



    output$model_spec_table <- renderTable({
    if (is.null(fits())) {
      return(tibble(Note = "No fitted models yet. Click 'Fit models'."))
    }

    # convert mable to tibble
    fm_tbl <- tryCatch(as_tibble(fits()), error = function(e) {
      message("as_tibble(fits()) error: ", e$message); return(NULL)
    })
    if (is.null(fm_tbl)) return(tibble(Error = "Cannot read fitted models."))

    # candidate list-columns holding models
    candidate_cols <- names(fm_tbl)[sapply(fm_tbl, function(col) {
      is.list(col) && length(col) > 0 && ! (is.atomic(col[[1]]) && length(col[[1]]) == 1)
    })]

    if (length(candidate_cols) == 0) {
      return(tibble(Error = "No model-type columns found in fitted results."))
    }

    rows <- list()
    for (colname in candidate_cols) {
      colvals <- fm_tbl[[colname]]
      for (i in seq_along(colvals)) {
        mo <- colvals[[i]]
        if (is.null(mo)) next
        # label/key if available
        label <- names(colvals)[i]
        if (is.null(label) || label == "") label <- paste0("Model#", i)
        # capture report text safely
        rep_txt <- tryCatch(paste(capture.output(report(mo)), collapse = "\n"), error = function(e) NA_character_)
        specs <- if (!is.na(rep_txt)) extract_spec_from_report(rep_txt) else list(ETS = NA_character_, ARIMA = NA_character_)
        rows[[length(rows) + 1]] <- tibble(
          ModelColumn = colname,
          Key = label,
          ETS_Form = specs$ETS,
          ARIMA_Orders = specs$ARIMA
        )
      }
    }

    if (length(rows) == 0) {
      return(tibble(Note = "No fitted objects available for specs."))
    }

    bind_rows(rows) %>%
      # replace NA with empty string for clean display
      mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
  })

  output$model_params_ui <- renderUI({
    if (is.null(fits())) {
      return(tags$div(tags$em("No fitted models yet. Click 'Fit models' to fit models.")))
    }

    fm_tbl <- tryCatch(as_tibble(fits()), error = function(e) {
      message("as_tibble(fits()) error: ", e$message); return(NULL)
    })
    if (is.null(fm_tbl)) return(tags$div(tags$strong("Could not read fitted models.")))

    candidate_cols <- names(fm_tbl)[sapply(fm_tbl, function(col) {
      is.list(col) && length(col) > 0 && ! (is.atomic(col[[1]]) && length(col[[1]]) == 1)
    })]

    if (length(candidate_cols) == 0) {
      return(tags$div(tags$strong("No model-type list-columns found in fitted results.")))
    }

    blocks <- purrr::map(candidate_cols, function(colname) {
      colvals <- fm_tbl[[colname]]
      header_block <- tags$div(tags$h3(colname), style = "margin-top:20px;")
      obj_blocks <- purrr::imap(colvals, function(mo, idx) {
        if (is.null(mo)) return(NULL)
        label <- tryCatch({ nm <- names(colvals)[idx]; if (is.null(nm) || nm=="") nm <- paste0("Model#", idx); nm }, error = function(e) paste0("Model#", idx))

        # Try broom::tidy()
        tidy_df <- tryCatch({ if (requireNamespace("broom", quietly = TRUE)) broom::tidy(mo) else NULL }, error = function(e) NULL)
        if (!is.null(tidy_df) && (is.data.frame(tidy_df) || tibble::is_tibble(tidy_df))) {
          return(tags$div(tags$h4(label), tags$b("tidy() parameters:"), HTML(knitr::kable(tidy_df, format="html"))))
        }

        # Try broom::glance()
        glance_df <- tryCatch({ if (requireNamespace("broom", quietly = TRUE)) broom::glance(mo) %>% as.data.frame() else NULL }, error = function(e) NULL)
        if (!is.null(glance_df) && (is.data.frame(glance_df) || tibble::is_tibble(glance_df))) {
          return(tags$div(tags$h4(label), tags$b("glance() summary:"), HTML(knitr::kable(glance_df, format="html"))))
        }

        # Fallback: full report()
        report_text <- tryCatch({ paste(capture.output(report(mo)), collapse = "\n") }, error = function(e) paste0("report() error: ", e$message))
        tags$div(tags$h4(label), tags$b("report():"), tags$pre(report_text))
      })
      tags$div(header_block, obj_blocks)
    })

    do.call(tagList, blocks)
  })


  acc_train_table <- reactive({
    if (is.null(fits())) return(NULL)
    train <- train_tbl()
    req(nrow(train) > 0)

    tryCatch({
      # accuracy for models vs training data
      fabletools::accuracy(fits(), train) %>%
        select(.model, ME, RMSE, MAE, MAPE) %>%
        mutate(Window = "Train")
    }, error = function(e) {
      message("acc_train_table error: ", e$message)
      tibble(.model = NA_character_, ME = NA_real_, RMSE = NA_real_, MAE = NA_real_, MAPE = NA_real_, Window = "Train (error)")
    })
  })

  acc_val_table <- reactive({
    val <- validation_tbl()
    fval <- fc_val()
    if (is.null(val) || nrow(val) == 0 || is.null(fval)) return(NULL)

    # If fallback tibble returned, compute accuracy manually by merging forecasts & actuals
    if (!is.null(attr(fval, "fallback")) && isTRUE(attr(fval, "fallback"))) {
      # fval is a tibble with Varietal, Date, Point_Forecast, Lo80, Hi80, ...
      joined <- fval %>% left_join(as_tibble(val) %>% select(Varietal, Date, Value), by = c("Varietal", "Date"))
      # compute error metrics per model (fallback .model column)
      library(Metrics) # lightweight; optional — or compute manually
      acc <- joined %>%
        group_by(.model) %>%
        summarise(
          ME = mean(Point_Forecast - Value, na.rm = TRUE),
          RMSE = sqrt(mean((Point_Forecast - Value)^2, na.rm = TRUE)),
          MAE = mean(abs(Point_Forecast - Value), na.rm = TRUE),
          MAPE = mean(abs((Point_Forecast - Value)/Value) * 100, na.rm = TRUE)
        ) %>%
        mutate(Window = "Validation")
      return(acc)
    }

    # Normal fabletools path
    tryCatch({
      fabletools::accuracy(fval, val) %>%
        select(.model, ME, RMSE, MAE, MAPE) %>%
        mutate(Window = "Validation")
    }, error = function(e) {
      message("acc_val_table error: ", e$message)
      tibble(.model = NA_character_, ME = NA_real_, RMSE = NA_real_, MAE = NA_real_, MAPE = NA_real_, Window = "Validation (error)")
    })
  })

    # Combined accuracy output for the new Accuracy tab
  output$acc_tab <- renderTable({
  if (is.null(fits())) {
    return(tibble(Note = "No fitted models yet. Click 'Fit models' to compute accuracy."))
  }

  # Compute training accuracy only if user selected checkbox
  train_df <- NULL
  if (isTRUE(input$show_train_acc)) {
    train_df <- tryCatch(acc_train_table(), error = function(e) NULL)
  }

  # Compute validation accuracy always
  val_df <- tryCatch(acc_val_table(), error = function(e) NULL)

  if (is.null(train_df) && is.null(val_df)) {
    return(tibble(Note = "Accuracy cannot be computed. Check split or selected varietals."))
  }

  combined <- dplyr::bind_rows(
    train_df %||% tibble(),
    val_df   %||% tibble()
  )

  # 🟦 SORTING BY RMSE (lowest RMSE = best)
  if ("RMSE" %in% names(combined)) {
    combined <- combined %>% arrange(RMSE)
  }

  combined
})

  output$acc_val_only <- renderTable({
    fval <- fc_val()
    val <- validation_tbl()
    if (is.null(fval) || is.null(val) || nrow(val) == 0) {
      return(tibble(Note = "Validation window empty or forecasts unavailable."))
    }
    tryCatch({
      fabletools::accuracy(fval, val) %>%
        select(.model, ME, RMSE, MAE, MAPE) %>%
        mutate(Window = "Validation") %>%
        arrange(RMSE)
    }, error = function(e) {
      message("acc_val_only error: ", e$message)
      tibble(Error = paste("Validation accuracy error:", e$message))
    })
  })

  # Training accuracy table output (separate)
  output$acc_train_only <- renderTable({
    if (is.null(fits())) return(tibble(Note = "No fitted models yet."))

    tryCatch({
      fabletools::accuracy(fits()) %>%
        select(.model, ME, RMSE, MAE, MAPE) %>%
        mutate(Window = "Train") %>%
        arrange(RMSE)
    }, error = function(e) {
      message("acc_train_only error: ", e$message)
      tibble(Error = paste("Train accuracy error:", e$message))
    })
  })

  output$forecast_plot <- renderPlot({
    req(fc_h())
    base_data <- selection_full()

    f_h <- fc_h()

    # fallback tibble case
    if (!is.null(attr(f_h, "fallback")) && isTRUE(attr(f_h, "fallback"))) {
      # simple ggplot: historical + point forecasts + ribbons
      gp <- ggplot() +
        geom_line(data = as_tibble(base_data), aes(x = as.Date(Date), y = Value, color = Varietal)) +
        geom_point(data = f_h, aes(x = as.Date(Date), y = Point_Forecast, color = Varietal)) +
        geom_ribbon(data = f_h, aes(x = as.Date(Date), ymin = Lo95, ymax = Hi95, fill = Varietal), alpha = 0.15) +
        facet_wrap(vars(Varietal), scales = "free_y") +
        labs(title = "Forecasts (fallback ARIMA)", x = "Month", y = "Sales") +
        theme_minimal()
      print(gp)
      return()
    }

    # Normal fable forecast object case — use autoplot/autolayer
    autoplot(base_data, Value) +
      autolayer(f_h, aes(color = .model), interval = TRUE) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(title = "Forecasts (with intervals)", x = "Month", y = "Sales") +
      theme_minimal()
  })




  # Features list
  output$feature_list <- renderUI({
    tags$ul(
      tags$li("Models: TSLM (trend + season), ETS (automatic), ARIMA (automatic)."),
      tags$li("Train/validation split controlled by slider (index-based) for reproducible splits."),
      tags$li("Forecast horizon control (months)."),
      tags$li("Forecast plots show prediction intervals and legends; facets when multiple varietals are selected."),
      tags$li("Accuracy table with separate Train vs Validation rows (RMSE, MAE, MAPE)."),
      tags$li("No hard-coded file paths; project-relative `AustralianWines.csv`."),
      tags$li("About tab summarizing data source and modeling choices.")
    )
  })
}

shinyApp(ui = ui, server = server)
