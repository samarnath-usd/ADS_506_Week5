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
          h4("Model specifications & parameters (preview)"),
          withSpinner(uiOutput("model_params_ui")),
        ),
        tabPanel(
          "Accuracy",
          br(),
          # checkbox inside the Accuracy tab to toggle train accuracy
          checkboxInput("show_train_acc", "Show training (in-sample) accuracy", value = FALSE),

          # explanation
          p("Validation accuracy (forecast vs actual) is always shown when a validation window exists."),
          br(),

          # accuracy table (reactive rendering handled in server)
          withSpinner(tableOutput("acc_tab"))
        ),


        tabPanel(
          "Forecast",
          br(),
          h4("Forecast comparison (with intervals)"),
          withSpinner(plotOutput("forecast_plot", height = "600px"))
        )
      )
    )

  )
)

# ---- Server ----
server <- function(input, output, session) {

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

  fits <- eventReactive(input$fit_models, {
    # Defensive checks
    if (is.null(train_tbl()) || nrow(train_tbl()) == 0) {
      showNotification("No training data. Adjust 'train_end' slider or varietal selection before fitting.", type = "error", duration = 6)
      # print debug info to console for troubleshooting
      message("DEBUG: train_tbl() is empty. train_tbl() rows = ", ifelse(is.null(train_tbl()), NA, nrow(train_tbl())))
      if (!is.null(selection_full())) {
        message("DEBUG: selection_full() rows = ", nrow(selection_full()))
        message("DEBUG: available dates (min,max) = ", min(selection_full()$Date), ", ", max(selection_full()$Date))
        message("DEBUG: selected varietals = ", paste(unique(selection_full()$Varietal), collapse = ", "))
      }
      return(NULL)
    }

    # If we reach here we have training data; fit models with progress notification
    withProgress(message = "Fitting models...", value = 0, {
      incProgress(0.1)
      res <- tryCatch({
        train_tbl() %>%
          model(
            TSLM  = TSLM(Value ~ trend() + season()),
            ETS   = ETS(Value),
            ARIMA = ARIMA(Value)
          )
      }, error = function(e) {
        showNotification(paste0("Model fitting failed: ", e$message), type = "error", duration = 8)
        message("Model fit error: ", e$message)
        return(NULL)
      })
      incProgress(0.9)
      res
    })
  }, ignoreNULL = FALSE)   # keep ignoreNULL FALSE if you want initial evaluation; change to TRUE if you want only on click

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


  # Forecasts for user-defined horizon
  fc_h <- reactive({
        req(fits())
        fits() |>
          forecast(h = input$h)
      })

      # ---- Safe fc_val (validation forecasts) ----
    fc_val <- reactive({
      val <- validation_tbl()
      if (is.null(val) || nrow(val) == 0) return(NULL)
      h_val <- n_distinct(val$Date)
      req(fits())

      tryCatch({
        fits() %>% forecast(h = h_val)
      }, error = function(e) {
        message("fc_val error: ", e$message)
        NULL
      })
    })
      
  output$model_params_ui <- renderUI({
  if (is.null(fits())) {
    return(tags$div(tags$em("No fitted models yet. Click 'Fit models' to fit models.")))
  }

  fm <- fits()
  fm_tbl <- tryCatch(as_tibble(fm), error = function(e) {
    message("as_tibble(fits()) error: ", e$message); return(NULL)
  })
  if (is.null(fm_tbl)) return(tags$div(tags$strong("Could not read fitted models.")))

  # Detect candidate model-type list-columns in the mable
  candidate_cols <- names(fm_tbl)[sapply(fm_tbl, function(col) {
    is.list(col) && length(col) > 0 && ! (is.atomic(col[[1]]) && length(col[[1]]) == 1)
  })]

  # Respect the user selection (show_models). If input missing, default to all candidate_cols
  selected_models <- tryCatch({
    if (is.null(input$show_models)) candidate_cols else intersect(candidate_cols, input$show_models)
  }, error = function(e) candidate_cols)

  if (length(selected_models) == 0) {
    return(tags$div(tags$strong("No model types selected OR no matching fitted model columns found.")))
  }

  # Helper to render a tibble as HTML table
  htmlEscape <- function(x) {
    x <- as.character(x)
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
    x
  }
  render_table_html <- function(df) {
    tryCatch({
      html <- knitr::kable(df, format = "html", table.attr = "class='table table-condensed'")
      HTML(html)
    }, error = function(e) {
      HTML(paste0("<pre>Could not render table: ", htmlEscape(e$message), "</pre>"))
    })
  }

  blocks <- purrr::map(selected_models, function(colname) {
    colvals <- fm_tbl[[colname]]

    header_block <- tags$div(tags$h3(colname), style = "margin-top:20px;")

    # for each fitted object in this model column (usually one per key/varietal)
    obj_blocks <- purrr::imap(colvals, function(mo, idx) {
      if (is.null(mo)) return(NULL)

      # Determine a label for this fitted object: use names if available else index
      label <- tryCatch({
        nm <- names(colvals)[idx]
        if (is.null(nm) || nchar(nm) == 0) nm <- paste0("Model #", idx)
        nm
      }, error = function(e) paste0("Model #", idx))

      # Try broom::tidy()
      tidy_df <- tryCatch({
        if (requireNamespace("broom", quietly = TRUE)) broom::tidy(mo) else NULL
      }, error = function(e) NULL)

      if (!is.null(tidy_df) && (is.data.frame(tidy_df) || tibble::is_tibble(tidy_df))) {
        tb_html <- render_table_html(tidy_df)
        return(tags$div(
          tags$h4(label),
          tags$b("tidy() parameters:"),
          tb_html
        ))
      }

      # Try broom::glance()
      glance_df <- tryCatch({
        if (requireNamespace("broom", quietly = TRUE)) broom::glance(mo) %>% as.data.frame() else NULL
      }, error = function(e) NULL)

      if (!is.null(glance_df) && (is.data.frame(glance_df) || tibble::is_tibble(glance_df))) {
        tb_html <- render_table_html(glance_df)
        return(tags$div(
          tags$h4(label),
          tags$b("glance() summary:"),
          tb_html
        ))
      }

      # Fallback: full report() text
      report_text <- tryCatch({
        paste(capture.output(report(mo)), collapse = "\n")
      }, error = function(e) {
        paste0("report() error: ", e$message)
      })

      tags$div(
        tags$h4(label),
        tags$b("report():"),
        tags$pre(report_text)
      )
    })

    tags$div(header_block, obj_blocks)
  })

  do.call(tagList, blocks)
})



  # ---- Accuracy: train & validation ----
  acc_train_table <- reactive({
    if (is.null(fits())) return(NULL)

    tryCatch({
      # training / in-sample accuracy for mable
      fabletools::accuracy(fits()) %>%
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

    tryCatch({
      # correct pattern: accuracy(forecast_fable, actuals_tsibble)
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




  # Forecast plot: history + forecasts + intervals
  output$forecast_plot <- renderPlot({
    req(fc_h())
    base_data <- selection_full()

    autoplot(base_data, Value) +
      autolayer(fc_h(), aes(color = .model), interval = TRUE) +
      facet_wrap(vars(Varietal), scales = "free_y") +
      labs(
        title = "Forecasts (with 80/95% intervals)",
        x = "Month",
        y = "Sales"
      ) +
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
