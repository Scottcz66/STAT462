library(shiny)
library(vcd)
library(GGally)
library(corrgram)
library(visdat)
library(DT)
library(car)
library(ggplot2)
library(plotly)
library(tabplot)

server <- function(input, output, session){
  
  
  
  # -----------------------------
  # Global filtered data by date
  # -----------------------------
  filtered_data <- reactive({
    d <- data
    req(input$date_range)
    d <- d[d$Date >= input$date_range[1] & d$Date <= input$date_range[2], , drop = FALSE]
    d
  })
  tab_ids <- c("miss", "tabp", "mosaic", "ggpairs", "corr", "dist", "box", "rise", "ts", "dt")
  
  lapply(tab_ids, function(id) {
    observeEvent(input[[paste0(id, "_toggle")]], {
      shinyjs::runjs(
        sprintf(
          "$('#%s_wrap').toggleClass('collapsed');",
          id
        )
      )
    })
  })
  
  observeEvent(input$miss_vars, {
    if (length(input$miss_vars) > 10) {
      updateCheckboxGroupInput(
        session,
        "miss_vars",
        selected = input$miss_vars[1:10]
      )
      showNotification("You can select up to 10 variables only.", type = "warning")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$tab_vars, {
    if (length(input$tab_vars) > 10) {
      updateCheckboxGroupInput(
        session,
        "tab_vars",
        selected = input$tab_vars[1:10]
      )
      showNotification("You can select up to 10 variables only.", type = "warning")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$VarsToShow, {
    if (length(input$VarsToShow) > 10) {
      updateCheckboxGroupInput(
        session,
        "VarsToShow",
        selected = input$VarsToShow[1:10]
      )
      showNotification("You can select up to 10 variables only.", type = "warning")
    }
  }, ignoreInit = TRUE)
  
  
  # -----------------------------
  # Overview
  # -----------------------------
  output$box_nrow <- renderUI({
    div(class = "summary-box",
        h4("Rows"),
        div(class = "big", nrow(filtered_data())))
  })
  
  output$box_ncol <- renderUI({
    div(class = "summary-box",
        h4("Columns"),
        div(class = "big", ncol(filtered_data())))
  })
  
  output$box_missing <- renderUI({
    d <- filtered_data()
    miss_pct <- round(mean(is.na(d)) * 100, 2)
    div(class = "summary-box",
        h4("Missing %"),
        div(class = "big", paste0(miss_pct, "%")))
  })
  
  output$box_datespan <- renderUI({
    d <- filtered_data()
    span_days <- as.numeric(max(d$Date, na.rm = TRUE) - min(d$Date, na.rm = TRUE)) + 1
    div(class = "summary-box",
        h4("Days covered"),
        div(class = "big", span_days))
  })
  
  output$tbl_preview <- renderDT({
    d <- filtered_data()
    
    preview_cols <- intersect(
      c("Y", "ID", "Operator", "Date", "Priority", "Price", "Speed",
        "Duration", "Temp", "Location", "Agreed", "State"),
      names(d)
    )
    
    datatable(
      head(d[, preview_cols, drop = FALSE], 8),
      options = list(
        pageLength = 8,
        scrollX = TRUE,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE
      )
    )
  })
  
  output$type_summary_dt <- renderDT({
    d <- filtered_data()
    
    type_df <- data.frame(
      Type = c("Numeric", "Factor / Character", "Date"),
      Count = c(
        sum(sapply(d, is.numeric)),
        sum(sapply(d, function(x) is.factor(x) || is.character(x))),
        sum(sapply(d, inherits, what = "Date"))
      )
    )
    
    datatable(
      type_df,
      rownames = FALSE,
      options = list(dom = "t")
    )
  })
  
  output$missing_summary_tbl <- renderDT({
    d <- filtered_data()
    miss_pct <- round(colMeans(is.na(d)) * 100, 2)
    miss_tbl <- data.frame(
      Variable = names(miss_pct),
      MissingPercent = miss_pct
    )
    miss_tbl <- miss_tbl[order(-miss_tbl$MissingPercent), ]
    datatable(miss_tbl, options = list(pageLength = 8, dom = "tip"))
  })
  
  output$overall_missing_plot <- renderPlot({
    d <- filtered_data()
    miss_pct <- sort(colMeans(is.na(d)) * 100, decreasing = TRUE)
    miss_pct <- head(miss_pct, 10)
    
    miss_df <- data.frame(
      Variable = factor(names(miss_pct), levels = rev(names(miss_pct))),
      MissingPercent = as.numeric(miss_pct)
    )
    
    ggplot(miss_df, aes(x = Variable, y = MissingPercent)) +
      geom_col(fill = "#9fd3f5") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      labs(
        title = "Top 10 variables by missing percentage",
        x = NULL,
        y = "Missing (%)"
      )
  }, res = 140)
  
  output$date_summary_tbl <- renderDT({
    d <- filtered_data()
    
    date_tbl <- data.frame(
      Metric = c("Start date", "End date", "Days covered", "Observations"),
      Value = c(
        as.character(min(d$Date, na.rm = TRUE)),
        as.character(max(d$Date, na.rm = TRUE)),
        as.numeric(max(d$Date, na.rm = TRUE) - min(d$Date, na.rm = TRUE)) + 1,
        nrow(d)
      )
    )
    
    datatable(
      date_tbl,
      rownames = FALSE,
      options = list(dom = "t")
    )
  })
  
  # -----------------------------
  # Mosaic
  # -----------------------------
  output$mosaic_plot <- renderPlot({
    d <- filtered_data()
    x <- as.factor(d[[input$mosaic_x]])
    y <- as.factor(d[[input$mosaic_y]])
    tbl <- table(x, y)
    mosaic(tbl, shade = input$mosaic_shade, legend = TRUE)
  }, res = 140)
  
  # -----------------------------
  # ggpairs
  # -----------------------------
  output$ggpairs_plot <- renderPlot({
    d <- filtered_data()
    
    n <- min(input$ggpairs_n, nrow(d))
    set.seed(1)
    d0 <- d[sample(seq_len(nrow(d)), n), , drop = FALSE]
    
    vars <- if (isTRUE(input$ggpairs_all)) colnames(d0) else input$VarsToShow
    req(vars)
    vars <- intersect(vars, colnames(d0))
    validate(need(length(vars) >= 2, "Please select at least 2 variables."))
    
    d2 <- d0[, vars, drop = FALSE]
    
    keep_type <- sapply(d2, function(x) is.numeric(x) || is.factor(x))
    d2 <- d2[, keep_type, drop = FALSE]
    
    keep_levels <- sapply(d2, function(x) {
      if (is.factor(x)) nlevels(x) <= 20 else TRUE
    })
    d2 <- d2[, keep_levels, drop = FALSE]
    
    ok <- sapply(d2, function(x) {
      x <- x[!is.na(x)]
      if (length(x) <= 1) return(FALSE)
      length(unique(x)) > 1
    })
    d2 <- d2[, ok, drop = FALSE]
    
    validate(need(ncol(d2) >= 2, "Not enough usable variables after filtering."))
    validate(need(ncol(d2) <= 15, "Too many variables selected. Please keep <= 15."))
    
    colVar <- input$VarToColour
    use_colour <- !is.null(colVar) && nzchar(colVar) && colVar != "None" && (colVar %in% colnames(d0))
    
    if (use_colour) {
      d2[[colVar]] <- as.factor(d0[[colVar]])
      p <- GGally::ggpairs(
        data = d2,
        mapping = ggplot2::aes_string(colour = colVar),
        progress = FALSE
      )
    } else {
      p <- GGally::ggpairs(data = d2, progress = FALSE)
    }
    
    print(p)
  }, res = 140)
  
  # -----------------------------
  # Corrgram
  # -----------------------------
  output$corr_plot <- renderPlot({
    d <- filtered_data()
    vars <- input$corr_vars
    req(vars)
    validate(need(length(vars) >= 2, "Please select at least 2 numeric variables."))
    
    d2 <- d[, vars, drop = FALSE]
    d2 <- d2[, sapply(d2, is.numeric), drop = FALSE]
    
    corrgram(
      d2,
      order = "HC",
      abs = TRUE,
      cor.method = input$corr_method,
      lower.panel = panel.shade,
      upper.panel = panel.shade,
      main = NULL
    )
  }, res = 140)
  
  # -----------------------------
  # Missingness
  # -----------------------------
  output$miss_plot <- renderPlot({
    req(input$miss_vars)
    validate(need(length(input$miss_vars) <= 10, "Please select at most 10 variables."))
    
    d <- filtered_data()[, input$miss_vars, drop = FALSE]
    
    if (input$miss_type == "dat") {
      p <- visdat::vis_dat(d)
    } else {
      p <- visdat::vis_miss(d, sort_miss = input$miss_sort)
    }
    
    p <- p +
      ggplot2::scale_x_discrete(position = "bottom") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45, hjust = 1, vjust = 1, size = 10
        ),
        plot.margin = ggplot2::margin(t = 10, r = 15, b = 60, l = 15)
      )
    
    print(p)
  }, res = 150)
  
  # -----------------------------
  # Distribution
  # -----------------------------
  output$dist_plot <- renderPlotly({
    d <- filtered_data()
    p <- ggplot(d, aes_string(x = input$dist_var)) +
      geom_histogram(bins = input$dist_bins, fill = "steelblue", colour = "white") +
      theme_minimal(base_size = 13) +
      labs(title = paste("Distribution of", input$dist_var), x = input$dist_var, y = "Count")
    ggplotly(p)
  })
  
  # -----------------------------
  # Tabplot
  # -----------------------------
  output$tabplot_chart <- renderPlot({
    req(input$tab_vars)
    validate(need(length(input$tab_vars) <= 10, "You can select up to 10 variables only."))
    
    d <- filtered_data()[, input$tab_vars, drop = FALSE]
    tableplot(d)
  }, res = 150)
  
  # -----------------------------
  # Boxplot
  # -----------------------------
  output$box_plot <- renderPlot({
    d <- filtered_data()
    x <- d[[input$box_var]]
    validate(need(is.numeric(x), "Selected variable is not numeric."))
    
    x2 <- x
    if (input$box_center || input$box_scale) {
      x2 <- as.numeric(scale(x2, center = input$box_center, scale = input$box_scale))
    }
    
    car::Boxplot(
      x2,
      range = input$box_range,
      id = list(n = 0),   #close label
      main = paste("Boxplot of", input$box_var),
      ylab = input$box_var
    )
  }, res = 140)
  
  # -----------------------------
  # Rising value
  # -----------------------------
  output$rise_plot <- renderPlot({
    d <- filtered_data()
    x <- d[[input$rise_var]]
    validate(need(is.numeric(x), "Selected variable is not numeric."))
    
    x <- x[!is.na(x)]
    if (length(x) < 2) return(NULL)
    
    if (input$rise_center || input$rise_scale) {
      x <- as.numeric(scale(x, center = input$rise_center, scale = input$rise_scale))
    }
    
    xs <- sort(x)
    dx <- diff(xs)
    
    plot(xs, type = "l",
         xlab = "Order (sorted index)",
         ylab = paste("Sorted", input$rise_var),
         main = paste("Rising-value chart:", input$rise_var))
    
    idx <- which(dx >= input$rise_thresh)
    if (length(idx) > 0) {
      points(idx, xs[idx], pch = 19)
      points(idx + 1, xs[idx + 1], pch = 19)
      abline(v = idx, lty = 2)
    }
  }, res = 140)
  
  # -----------------------------
  # Time series
  # -----------------------------
  output$ts_plot <- renderPlotly({
    d <- filtered_data()[, c("Date", input$ts_var), drop = FALSE]
    names(d) <- c("Date", "Value")
    
    if (input$ts_group == "Day") {
      agg <- aggregate(Value ~ Date, data = d, FUN = mean, na.rm = TRUE)
    } else if (input$ts_group == "Week") {
      d$Period <- as.Date(cut(d$Date, "week"))
      agg <- aggregate(Value ~ Period, data = d, FUN = mean, na.rm = TRUE)
      names(agg) <- c("Date", "Value")
    } else {
      d$Period <- as.Date(cut(d$Date, "month"))
      agg <- aggregate(Value ~ Period, data = d, FUN = mean, na.rm = TRUE)
      names(agg) <- c("Date", "Value")
    }
    
    p <- ggplot(agg, aes(Date, Value)) +
      geom_line(color = "#2b7bbb") +
      geom_point(color = "#2b7bbb", size = 1.5) +
      theme_minimal(base_size = 13) +
      labs(title = paste("Time series of", input$ts_var),
           x = "Date", y = "Average value")
    
    ggplotly(p)
  })
  
  # -----------------------------
  # Datatable
  # -----------------------------
  output$tbl_full <- renderDT({
    datatable(filtered_data(), options = list(pageLength = input$dt_page, scrollX = TRUE))
  })
}