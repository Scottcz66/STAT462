library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(plotly)


compact_sidebar <- function(...) {
  bslib::sidebar(
    ...,
    width = "260px",
    open = TRUE,
    bg = "#f8fbff"
  )
}

make_viz_tab <- function(id, controls, main_content) {
  tagList(
    div(
      class = "viz-topbar",
      actionLink(
        inputId = paste0(id, "_toggle"),
        label = NULL,
        icon = icon("sliders"),
        class = "toggle-link"
      )
    ),
    div(
      id = paste0(id, "_wrap"),
      class = "viz-wrap",
      div(
        class = "viz-controls",
        controls
      ),
      div(
        class = "viz-main",
        main_content
      )
    )
  )
}

ui <- navbarPage(
  title = "Assignment 1 - Zheng Chao",
  id = "main_nav",
  
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
      .navbar {
        background-color: #dff1ff !important;
        border-bottom: 1px solid #c7e6ff !important;
      }
      .navbar-default .navbar-nav > li > a,
      .navbar-default .navbar-brand {
        color: #1f3b57 !important;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover {
        background-color: #c8e8ff !important;
        color: #12324d !important;
      }

      .global-filter {
        padding: 10px 18px 0 18px;
        background: #f8fbff;
        border-bottom: 1px solid #e2eef8;
      }

      .full-sidebar-select .selectize-control,
      .full-sidebar-select .selectize-input,
      .full-sidebar-select .selectize-dropdown {
        width: 100% !important;
      }

      .full-sidebar-select .selectize-dropdown {
        min-width: 100% !important;
      }

      .full-sidebar-select .selectize-dropdown-content {
        max-height: 260px !important;
      }
      .summary-box {
        background: #f8fbff;
        border: 1px solid #d9e9f6;
        border-radius: 8px;
        padding: 12px;
        margin-bottom: 12px;
        text-align: center;
      }
      .summary-box h4 {
        margin: 0 0 6px 0;
        font-size: 14px;
        color: #46627d;
      }
      .summary-box .big {
        font-size: 24px;
        font-weight: 700;
        color: #16354f;
      }

      .viz-topbar {
        display: flex;
        justify-content: flex-start;
        margin-bottom: 10px;
      }

      .toggle-link {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 38px;
        height: 38px;
        border-radius: 8px;
        background: #7cc7ff;
        color: #ffffff !important;
        font-size: 18px;
        text-decoration: none !important;
      }

      .toggle-link:hover {
        background: #59b7ff;
        color: #ffffff !important;
      }

      .viz-wrap {
        display: grid;
        grid-template-columns: 290px minmax(0, 1fr);
        gap: 16px;
        align-items: start;
        transition: all 0.25s ease;
      }

      .viz-wrap.collapsed {
        grid-template-columns: 0 minmax(0, 1fr);
      }

      .viz-wrap .viz-controls {
        background: #f8fbff;
        border: 1px solid #d9e9f6;
        border-radius: 10px;
        padding: 14px;
        min-width: 0;
        overflow: visible;
        transition: all 0.25s ease;
        position: relative;
        z-index: 10;
      }
      .selectize-dropdown,
      .dropdown-menu,
      .bootstrap-select .dropdown-menu {
        z-index: 3000 !important;
      }
      .viz-wrap .viz-controls {
        max-height: calc(100vh - 180px);
        overflow-y: auto;
        overflow-x: visible;
      }

      .viz-wrap.collapsed .viz-controls {
        padding: 0;
        border: none;
        width: 0;
        opacity: 0;
      }

      .viz-wrap .viz-main {
        min-width: 0;
        width: 100%;
      }

      .plot-title {
        font-size: 30px;
        font-weight: 600;
        margin: 0 0 18px 0;
        color: #1f1f1f;
      }

      .overview-section {
        width: 100%;
        border: 1px solid #d9e9f6;
        border-radius: 10px;
        background: #ffffff;
        margin-bottom: 14px;
        overflow: hidden;
      }

      .overview-section summary {
        list-style: none;
        cursor: pointer;
        padding: 16px 20px;
        font-size: 28px;
        font-weight: 600;
        background: #eef8ff;
        color: #18384f;
        border-bottom: 1px solid #d9e9f6;
      }

      .overview-section summary::-webkit-details-marker {
        display: none;
      }

      .overview-body {
        padding: 16px 18px;
      }

      .dt-center {
        width: 100%;
      }
    "))
    ),
    div(
      class = "global-filter",
      fluidRow(
        column(
          4,
          dateRangeInput(
            "date_range",
            "Select period:",
            start = date_min,
            end = date_max,
            min = date_min,
            max = date_max
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Overview",
    fluidPage(
      fluidRow(
        column(4, uiOutput("box_nrow")),
        column(4, uiOutput("box_ncol")),
        column(4, uiOutput("box_missing"))
      ),
      br(),
      
      tags$details(
        open = NA,
        class = "overview-section",
        tags$summary("Dataset preview"),
        div(class = "overview-body", DTOutput("tbl_preview"))
      ),
      
      tags$details(
        class = "overview-section",
        tags$summary("Data type summary"),
        div(class = "overview-body", DTOutput("type_summary_dt"))
      ),
      
      tags$details(
        class = "overview-section",
        tags$summary("Top missing variables"),
        div(
          class = "overview-body",
          plotOutput("overall_missing_plot", height = "420px"),
          br(),
          DTOutput("missing_summary_tbl")
        )
      ),
      
      tags$details(
        class = "overview-section",
        tags$summary("Date range summary"),
        div(class = "overview-body", DTOutput("date_summary_tbl"))
      )
    )
  ),
  
  tabPanel(
    "Mosaic Chart",
    make_viz_tab(
      "mosaic",
      tagList(
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "mosaic_x",
            "X (categorical):",
            choices = cat_vars,
            selected = "Priority",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "mosaic_y",
            "Y (categorical):",
            choices = cat_vars,
            selected = "Agreed",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        checkboxInput("mosaic_shade", "Shade (highlight associations)", value = TRUE)
      ),
      plotOutput("mosaic_plot", height = "700px")
    )
  ),
  
  tabPanel(
    "ggpairs (GGally)",
    make_viz_tab(
      "ggpairs",
      tagList(
        checkboxInput("ggpairs_all", "Use ALL variables (slow)", value = FALSE),
        sliderInput("ggpairs_n", "Sample rows for ggpairs:", min = 50, max = nrow(data), value = 200, step = 50),
        tags$b("Variables to chart (max 10):"),
        
        tags$div(
          style = "
            border: 1px solid #d9e9f6;
            border-radius: 8px;
            padding: 10px;
            height: 320px;
            overflow-y: auto;
            overflow-x: hidden;
            background: #ffffff;
          ",
          checkboxGroupInput(
            "VarsToShow",
            label = NULL,
            choices = ggpairsVars,
            selected = intersect(
              c("Y", "sensor1", "sensor2", "sensor3", "sensor4", "sensor5"),
              ggpairsVars
            )
          )
        ),
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "VarToColour",
            "Variable to colour (factor):",
            choices = c("None", as.character(factVars)),
            selected = "None",
            options = list(
              dropdownParent = "body"
            )
          )
        )
      ),
      plotOutput("ggpairs_plot", height = "950px")
    )
  ),
  
  tabPanel(
    "Correlation (corrgram)",
    make_viz_tab(
      "corr",
      tagList(
        checkboxGroupInput("corr_vars", "Select numeric vars:",
                           choices = num_vars,
                           selected = c("Y", "sensor1", "sensor2", "sensor3", "sensor4")),
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "corr_method",
            "Method:",
            choices = c("pearson", "spearman"),
            selected = "pearson",
            options = list(
              dropdownParent = "body"
            )
          )
        )
      ),
      plotOutput("corr_plot", height = "850px")
    )
  ),
  
  tabPanel(
    "Missingness (visdat)",
    make_viz_tab(
      "miss",
      tagList(
        radioButtons(
          "miss_type",
          "Chart type:",
          choices = c(
            "vis_miss (missing pattern)" = "miss",
            "vis_dat (data type + NA)" = "dat"
          ),
          selected = "miss"
        ),
        checkboxInput("miss_sort", "Sort by missingness", value = TRUE),
        tags$b("Variables to show (max 10):"),
        
        tags$div(
          style = "
    border: 1px solid #d9e9f6;
    border-radius: 8px;
    padding: 10px;
    height: 320px;
    overflow-y: auto;
    overflow-x: hidden;
    background: #ffffff;
  ",
          checkboxGroupInput(
            "miss_vars",
            label = NULL,
            choices = names(data),
            selected = intersect(
              c("Y", "ID", "Operator", "Date", "Priority", "Price",
                "Speed", "Duration", "Temp", "Location"),
              names(data)
            )
          )
        )
      ),
      tagList(
        div(class = "plot-title", "Missingness"),
        plotOutput("miss_plot", height = "780px")
      )
    )
  ),
  
  tabPanel(
    "Distribution",
    make_viz_tab(
      "dist",
      tagList(
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "dist_var",
            "Select numeric variable:",
            choices = num_vars,
            selected = "Y",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        sliderInput("dist_bins", "Number of bins:", min = 10, max = 100, value = 30)
      ),
      plotlyOutput("dist_plot", height = "700px")
    )
  ),
  
  tabPanel(
    "Tabplot",
    make_viz_tab(
      "tabp",
      tagList(
        helpText("You can select up to 10 variables only."),
        tags$b("Variables to show (max 10):"),
        
        tags$div(
          style = "
    border: 1px solid #d9e9f6;
    border-radius: 8px;
    padding: 10px;
    height: 320px;
    overflow-y: auto;
    overflow-x: hidden;
    background: #ffffff;
  ",
          checkboxGroupInput(
            "tab_vars",
            label = NULL,
            choices = names(data),
            selected = tabplot_default_vars
          )
        )
      ),
      plotOutput("tabplot_chart", height = "950px")
    )
  ),
  
  tabPanel(
    "Boxplot (car)",
    make_viz_tab(
      "box",
      tagList(
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "box_var",
            "Numeric var:",
            choices = num_vars,
            selected = "Y",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        sliderInput("box_range", "Outlier rule (range):", min = 0.5, max = 5, value = 1.5, step = 0.1),
        checkboxInput("box_center", "Center numeric data", value = FALSE),
        checkboxInput("box_scale", "Scale numeric data", value = FALSE)
      ),
      plotOutput("box_plot", height = "800px")
    )
  ),
  
  tabPanel(
    "Rising value",
    make_viz_tab(
      "rise",
      tagList(
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "rise_var",
            "Numeric var:",
            choices = num_vars,
            selected = "Y",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        sliderInput("rise_thresh", "Gap threshold (highlight diffs >=):", min = 0, max = 50, value = 5, step = 0.5),
        checkboxInput("rise_center", "Center", value = FALSE),
        checkboxInput("rise_scale", "Scale", value = FALSE)
      ),
      plotOutput("rise_plot", height = "800px")
    )
  ),
  
  tabPanel(
    "Time series",
    make_viz_tab(
      "ts",
      tagList(
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "ts_var",
            "Numeric variable:",
            choices = num_vars,
            selected = "Y",
            options = list(
              dropdownParent = "body"
            )
          )
        ),
        div(
          class = "full-sidebar-select",
          selectizeInput(
            "ts_group",
            "Aggregate by:",
            choices = c("Day", "Week", "Month"),
            selected = "Week",
            options = list(
              dropdownParent = "body"
            )
          )
        )
      ),
      plotlyOutput("ts_plot", height = "700px")
    )
  ),
  
  tabPanel(
    "Datatable (DT)",
    make_viz_tab(
      "dt",
      tagList(
        sliderInput("dt_page", "Rows per page:", min = 5, max = 50, value = 10, step = 5)
      ),
      DTOutput("tbl_full")
    )
  )
)





