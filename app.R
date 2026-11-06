# =============================================================================
# HomeValue | Housing Price Statistical Analysis
# Run: shiny::runApp("HomeValue_Shiny_App")
# =============================================================================

# ----------------------------- Packages --------------------------------------

required_packages <- c(
  "shiny", "bslib", "ggplot2", "dplyr", "DT", "broom", "scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages)) {
  stop(
    "Install the following packages: ",
    paste(missing_packages, collapse = ", "),
    "\nRun: install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(broom)
library(scales)

# ----------------------------- Data ------------------------------------------

load_housing <- function(path = "Housing.csv") {
  
  if (!file.exists(path)) {
    stop("Housing.csv was not found. Keep it in the same folder as app.R.")
  }
  
  dat <- read.csv(
    path,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "N/A")
  )
  
  required <- c(
    "price", "area", "bedrooms", "bathrooms", "stories",
    "mainroad", "guestroom", "basement", "hotwaterheating",
    "airconditioning", "parking", "prefarea", "furnishingstatus"
  )
  
  absent <- setdiff(required, names(dat))
  
  if (length(absent)) {
    stop("Missing columns: ", paste(absent, collapse = ", "))
  }
  
  dat <- dat %>%
    mutate(
      across(
        c(
          mainroad, guestroom, basement, hotwaterheating,
          airconditioning, prefarea, furnishingstatus
        ),
        as.factor
      )
    )
  
  dat
}

housing <- load_housing()

num_vars <- names(housing)[
  vapply(housing, is.numeric, logical(1))
]

cat_vars <- names(housing)[
  vapply(
    housing,
    function(x) is.factor(x) || is.character(x),
    logical(1)
  )
]

predictor_choices <- setdiff(names(housing), "price")
numeric_predictors <- setdiff(num_vars, "price")

# ----------------------------- Theme -----------------------------------------

app_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#236B5D",
  secondary = "#66756F",
  success = "#236B5D",
  bg = "#F6F8F7",
  fg = "#23332F",
  base_font = font_collection("Inter", "Arial", "sans-serif"),
  heading_font = font_collection("Inter", "Arial", "sans-serif")
)

# ----------------------------- UI --------------------------------------------

ui <- page_navbar(
  title = span("Housing Price Analysis", class = "nav-title"),
  id = "main_nav",
  selected = "Overview",
  theme = app_theme,
  
  header = tags$head(
    tags$style(HTML("
      body {
        background: #F6F8F7;
        color: #23332F;
        font-size: 14px;
      }

      .navbar {
        background: #FFFFFF !important;
        border-bottom: 1px solid #E1E9E5;
        box-shadow: 0 2px 8px rgba(25, 55, 45, .04);
        padding: 8px 18px;
      }

      .nav-title {
        color: #236B5D;
        font-size: 18px;
        font-weight: 750;
        letter-spacing: -.4px;
      }

      .navbar .nav-link {
        color: #52645D;
        font-weight: 600;
        font-size: 13px;
        border-radius: 7px;
        margin: 2px;
      }

      .navbar .nav-link.active {
        background: #E7F2ED !important;
        color: #236B5D !important;
      }

      .container-fluid {
        max-width: 1500px;
        padding-top: 24px;
        padding-bottom: 28px;
      }

      .page-heading {
        font-size: 25px;
        font-weight: 750;
        letter-spacing: -.6px;
        margin: 2px 0 5px;
        color: #203B32;
      }

      .page-description {
        color: #708078;
        font-size: 13px;
        margin-bottom: 22px;
      }

      .hero {
        background: #FFFFFF;
        border: 1px solid #DFE9E4;
        border-left: 5px solid #236B5D;
        border-radius: 12px;
        padding: 30px;
        margin-bottom: 22px;
      }

      .hero h1 {
        color: #203B32;
        font-size: clamp(25px, 3vw, 36px);
        font-weight: 780;
        letter-spacing: -1px;
        line-height: 1.2;
        margin: 0 0 12px;
        max-width: 750px;
      }

      .hero p {
        color: #64756D;
        max-width: 720px;
        line-height: 1.8;
        margin: 0;
      }

      .eyebrow {
        text-transform: uppercase;
        font-size: 10px;
        letter-spacing: 1.8px;
        color: #236B5D;
        font-weight: 800;
        margin-bottom: 12px;
      }

      .panel-card, .metric-card {
        background: #FFFFFF;
        border: 1px solid #E0E8E3;
        border-radius: 11px;
        padding: 20px;
        margin-bottom: 18px;
        box-shadow: 0 2px 7px rgba(25, 55, 45, .025);
      }

      .metric-card {
        min-height: 115px;
        padding: 19px;
      }

      .metric-label {
        color: #738078;
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: .8px;
      }

      .metric-value {
        color: #236B5D;
        font-size: 27px;
        font-weight: 800;
        margin-top: 10px;
        line-height: 1.15;
      }

      .metric-note {
        color: #89958F;
        font-size: 11px;
        margin-top: 7px;
      }

      .section-title {
        font-size: 16px;
        font-weight: 750;
        color: #29463B;
        margin: 2px 0 16px;
      }

      .help-note {
        color: #718078;
        font-size: 12px;
        line-height: 1.8;
      }

      .control-label {
        color: #344B41;
        font-size: 12px;
        font-weight: 700;
      }

      .form-control, .selectize-input {
        border-color: #DCE6E0;
        border-radius: 7px;
        box-shadow: none !important;
        font-size: 13px;
      }

      .btn-primary {
        background: #236B5D;
        border-color: #236B5D;
        border-radius: 7px;
        font-size: 12px;
        font-weight: 650;
        padding: 9px 15px;
      }

      .btn-primary:hover {
        background: #195447;
        border-color: #195447;
      }

      .table {
        font-size: 12px;
      }

      table.dataTable thead th {
        background: #EDF4F0;
        color: #36564A;
      }

      .stat-note {
        background: #F0F6F2;
        border-radius: 8px;
        padding: 13px 15px;
        color: #49665A;
        font-size: 12px;
        line-height: 1.8;
        margin-top: 12px;
      }

      .roadmap-item {
        border-bottom: 1px solid #EDF0EE;
        padding: 12px 0;
      }

      .roadmap-item:last-child {
        border-bottom: none;
      }

      .roadmap-number {
        display: inline-block;
        background: #E7F2ED;
        color: #236B5D;
        border-radius: 6px;
        padding: 5px 8px;
        font-size: 11px;
        font-weight: 800;
        margin-right: 10px;
      }

      .footer-note {
        color: #8A9790;
        font-size: 11px;
        text-align: center;
        padding: 20px;
        border-top: 1px solid #E2EAE5;
      }

      /* Overview-only styling */
      .overview-header {
        display: flex;
        justify-content: space-between;
        align-items: flex-end;
        gap: 20px;
        margin: 2px 0 22px;
      }

      .overview-header .page-heading {
        margin-bottom: 6px;
      }

      .overview-header .page-description {
        margin-bottom: 0;
        max-width: 700px;
        line-height: 1.7;
      }

      .overview-tag {
        flex-shrink: 0;
        padding: 8px 12px;
        background: #E7F2ED;
        color: #236B5D;
        border: 1px solid #D6E8DF;
        border-radius: 7px;
        font-size: 11px;
        font-weight: 700;
      }

      .overview-metrics .metric-card {
        min-height: 104px;
        padding: 17px 19px;
        margin-bottom: 16px;
      }

      .overview-metrics .metric-value {
        margin-top: 8px;
        font-size: 26px;
      }

      .overview-layout {
        display: grid;
        grid-template-columns: minmax(0, 1.15fr) minmax(0, 0.85fr);
        gap: 16px;
        align-items: stretch;
        margin-bottom: 16px;
      }

      .overview-layout .panel-card {
        height: 100%;
        margin-bottom: 0;
      }

      .overview-copy {
        color: #65766E;
        font-size: 13px;
        line-height: 1.8;
        margin-bottom: 14px;
      }

      .overview-objective {
        padding: 14px 16px;
        border-left: 3px solid #236B5D;
        background: #F0F6F2;
        border-radius: 0 8px 8px 0;
        color: #49665A;
        font-size: 12px;
        line-height: 1.7;
        margin-top: 17px;
      }

      .overview-objective strong {
        display: block;
        color: #236B5D;
        margin-bottom: 4px;
      }

      .overview-workflow {
        display: grid;
        grid-template-columns: repeat(5, minmax(0, 1fr));
        gap: 12px;
      }

      .overview-step {
        background: #FFFFFF;
        border: 1px solid #E0E8E3;
        border-radius: 9px;
        padding: 15px;
        min-height: 116px;
      }

      .overview-step-number {
        color: #236B5D;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: .5px;
        margin-bottom: 10px;
      }

      .overview-step-title {
        color: #29463B;
        font-size: 12px;
        font-weight: 750;
        margin-bottom: 6px;
      }

      .overview-step-description {
        color: #718078;
        font-size: 11px;
        line-height: 1.6;
      }

      .overview-dataset {
        margin-top: 16px;
      }

      .overview-dataset .panel-card {
        margin-bottom: 0;
      }

      .overview-dataset .section-title {
        margin-bottom: 18px;
      }

      .overview-dataset strong {
        color: #36564A;
        font-size: 12px;
      }

      .overview-dataset p {
        color: #718078;
        font-size: 12px;
        line-height: 1.7;
        overflow-wrap: anywhere;
        margin: 5px 0 15px;
      }

      @media (max-width: 900px) {
        .overview-workflow {
          grid-template-columns: repeat(3, minmax(0, 1fr));
        }
      }

      @media (max-width: 768px) {
        .hero { padding: 22px; }
        .container-fluid { padding-top: 16px; }
        .page-heading { font-size: 22px; }
        .panel-card { padding: 15px; }

        .overview-header {
          align-items: flex-start;
          flex-direction: column;
          gap: 10px;
        }

        .overview-layout {
          grid-template-columns: 1fr;
        }

        .overview-workflow {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 480px) {
        .overview-workflow {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  
  # ============================= OVERVIEW ===================================
  
  nav_panel(
    "Overview",
    
    div(
      class = "container-fluid",
      
      div(
        class = "overview-header",
        
        div(
          h2(
            class = "page-heading",
            "Housing Price Analysis"
          ),
          
          p(
            class = "page-description",
            "Explore housing data through descriptive statistics, ",
            "correlation, regression, and hypothesis testing."
          )
        ),
        
        div(
          class = "overview-tag",
          "STATISTICAL ANALYSIS"
        )
      ),
      
      div(
        class = "overview-metrics",
        uiOutput("overview_metrics")
      ),
      
      div(
        class = "overview-layout",
        
        div(
          class = "panel-card",
          
          h4(
            class = "section-title",
            "Project overview"
          ),
          
          p(
            class = "overview-copy",
            "This analysis explores how housing characteristics, ",
            "including area, rooms, amenities, and furnishing status, ",
            "relate to house prices."
          ),
          
          p(
            class = "overview-copy",
            "Statistical techniques are used to summarize the data, ",
            "measure linear relationships, estimate regression models, ",
            "and test associations between categorical variables."
          ),
          
          div(
            class = "overview-objective",
            
            strong("Research objective"),
            
            "To investigate the relationships between selected ",
            "property characteristics and housing prices using ",
            "statistical methods."
          )
        ),
        
        div(
          class = "panel-card overview-dataset",
          
          h4(
            class = "section-title",
            "Dataset snapshot"
          ),
          
          uiOutput("dataset_summary")
        )
      ),
      
      div(
        class = "panel-card",
        
        h4(
          class = "section-title",
          "Analysis workflow"
        ),
        
        div(
          class = "overview-workflow",
          
          div(
            class = "overview-step",
            
            div(
              class = "overview-step-number",
              "01 / EXPLORE"
            ),
            
            div(
              class = "overview-step-title",
              "Data exploration"
            ),
            
            div(
              class = "overview-step-description",
              "Inspect variables, data types, and missing values."
            )
          ),
          
          div(
            class = "overview-step",
            
            div(
              class = "overview-step-number",
              "02 / SUMMARIZE"
            ),
            
            div(
              class = "overview-step-title",
              "Descriptive statistics"
            ),
            
            div(
              class = "overview-step-description",
              "Study averages, spread, quartiles, and distributions."
            )
          ),
          
          div(
            class = "overview-step",
            
            div(
              class = "overview-step-number",
              "03 / COMPARE"
            ),
            
            div(
              class = "overview-step-title",
              "Correlation"
            ),
            
            div(
              class = "overview-step-description",
              "Examine the direction and strength of linear relationships."
            )
          ),
          
          div(
            class = "overview-step",
            
            div(
              class = "overview-step-number",
              "04 / MODEL"
            ),
            
            div(
              class = "overview-step-title",
              "Regression"
            ),
            
            div(
              class = "overview-step-description",
              "Estimate and assess simple and multiple linear models."
            )
          ),
          
          div(
            class = "overview-step",
            
            div(
              class = "overview-step-number",
              "05 / TEST"
            ),
            
            div(
              class = "overview-step-title",
              "Chi-square test"
            ),
            
            div(
              class = "overview-step-description",
              "Test whether categorical variables are associated."
            )
          )
        )
      )
    )
  ),
  
  # ============================= DATA EXPLORER ==============================
  
  nav_panel(
    "Data Explorer",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Data Explorer"),
      p(
        class = "page-description",
        "Inspect individual variables, their data types, distributions, and missing values."
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            h4(class = "section-title", "Variable selection"),
            
            selectInput(
              "explore_var",
              "Choose a variable",
              choices = names(housing),
              selected = "price"
            ),
            
            checkboxInput(
              "show_na",
              "Include missing values in category counts",
              FALSE
            ),
            
            p(
              class = "help-note",
              "Select a variable to view its summary and distribution."
            )
          )
        ),
        
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "section-title", "Variable profile"),
            uiOutput("explore_profile")
          )
        )
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Variable distribution"),
        plotOutput("explore_plot", height = "360px")
      )
    )
  ),
  
  # ============================= DESCRIPTIVE STATS ==========================
  
  nav_panel(
    "Descriptive Stats",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Descriptive Statistics"),
      p(
        class = "page-description",
        "Summarize central tendency, spread, quartiles, and the distribution of a numeric variable."
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            
            selectInput(
              "desc_var",
              "Numeric variable",
              choices = num_vars,
              selected = "price"
            ),
            
            selectInput(
              "desc_plot",
              "Distribution plot",
              choices = c(
                "Histogram" = "hist",
                "Boxplot" = "box"
              )
            )
          )
        ),
        
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "section-title", "Statistical summary"),
            tableOutput("desc_table")
          )
        )
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Distribution"),
        plotOutput("desc_plot_out", height = "370px")
      )
    )
  ),
  
  # ============================= CORRELATION ================================
  
  nav_panel(
    "Correlation",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Correlation Analysis"),
      p(
        class = "page-description",
        "Explore the direction and strength of linear relationships between numeric variables."
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            
            h4(class = "section-title", "Variables"),
            
            selectInput(
              "corr_vars",
              "Select numeric variables",
              choices = num_vars,
              selected = num_vars,
              multiple = TRUE
            ),
            
            p(
              class = "help-note",
              "Pearson correlation ranges from -1 to +1. Select at least two variables."
            )
          )
        ),
        
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "section-title", "Correlation matrix"),
            plotOutput("corr_plot", height = "460px")
          )
        )
      )
    )
  ),
  
  # ============================= SIMPLE REGRESSION ==========================
  
  nav_panel(
    "Simple Regression",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Simple Linear Regression"),
      p(
        class = "page-description",
        "Model house price using one numeric predictor."
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            
            selectInput(
              "slr_x",
              "Predictor (X)",
              choices = numeric_predictors,
              selected = "area"
            ),
            
            checkboxInput(
              "slr_ci",
              "Show confidence interval",
              TRUE
            ),
            
            div(
              class = "stat-note",
              strong("Model"),
              br(),
              "Price = Intercept + Slope × Predictor"
            )
          )
        ),
        
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "section-title", "Regression line"),
            plotOutput("slr_plot", height = "380px")
          )
        )
      ),
      
      fluidRow(
        column(
          5,
          div(
            class = "panel-card",
            h4(class = "section-title", "Model summary"),
            tableOutput("slr_summary")
          )
        ),
        
        column(
          7,
          div(
            class = "panel-card",
            h4(class = "section-title", "Regression coefficients"),
            DTOutput("slr_coef")
          )
        )
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Interpretation"),
        uiOutput("slr_interpretation")
      )
    )
  ),
  
  # ============================= MULTIPLE REGRESSION ========================
  
  nav_panel(
    "Multiple Regression",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Multiple Linear Regression"),
      p(
        class = "page-description",
        "Evaluate house price using several numeric and categorical predictors simultaneously."
      ),
      
      fluidRow(
        column(
          4,
          div(
            class = "panel-card",
            
            h4(class = "section-title", "Model configuration"),
            
            selectizeInput(
              "mlr_x",
              "Predictor variables",
              choices = predictor_choices,
              selected = c(
                "area", "bedrooms", "bathrooms", "stories",
                "parking", "airconditioning", "furnishingstatus"
              ),
              multiple = TRUE,
              options = list(
                plugins = list("remove_button")
              )
            ),
            
            checkboxInput(
              "mlr_log",
              "Use log(price) as target",
              FALSE
            ),
            
            actionButton(
              "fit_mlr",
              "Fit model",
              class = "btn-primary"
            ),
            
            p(
              class = "help-note",
              "Factor predictors are automatically represented using indicator contrasts."
            )
          )
        ),
        
        column(
          8,
          div(
            class = "panel-card",
            h4(class = "section-title", "Model performance"),
            uiOutput("mlr_metrics")
          )
        )
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Regression coefficients"),
        DTOutput("mlr_coef")
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Actual vs predicted prices"),
        plotOutput("mlr_pred_plot", height = "360px")
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Model interpretation"),
        uiOutput("mlr_interpretation")
      ),
      
      
    )
  ),
  
  # ============================= CHI-SQUARE ================================
  
  nav_panel(
    "Chi-square",
    
    div(
      class = "container-fluid",
      
      h2(class = "page-heading", "Chi-square Test of Independence"),
      p(
        class = "page-description",
        "Test whether two categorical variables are statistically associated."
      ),
      
      fluidRow(
        column(
          3,
          div(
            class = "panel-card",
            
            h4(class = "section-title", "Test configuration"),
            
            selectInput(
              "chi_x",
              "First categorical variable",
              choices = cat_vars,
              selected = "airconditioning"
            ),
            
            selectInput(
              "chi_y",
              "Second categorical variable",
              choices = cat_vars,
              selected = "furnishingstatus"
            ),
            
            div(
              class = "stat-note",
              strong("Hypotheses"),
              br(),
              "H0: The variables are independent.",
              br(),
              "H1: The variables are associated."
            )
          )
        ),
        
        column(
          9,
          div(
            class = "panel-card",
            h4(class = "section-title", "Contingency table"),
            tableOutput("chi_table")
          )
        )
      ),
      
      fluidRow(
        column(
          5,
          div(
            class = "panel-card",
            h4(class = "section-title", "Test results"),
            tableOutput("chi_result")
          )
        ),
        
        column(
          7,
          div(
            class = "panel-card",
            h4(class = "section-title", "Observed frequencies"),
            plotOutput("chi_plot", height = "320px")
          )
        )
      ),
      
      div(
        class = "panel-card",
        h4(class = "section-title", "Interpretation"),
        uiOutput("chi_interpretation")
      )
    )
  ),
  
  
)

# ============================= SERVER =========================================

server <- function(input, output, session) {
  
  # -------------------------- Overview ---------------------------------------
  
  output$overview_metrics <- renderUI({
    
    n <- nrow(housing)
    p <- sum(!is.na(housing$price))
    
    fluidRow(
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Housing records"),
          div(class = "metric-value", format(n, big.mark = ",")),
          div(class = "metric-note", "Rows in the dataset")
        )
      ),
      
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Variables"),
          div(class = "metric-value", ncol(housing)),
          div(class = "metric-note", "Price and property features")
        )
      ),
      
      column(
        4,
        div(
          class = "metric-card",
          div(class = "metric-label", "Valid prices"),
          div(class = "metric-value", format(p, big.mark = ",")),
          div(class = "metric-note", "Non-missing target values")
        )
      )
    )
  })
  
  output$dataset_summary <- renderUI({
    
    div(
      fluidRow(
        column(
          6,
          strong("Target variable"),
          p("price"),
          strong("Numeric variables"),
          p(paste(num_vars, collapse = ", "))
        ),
        
        column(
          6,
          strong("Categorical variables"),
          p(paste(cat_vars, collapse = ", ")),
          strong("Missing values"),
          p(sum(is.na(housing)))
        )
      )
    )
  })
  
  # -------------------------- Data Explorer ---------------------------------
  
  output$explore_profile <- renderUI({
    
    x <- housing[[input$explore_var]]
    
    if (is.numeric(x)) {
      
      div(
        fluidRow(
          column(4, strong("Data type"), p("Numeric")),
          column(4, strong("Valid observations"),
                 p(sum(!is.na(x)))),
          column(4, strong("Missing values"),
                 p(sum(is.na(x))))
        ),
        
        if (sum(!is.na(x)) > 0) {
          fluidRow(
            column(4, strong("Mean"),
                   p(comma(mean(x, na.rm = TRUE), accuracy = .01))),
            column(4, strong("Median"),
                   p(comma(median(x, na.rm = TRUE), accuracy = .01))),
            column(4, strong("Standard deviation"),
                   p(comma(sd(x, na.rm = TRUE), accuracy = .01)))
          )
        }
      )
      
    } else {
      
      tab <- table(
        x,
        useNA = if (isTRUE(input$show_na)) "ifany" else "no"
      )
      
      div(
        fluidRow(
          column(4, strong("Data type"), p("Categorical")),
          column(4, strong("Valid observations"),
                 p(sum(!is.na(x)))),
          column(4, strong("Categories"),
                 p(length(unique(x[!is.na(x)]))))
        ),
        
        h5("Category frequencies"),
        
        tableOutput("explore_frequency")
      )
    }
  })
  
  output$explore_frequency <- renderTable({
    
    x <- housing[[input$explore_var]]
    
    validate(need(!is.numeric(x), ""))
    
    tab <- as.data.frame(
      table(
        x,
        useNA = if (isTRUE(input$show_na)) "ifany" else "no"
      )
    )
    
    names(tab) <- c("Category", "Frequency")
    
    tab
  }, striped = TRUE, bordered = TRUE)
  
  output$explore_plot <- renderPlot({
    
    x <- housing[[input$explore_var]]
    
    if (is.numeric(x)) {
      
      ggplot(
        data.frame(value = x),
        aes(x = value)
      ) +
        geom_histogram(
          bins = 30,
          fill = "#3A8775",
          color = "white",
          na.rm = TRUE
        ) +
        labs(
          title = paste("Distribution of", input$explore_var),
          x = input$explore_var,
          y = "Frequency"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      
      df <- data.frame(
        category = as.character(x),
        stringsAsFactors = FALSE
      )
      
      if (!isTRUE(input$show_na)) {
        df <- df[!is.na(df$category), , drop = FALSE]
      } else {
        df$category[is.na(df$category)] <- "(Missing)"
      }
      
      ggplot(df, aes(x = category)) +
        geom_bar(fill = "#3A8775", width = .7) +
        labs(
          title = paste("Frequency of", input$explore_var),
          x = input$explore_var,
          y = "Frequency"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 25, hjust = 1)
        )
    }
  })
  
  # -------------------------- Descriptive Statistics ------------------------
  
  output$desc_table <- renderTable({
    
    x <- housing[[input$desc_var]]
    
    validate(need(sum(!is.na(x)) > 0, "No valid observations."))
    
    data.frame(
      Statistic = c(
        "Valid observations",
        "Missing values",
        "Mean",
        "Median",
        "Standard deviation",
        "Variance",
        "Minimum",
        "First quartile (25%)",
        "Third quartile (75%)",
        "Maximum"
      ),
      
      Value = c(
        sum(!is.na(x)),
        sum(is.na(x)),
        mean(x, na.rm = TRUE),
        median(x, na.rm = TRUE),
        sd(x, na.rm = TRUE),
        var(x, na.rm = TRUE),
        min(x, na.rm = TRUE),
        unname(quantile(x, .25, na.rm = TRUE)),
        unname(quantile(x, .75, na.rm = TRUE)),
        max(x, na.rm = TRUE)
      )
    )
  }, digits = 3, striped = TRUE, bordered = TRUE)
  
  output$desc_plot_out <- renderPlot({
    
    x <- housing[[input$desc_var]]
    
    if (input$desc_plot == "hist") {
      
      ggplot(data.frame(value = x), aes(x = value)) +
        geom_histogram(
          bins = 30,
          fill = "#3A8775",
          color = "white",
          na.rm = TRUE
        ) +
        labs(
          title = paste("Distribution of", input$desc_var),
          x = input$desc_var,
          y = "Frequency"
        ) +
        theme_minimal(base_size = 13)
      
    } else {
      
      ggplot(data.frame(value = x), aes(y = value)) +
        geom_boxplot(
          fill = "#D5E8DF",
          color = "#236B5D",
          na.rm = TRUE,
          width = .35
        ) +
        labs(
          title = paste("Boxplot of", input$desc_var),
          x = NULL,
          y = input$desc_var
        ) +
        theme_minimal(base_size = 13)
    }
  })
  
  # -------------------------- Correlation ------------------------------------
  
  selected_numeric <- reactive({
    
    req(input$corr_vars)
    
    intersect(input$corr_vars, num_vars)
  })
  
  correlation_matrix <- reactive({
    
    vars <- selected_numeric()
    
    validate(
      need(length(vars) >= 2, "Select at least two numeric variables.")
    )
    
    validate(
      need(
        all(vapply(housing[, vars, drop = FALSE], function(x) {
          sd(x, na.rm = TRUE) > 0
        }, logical(1))),
        "Selected variables must have non-zero variance."
      )
    )
    
    cor(
      housing[, vars, drop = FALSE],
      use = "pairwise.complete.obs",
      method = "pearson"
    )
  })
  
  output$corr_plot <- renderPlot({
    
    m <- correlation_matrix()
    
    long <- as.data.frame(as.table(m))
    names(long) <- c("Variable1", "Variable2", "Correlation")
    
    ggplot(
      long,
      aes(Variable1, Variable2, fill = Correlation)
    ) +
      geom_tile(color = "white", linewidth = .8) +
      geom_text(
        aes(label = sprintf("%.2f", Correlation)),
        size = 4,
        color = "#203B32"
      ) +
      scale_fill_gradient2(
        low = "#C56A65",
        mid = "#FFFFFF",
        high = "#3A8775",
        midpoint = 0,
        limits = c(-1, 1)
      ) +
      labs(
        title = "Pearson Correlation Matrix",
        subtitle = "Values closer to +1 or -1 indicate stronger linear relationships",
        x = NULL,
        y = NULL,
        fill = "Correlation"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 35, hjust = 1),
        panel.grid = element_blank(),
        plot.title = element_text(face = "bold")
      )
  })
  
  # -------------------------- Simple Regression ------------------------------
  
  slr_model <- reactive({
    
    req(input$slr_x)
    
    lm(
      reformulate(input$slr_x, response = "price"),
      data = housing,
      na.action = na.omit
    )
  })
  
  output$slr_plot <- renderPlot({
    
    m <- slr_model()
    
    ggplot(
      model.frame(m),
      aes(x = .data[[input$slr_x]], y = price)
    ) +
      geom_point(
        alpha = .55,
        color = "#236B5D"
      ) +
      geom_smooth(
        method = "lm",
        se = isTRUE(input$slr_ci),
        color = "#C56F53",
        fill = "#F0D5C9"
      ) +
      labs(
        title = paste("House Price vs", input$slr_x),
        x = input$slr_x,
        y = "House price"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$slr_summary <- renderTable({
    
    m <- slr_model()
    s <- summary(m)
    
    data.frame(
      Measure = c(
        "Observations",
        "R-squared",
        "Adjusted R-squared",
        "Residual standard error",
        "Model p-value"
      ),
      
      Value = c(
        nobs(m),
        s$r.squared,
        s$adj.r.squared,
        s$sigma,
        pf(
          s$fstatistic[1],
          s$fstatistic[2],
          s$fstatistic[3],
          lower.tail = FALSE
        )
      )
    )
  }, digits = 4, striped = TRUE, bordered = TRUE)
  
  output$slr_coef <- renderDT({
    
    tab <- tidy(slr_model(), conf.int = TRUE)
    
    datatable(
      tab,
      options = list(dom = "tip", scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c(
          "estimate", "std.error", "statistic",
          "p.value", "conf.low", "conf.high"
        ),
        digits = 4
      )
  })
  
  output$slr_interpretation <- renderUI({
    
    m <- slr_model()
    s <- summary(m)
    
    co <- coef(m)[2]
    pv <- s$coefficients[2, 4]
    
    div(
      p(
        sprintf(
          "A one-unit increase in %s is associated with an estimated average change of %s in price.",
          input$slr_x,
          comma(co, accuracy = .01)
        )
      ),
      
      p(
        sprintf(
          "R-squared = %.3f. The model explains approximately %.1f%% of the observed variation in price.",
          s$r.squared,
          100 * s$r.squared
        )
      ),
      
      p(
        if (pv < .05) {
          "The slope is statistically significant at the 5% significance level."
        } else {
          "The slope is not statistically significant at the 5% significance level."
        }
      ),
      
      class = "help-note"
    )
  })
  
  # -------------------------- Multiple Regression ----------------------------
  
  mlr_model <- eventReactive(input$fit_mlr, {
    
    req(input$mlr_x)
    
    predictors <- unique(input$mlr_x)
    
    validate(
      need(length(predictors) > 0, "Select at least one predictor.")
    )
    
    target <- if (isTRUE(input$mlr_log)) "log(price)" else "price"
    
    if (isTRUE(input$mlr_log)) {
      validate(
        need(
          all(housing$price[!is.na(housing$price)] > 0),
          "All observed prices must be positive for log(price)."
        )
      )
    }
    
    form <- reformulate(predictors, response = target)
    
    lm(form, data = housing, na.action = na.omit)
    
  }, ignoreInit = FALSE)
  
  mlr_data <- reactive({
    
    m <- mlr_model()
    
    d <- model.frame(m)
    pred <- predict(m, newdata = d)
    actual <- model.response(d)
    
    if (isTRUE(input$mlr_log)) {
      actual_original <- exp(actual)
      pred_original <- exp(pred)
    } else {
      actual_original <- actual
      pred_original <- pred
    }
    
    data.frame(
      actual = actual_original,
      predicted = pred_original,
      residual = actual_original - pred_original
    )
  })
  
  output$mlr_metrics <- renderUI({
    
    m <- mlr_model()
    s <- summary(m)
    
    fluidRow(
      column(
        6,
        div(
          class = "metric-card",
          div(class = "metric-label", "Observations"),
          div(class = "metric-value", format(nobs(m), big.mark = ","))
        )
      ),
      
      column(
        6,
        div(
          class = "metric-card",
          div(class = "metric-label", "R-squared"),
          div(class = "metric-value", sprintf("%.3f", s$r.squared))
        )
      ),
      
      column(
        6,
        div(
          class = "metric-card",
          div(class = "metric-label", "Adjusted R-squared"),
          div(class = "metric-value", sprintf("%.3f", s$adj.r.squared))
        )
      ),
      
      column(
        6,
        div(
          class = "metric-card",
          div(class = "metric-label", "Residual standard error"),
          div(class = "metric-value", comma(s$sigma, accuracy = .01))
        )
      )
    )
  })
  
  output$mlr_coef <- renderDT({
    
    m <- mlr_model()
    tab <- tidy(m, conf.int = TRUE)
    
    datatable(
      tab,
      options = list(pageLength = 12, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = intersect(
          c(
            "estimate", "std.error", "statistic",
            "p.value", "conf.low", "conf.high"
          ),
          names(tab)
        ),
        digits = 4
      )
  })
  
  output$mlr_pred_plot <- renderPlot({
    
    d <- mlr_data()
    
    ggplot(d, aes(x = actual, y = predicted)) +
      geom_point(alpha = .65, color = "#236B5D") +
      geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dashed",
        color = "#C56F53",
        linewidth = 1
      ) +
      labs(
        x = "Actual price",
        y = "Predicted price",
        title = "Actual vs Predicted Price",
        subtitle = "Points closer to the diagonal indicate closer predictions"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$mlr_interpretation <- renderUI({
    
    m <- mlr_model()
    s <- summary(m)
    
    sig <- tidy(m) %>%
      filter(
        term != "(Intercept)",
        !is.na(p.value),
        p.value < .05
      ) %>%
      pull(term)
    
    div(
      p(
        sprintf(
          "Predictors included: %s.",
          paste(attr(terms(m), "term.labels"), collapse = ", ")
        )
      ),
      
      p(
        sprintf(
          "R-squared = %.3f; adjusted R-squared = %.3f.",
          s$r.squared,
          s$adj.r.squared
        )
      ),
      
      p(
        if (length(sig)) {
          paste(
            "Terms with p-values below 0.05:",
            paste(sig, collapse = ", "),
            "."
          )
        } else {
          "No predictor terms have p-values below 0.05."
        }
      ),
      
      p(
        "Interpret each coefficient while holding the other included predictors constant. Categorical coefficients are comparisons with a reference category."
      ),
      
      class = "help-note"
    )
  })
  
  # -------------------------- Chi-square Test -------------------------------
  
  chi_data <- reactive({
    
    req(input$chi_x, input$chi_y)
    
    validate(
      need(
        input$chi_x != input$chi_y,
        "Choose two different categorical variables."
      )
    )
    
    table(
      housing[[input$chi_x]],
      housing[[input$chi_y]]
    )
  })
  
  output$chi_table <- renderTable({
    
    as.data.frame.matrix(chi_data())
    
  }, striped = TRUE, bordered = TRUE)
  
  chi_test <- reactive({
    
    tab <- chi_data()
    
    validate(
      need(
        nrow(tab) > 1 && ncol(tab) > 1,
        "Both variables must have at least two categories."
      )
    )
    
    # Yates correction is applied automatically when applicable.
    suppressWarnings(chisq.test(tab))
  })
  
  output$chi_result <- renderTable({
    
    z <- chi_test()
    
    data.frame(
      Measure = c(
        "Chi-square statistic",
        "Degrees of freedom",
        "P-value",
        "Minimum expected count"
      ),
      
      Result = c(
        unname(z$statistic),
        unname(z$parameter),
        z$p.value,
        min(z$expected)
      )
    )
    
  }, digits = 5, striped = TRUE, bordered = TRUE)
  
  output$chi_plot <- renderPlot({
    
    tab <- as.data.frame(chi_data())
    names(tab) <- c("Category1", "Category2", "Count")
    
    ggplot(
      tab,
      aes(x = Category1, y = Count, fill = Category2)
    ) +
      geom_col(position = "dodge") +
      labs(
        x = input$chi_x,
        y = "Observed frequency",
        fill = input$chi_y,
        title = "Observed Category Frequencies"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 25, hjust = 1)
      )
  })
  
  output$chi_interpretation <- renderUI({
    
    z <- chi_test()
    
    if (min(z$expected) < 5) {
      note <- paste(
        "Warning: at least one expected count is below 5.",
        "The chi-square approximation may be unreliable.",
        "Consider an appropriate exact or Monte Carlo test."
      )
    } else {
      note <- paste(
        "All expected counts are at least 5.",
        "The usual expected-count guideline is satisfied."
      )
    }
    
    div(
      p(
        if (z$p.value < .05) {
          paste0(
            "Reject the null hypothesis at the 5% significance level. ",
            "There is evidence of an association between ",
            input$chi_x, " and ", input$chi_y, "."
          )
        } else {
          paste0(
            "Do not reject the null hypothesis at the 5% significance level. ",
            "There is insufficient evidence of an association between ",
            input$chi_x, " and ", input$chi_y, "."
          )
        }
      ),
      
      p(sprintf("P-value = %.5f.", z$p.value)),
      
      p(note),
      
      p(
        "The test identifies association, not causation.",
        class = "help-note"
      )
    )
  })
}

# ----------------------------- Run App ---------------------------------------

shinyApp(ui = ui, server = server)