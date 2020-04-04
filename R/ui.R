#' @import shiny
ui <- function(req) {
  fluidPage(
    tags$head(tags$link(href = "covidshiny/app.css", rel="stylesheet", type="text/css")),

    tags$br(),

    titlePanel(app.title),

    actionLink('howtouse', "Learn more about this tool."),

    tags$br(), tags$br(),

    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Settings",

            tags$br(),

            # Location Information
            h4(strong("Location Information")),

            numericInput(
              inputId = 'num_people',
              label = "Number of People in Area",
              value = 883305
            ),

            shinyWidgets::radioGroupButtons(
              inputId = 'metric',
              label = "Input Metric:",
              choices = c('Hospitalizations', 'Cases'),
              justified = TRUE,
              status = "primary"
            ),

            conditionalPanel(
              "input.metric == 'Hospitalizations'",
              numericInput(
                inputId = 'num_hospitalized',
                label = "Estimate of current inpatients with COVID-19:",
                value = 50
              )
            ),
            conditionalPanel(
              "input.metric != 'Hospitalizations'",
              numericInput(
                inputId = 'num_cases',
                label = "Estimate of number of cases of COVID-19:",
                value = 50
              )
            ),

            dateInput(inputId = 'curr_date', label = "On Date:"),

            uiOutput(outputId = 'prior_val'),

            tags$br(),

            shinyWidgets::materialSwitch(
              inputId = "usedouble",
              label = 'Use doubling time instead of Re',
              status = 'primary'
            ),

            tags$hr(),

            # Interventions
            h4(strong("Add Interventions")),

            checkboxInput(inputId = 'showint', label = 'Add Intervention'),

            uiOutput(outputId = 'intervention_ui'),

            DT::dataTableOutput(outputId = 'int_table'),

            tags$hr(),

            # Influx
            h4(strong("Add Influx of Infections")),

            checkboxInput(inputId = 'showinflux', label = 'Add Influx of Infected Individuals'),

            uiOutput(outputId = 'influx_ui'),

            tags$hr(),

            # Other Settings
            h4(strong("Settings")),

            fluidRow(
              column(
                8,
                strong("Use Markov Model (beta)")
              ),
              column(
                4,
                shinyWidgets::switchInput(inputId = "model_select", value = FALSE)
              )
            ),

            tags$br(),

            sliderInput(
              inputId = 'proj_num_days',
              label = 'Number of Days to Project',
              min = 10,
              max = 730,
              step = 5,
              value = 365
            ),

            bookmarkButton(),

            tags$script(
              "$(document).on('click', '#int_table button', function () {
                Shiny.onInputChange('lastClickId',this.id);
                Shiny.onInputChange('lastClick', Math.random())
                                         });"
            ),

            HTML(end.notes)
          ),
          tabPanel(
            "Parameters",
            uiOutput("params_ui")
          )
        )
      ),

      mainPanel(
        # Day 0 Estimates
        wellPanel(
          uiOutput(outputId = 'infected_ct')
        ),

        # Projections
        wellPanel(
          style = "background: white",

          h3(strong("Projections")),
          shinyWidgets::radioGroupButtons(
            inputId = 'selected_graph',
            label = '',
            choices = c('Cases', 'Hospitalization', 'Hospital Resources'),
            justified = TRUE,
            status = "primary"
          ),
          uiOutput(outputId = 'plot_output'),
          tags$br(),
          fluidRow(
            align = 'center',
            column(
              offset = 1,
              width = 1,
              tags$br(),
              actionButton(
                "goleft",
                "",
                icon = icon("arrow-left"),
                width = '100%'
              )
            ),
            column(
              width = 8,
              uiOutput(outputId = 'description')
            ),
            column(
              width = 1,
              tags$br(),
              actionButton(
                "goright",
                "",
                icon = icon("arrow-right"),
                width = '100%'
              )
            )
          ),

          tags$br(), tags$br(),

          DT::dataTableOutput(outputId = 'rendered.table'),
          downloadButton(outputId = 'downloadData', label = "Download as CSV"),
          tags$br(), tags$br()
        )
      )
    )
  )
}
