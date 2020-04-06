#' @import shiny
ui <- function(req) {
  navbarPage(title = "LEMMA - Local Epidemic Modeling for Management & Action",
             tabPanel("Projections", 
                      fluidPage(theme = shinythemes::shinytheme("sandstone"),
                        tags$head(tags$link(href = "covidshiny/app.css", rel="stylesheet", type="text/css")),
                        shinyjs::useShinyjs(),
                        shinyalert::useShinyalert(),
                        
                        
                        sidebarLayout(
                          sidebarPanel(
                            tabsetPanel(
                              type = "tabs",
                              tabPanel(
                                "Model Inputs",
                                
                                tags$br(),
                                
                                # Location Information
                                h4(strong("Model Inputs")),
                                
                                numericInput(
                                  inputId = 'num_people',
                                  label = "Number of People in Area",
                                  value = 883305
                                ),
                                
                                numericInput(
                                  inputId = 'num_hospitalized',
                                  label = "Estimated number of hospitalized patients with COVID-19",
                                  value = 50
                                ),
                                
                                dateInput(inputId = 'curr_date', label = "On Date:"),
                                
                                conditionalPanel(
                                  "input.usedouble == true",
                                  sliderInput(
                                    inputId = 'doubling_time',
                                    label = "Doubling Time (days) Before ...",
                                    min = 1,
                                    max = 12,
                                    step = 1,
                                    value = 6
                                  )
                                ),
                                conditionalPanel(
                                  "input.usedouble == false",
                                  sliderInput(
                                    inputId = 'r0_prior',
                                    label = "Re Before ...",
                                    min = 0.1,
                                    max = 7,
                                    step = 0.1,
                                    value = 2.8
                                  )
                                ),
                                
                                tags$br(),
                                
                                shinyWidgets::materialSwitch(
                                  inputId = "usedouble",
                                  label = 'Use doubling time instead',
                                  status = 'primary'
                                ),
                                
                                tags$hr(),
                                
                                # Interventions
                                h4(strong("Add Interventions")),
                                
                                checkboxInput(inputId = 'showint', label = 'Add Intervention'),
                                
                                conditionalPanel(
                                  "input.showint == true",
                                  dateInput(
                                    inputId = 'int_date',
                                    label = "Date Intervention is Implemented"
                                  ),
                                  conditionalPanel(
                                    "input.usedouble == true",
                                    sliderInput(
                                      inputId = 'new_double',
                                      label = "New Doubling Time (days) After Interventions",
                                      min = 0,
                                      max = 50,
                                      step = 1,
                                      value = 6
                                    )
                                  ),
                                  conditionalPanel(
                                    "input.usedouble == false",
                                    sliderInput(
                                      inputId = 'r0_new',
                                      label = "New Re After Intervention",
                                      min = 0.1,
                                      max = 6,
                                      step = 0.1,
                                      value = 2.8
                                    )
                                  ),
                                  sliderInput(
                                    'smooth.int',
                                    label = "Smoothed over how many days?",
                                    value = 0,
                                    min = 0,
                                    max = 30
                                  ),
                                  actionButton(inputId = 'add_intervention', label =  "Save Intervention")
                                ),
                                
                                DT::dataTableOutput(outputId = 'int_table'),
                                
                                tags$hr(),
                                
                                # Influx
                                h4(strong("Infected Individuals Entering the Area")),
                                
                                checkboxInput(inputId = 'showinflux', label = 'Add Influx of Infected Individuals'),
                                
                                uiOutput(outputId = 'influx_ui'),
                                
                                tags$hr(),
                                
                                # Other Settings
                                # For the release, we are removing the Markov Model switch for now. 
                                # This is currently a beta feature only for internal use. 
                                # fluidRow(
                                #   column(
                                #     8,
                                #     strong("Use Markov Model (beta)")
                                #   ),
                                #   column(
                                #     4,
                                #     shinyWidgets::switchInput(inputId = "model_select", value = FALSE)
                                #   )
                                # ),
                                
                                sliderInput(
                                  inputId = 'proj_num_days',
                                  label = 'Number of Days to Project',
                                  min = 10,
                                  max = 365,
                                  step = 1,
                                  value = 100
                                ),
                                
                                bookmarkButton(),
                                
                                tags$script(
                                  "$(document).on('click', '#int_table button', function () {
                Shiny.onInputChange('lastClickId',this.id);
                Shiny.onInputChange('lastClick', Math.random())
                                         });"
                                )
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
                              style = "background: white; border-color: #d1d1d1",
                              
                              h3(strong("Projections")),
                              shinyWidgets::radioGroupButtons(
                                inputId = 'selected_graph',
                                label = '',
                                choices = c('Hospitalization', 'Hospital Resources', 'Cases'),
                                justified = TRUE,
                                status = "primary"
                              ),
                              conditionalPanel(
                                "input.selected_graph == 'Hospital Resources'",
                                  splitLayout(
                                    numericInput(
                                      inputId = 'hosp_cap',
                                      label = "Hospital Bed Availability",
                                      value = 1000
                                    ),
                                    numericInput(
                                      inputId = 'icu_cap',
                                      label = "ICU Space Availability",
                                      value = 200
                                    ),
                                    numericInput(
                                      inputId = 'vent_cap',
                                      label = avail.vent.wording ,
                                      value = 100
                                    )
                                  )
                              ),
              
                              splitLayout(
                                       checkboxGroupInput(
                                         inputId = 'selected_lines',
                                         label = 'Selected',
                                         choices = c('Hospital', 'ICU', 'Ventilator'),
                                         selected =  c('Hospital', 'ICU', 'Ventilator'),
                                         inline = TRUE),
                                       div(
                                         id = "freeze-section",
                                         textInput("freeze_name",label = NULL, value = "", placeholder = "Projection name"),
                                         actionButton("freeze_btn", "Freeze"),
                                         actionButton("freeze_reset", "Clear")
                                       )
                                ),
                              plotOutput(outputId = 'rendered_plot',
                                         click = "plot_click"
                              ),
                              
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
                            )
                          )
                        )
                      )
             ),
             tabPanel("About", 
                      wellPanel(h3(strong("About LEMMA")),
                                HTML(what.is.lemma))
             ),
             tags$br(), tags$br()
  )
}
