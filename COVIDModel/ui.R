# loading language strings
source('wording.R')

# loading inputs
source('inputsAndPages.R')

# libraries
library(shiny)
library(shinyWidgets)
library(DT)

shinyUI(function(req) {
  tagList(
    
    tags$style(".container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
               @media screen and (min-width: 1300px){
                .container{
                    width: 1300px;
                }
               }"),
    
    tags$div(class="container",
             fluidPage(
               tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #000000;}"))
               ),
               HTML('<br>'),
               
               titlePanel(app.title),
               
               actionLink('howtouse', "Learn more about this tool."),
               
               HTML('<br><br>'),
               
               sidebarLayout(
                 sidebarPanel(
                   
                   # Location Information
                   HTML('<h4><b>Location Information</b></h4>'),
                   numericInput(inputId = 'num_people', 
                                label = "Number of People in Area", 
                                value = 883305),
                   
                   radioGroupButtons(inputId = 'input.metric', 
                                     label = "Input Metric:", 
                                     choices = c('Hospitalizations', 'Cases'),
                                     justified = TRUE, 
                                     status = "primary"),
                   
                   uiOutput(outputId = 'prediction_fld'),
                   
                   dateInput(inputId = 'curr_date', 
                             label = "On Date:"),
                   
                   uiOutput(outputId = 'prior_val'),
                   
                   HTML('<br>'),
                   
                   materialSwitch(inputId = "usedouble", 
                                  label = 'Use doubling time instead of Re', 
                                  status = 'primary'),

                   
                   HTML('<br>'),
                   hr(),
                   
                   # Interventions
                   HTML('<h4><b>Add Interventions</b></h4>'),
                   
                   checkboxInput(inputId = 'showint', 
                                 label = 'Add Intervention'),
                   
                   uiOutput(outputId = 'intervention_ui'),
                   
                   dataTableOutput(outputId = 'int_table'),
  
                   HTML('<br>'),
                   
                   hr(),
                   
                   # Influx 
                   HTML('<h4><b>Add Influx of Infections</b></h4>'),
                   
                   checkboxInput(inputId = 'showinflux', 
                                 label = 'Add Influx of Infected Individuals'),
                   
                   uiOutput(outputId = 'influx_ui'),
                   
                   HTML('<br>'),
                   hr(),
                   
                   # Other Settings
                   HTML('<h4><b>Settings</b></h4>'),
                   
                   fluidRow(column(8, HTML('<b>Use Markov Model (beta)</b> <br>')), 
                            column(4, switchInput(inputId = "model_select", value = FALSE))),
                   
                   HTML('<br>'),
                   
                   sliderInput(inputId = 'proj_num_days', 
                               label = 'Number of Days to Project', 
                               min = 10, 
                               max = 730, 
                               step = 5, 
                               value = 365),
                   
                   actionButton(inputId = 'parameters_modal',
                                label = 'Customize Other Parameters'),
                   
                   bookmarkButton(),
                   
                   tags$script("$(document).on('click', '#int_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),

                   HTML(end.notes),
                 ),
                 
                 mainPanel(
                   
                   # Day 0 Estimates 
                   wellPanel(
                     htmlOutput(outputId = 'infected_ct')
                   ),
                   
                   # Projections
                   wellPanel(
                     style = "background: white",
                     
                     HTML('<h3><b>Projections</b></h3>'),
                     radioGroupButtons(inputId = 'selected_graph', 
                                       label = '', 
                                       choices = c('Cases', 'Hospitalization', 'Hospital Resources'),
                                       justified = TRUE, 
                                       status = "primary"),
                     uiOutput(outputId = 'plot_output'),
                     HTML('<br>'),
                     fluidRow(
                       align = 'center',
                       column(width = 1),
                       column(width = 1, HTML('<br>'), 
                              actionButton("goleft", "", icon = icon("arrow-left"), width = '100%')),
                       column(width = 8,  uiOutput(outputId = 'description')),
                       column(width = 1, HTML('<br>'), 
                              actionButton("goright", "", icon = icon("arrow-right"), width = '100%')),
                       column(width = 1)
                     ),
                     
                     HTML('<br><br>'),
                     
                     div(
                       dataTableOutput(outputId = 'rendered.table'),
                       style = "font-size:110%"),
                     downloadButton(outputId = 'downloadData', 
                                    label = "Download as CSV"),
                     HTML('<br><br>')
                   )
                 )
               )
             )
    )
  )
})