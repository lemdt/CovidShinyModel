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
               
               actionLink('howtouse', about.link.wording),
               
               HTML('<br><br>'),
               
               sidebarLayout(
                 sidebarPanel(
                   
                   # Location Information
                   HTML(location.header),
                   numericInput(inputId = 'num_people', 
                                label = num.people.wording, 
                                value = 883305),
                   
                   radioGroupButtons(inputId = 'input.metric', 
                                     label = input.metric.wording, 
                                     choices = c('Hospitalizations', 'Cases'),
                                     justified = TRUE, 
                                     status = "primary"),
                   
                   uiOutput(outputId = 'prediction_fld'),
                   
                   dateInput(inputId = 'curr_date', 
                             label = curr.date.wording),
                   
                   uiOutput(outputId = 'prior_val'),
                   
                   HTML('<br>'),
                   
                   materialSwitch(inputId = "usedouble", 
                                  label = use.double.wording, 
                                  status = 'primary'),

                   
                   HTML('<br>'),
                   hr(),
                   
                   # Interventions
                   HTML(int.header),
                   
                   checkboxInput(inputId = 'showint', 
                                 label = add.int.cb.wording),
                   
                   uiOutput(outputId = 'intervention_ui'),
                   
                   dataTableOutput(outputId = 'int_table'),
  
                   HTML('<br>'),
                   
                   hr(),
                   
                   # Influx 
                   HTML(influx.header),
                   
                   checkboxInput(inputId = 'showinflux', 
                                 label = influx.cb.wording),
                   
                   uiOutput(outputId = 'influx_ui'),
                   
                   HTML('<br>'),
                   hr(),
                   
                   # Other Settings
                   HTML(settings.wording),
                   
                   sliderInput(inputId = 'proj_num_days', 
                               label = proj.days.wording, 
                               min = 10, 
                               max = 730, 
                               step = 5, 
                               value = 365),
                   
                   actionButton(inputId = 'parameters_modal',
                                label = cust.params.wording),
                   
                   HTML(end.notes),
                   
                   tags$script("$(document).on('click', '#int_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),
                   hr(),
                   
                   # Bookmarks
                   bookmarkButton()
                 ),
                 
                 mainPanel(
                   
                   # Day 0 Estimates 
                   wellPanel(
                     htmlOutput(outputId = 'infected_ct')
                   ),
                   
                   # Projections
                   wellPanel(
                     style = "background: white",
                     
                     HTML(proj.header),
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
                                    label = download.link.wording),
                     HTML('<br><br>')
                   )
                 )
               )
             )
    )
  )
})