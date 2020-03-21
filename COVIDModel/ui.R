library(shiny)
library(shinyWidgets)
library(DT)

shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  
  titlePanel("COVID-19 Epidemic Modeling"),
  
  sidebarLayout(
    sidebarPanel(
      HTML('<h4><b>Setting</b></h4>'),
      
      dateInput(inputId = 'curr_date', 
                label = 'Set Day 0 Date',
      ),
      
      numericInput(inputId = 'num_people', 
                   label = 'Number of People in Area', 
                   value = 883305),
      
      # TODO: add in prediction fields for ICU patients, deaths and ventilators
      uiOutput(outputId = 'prediction_fld'),

      uiOutput(outputId = 'prior_val'),
      
      hr(),
      
      HTML('<h4><b>Efficacy of Interventions</b></h4>'),

      uiOutput(outputId = 'int_val'),
      
      sliderInput(inputId = 'int_day', 
                  label = 'Day after Day 0 Intervention is Implemented',  
                  min = 0, 
                  max = 365, 
                  step = 1, 
                  value = 1), 
    
      actionButton(inputId = 'add_intervention', 
                 label = 'Save Intervention'),
      
      dataTableOutput(outputId = 'int_table'),
      
      hr(),
      
      HTML('<h4><b>Settings</b></h4>'),
      
      sliderInput(inputId = 'proj_num_days', 
                  label = 'Number of Days to Project', 
                  min = 10, 
                  max = 730, 
                  step = 5, 
                  value = 365),
      
      materialSwitch(inputId = "usedouble", 
                     label = 'Use doubling time instead of Re', 
                     status = 'primary'),
      
      actionButton(inputId = 'parameters_modal',
                   label = 'Customize Other Parameters'), 
      
      HTML('<br><br><b>Notes</b>: This app is a modified version of the <a href="http://penn-chime.phl.io/">Penn Chime app</a>.
                 This is a beta version - the projections may or may not be accurate.'),
      
      tags$script("$(document).on('click', '#int_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });")
    ),
    
    mainPanel(
      wellPanel(
        htmlOutput(outputId = 'infected_ct')
        ),
      
      radioGroupButtons(inputId = 'selected_graph', 
                        label = '', 
                        choices = c('Cases', 'Hospitalization', 'Hospital Resources'),
                        justified = TRUE, 
                        status = "primary"),
      
      uiOutput(outputId = 'plot_output'),
      
      downloadButton(outputId = 'downloadData', 
                     label = "Download as CSV"),
      
      uiOutput(outputId = 'description')
    )
  )
)
)
