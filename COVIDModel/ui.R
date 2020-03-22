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
      HTML('<h4><b>Location Information</b></h4>'),
      
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
      
      HTML('<h4><b>Add Interventions</b></h4>'),
      
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
        HTML('<h2><b>Day 0 Estimates</b></h2>'),
        htmlOutput(outputId = 'infected_ct')
      ),
      
      wellPanel(
        style = "background: white",
        HTML('<h2><b>Projections</b></h2>'),
        
        radioGroupButtons(inputId = 'selected_graph', 
                          label = '', 
                          choices = c('Cases', 'Hospitalization', 'Hospital Resources'),
                          justified = TRUE, 
                          status = "primary"),
        
        uiOutput(outputId = 'plot_output'),
        
        HTML('<br>'),
        
        fluidRow(
          align = 'center',
          column(width = 2),
          column(width = 1, 
                 HTML('<br>'),
                 actionButton("goleft", "", icon = icon("arrow-left"), width = '100%')),
          column(width = 6,  
                 uiOutput(outputId = 'description')),
          column(width = 1, 
                 HTML('<br>'),
                 actionButton("goright", "", icon = icon("arrow-right"), width = '100%')),
          column(width = 2)
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
