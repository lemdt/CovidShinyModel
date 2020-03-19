library(shiny)
library(shinyWidgets)

shinyUI(fluidPage(
  
  titlePanel("COVID-19 Epidemic Modeling"),
  
  sidebarLayout(
    sidebarPanel(
      HTML('<h4>Location Information</h4>'),
      
      numericInput(inputId = 'num_people', 
                   label = 'Number of People in Area', 
                   value = 100000),
      
      numericInput(inputId = 'num_hospitalized', 
                   label = 'Number Currently Hospitalized from COVID-19', 
                   value = 2),
      
      uiOutput(outputId = 'prior_val'),
      
      hr(),
      
      HTML('<h4>Efficacy of Interventions</h4>'),

      uiOutput(outputId = 'int_val'),
      
      sliderInput(inputId = 'int_day', 
                  label = 'Day Intervention is Implemented',  
                  min = 0, 
                  max = 365, 
                  step = 1, 
                  value = 1), 
      
      hr(),
      
      HTML('<h4>Settings</h4>'),
      
      materialSwitch(inputId = "usedouble", 
                     label = 'Use doubling time instead of R0', 
                     status = 'primary'),
      
      sliderInput(inputId = 'proj_num_days', 
                  label = 'Number of Days to Project', 
                  min = 10, 
                  max = 730, 
                  step = 5, 
                  value = 365),
      
      actionButton(inputId = 'parameters_modal',
                   label = 'Customize Parameters'), 
      
      HTML('<br><br><b>Notes</b>: This app is a modified version of the <a href="http://penn-chime.phl.io/">Penn Chime app</a>.
                 This is a beta version - the projections may or may not be accurate.')
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
