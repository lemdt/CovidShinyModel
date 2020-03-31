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
               how.to.use.link,
               
               HTML('<br><br>'),
               
               sidebarLayout(
                 sidebarPanel(
                   
                   # Location Information
                   HTML(location.header),
                   num.people.input,
                   input.metric.input,
                   uiOutput(outputId = 'prediction_fld'),
                   curr.date.input,
                   uiOutput(outputId = 'prior_val'),
                   HTML('<br>'),
                   use.double.input,

                   
                   HTML('<br>'),
                   hr(),
                   
                   # Interventions
                   HTML(int.header),
                   show.int.input,
                   uiOutput(outputId = 'intervention_ui'),
                   dataTableOutput(outputId = 'int_table'),
  
                   HTML('<br>'),
                   hr(),
                   
                   # Influx 
                   HTML(influx.header),
                   show.influx.input,
                   uiOutput(outputId = 'influx_ui'),
                   
                   HTML('<br>'),
                   hr(),
                   
                   # Other Settings
                   HTML(settings.wording),
                   proj.num.days.input,
                   other.params.button, 
                   HTML(end.notes),
                   
                   tags$script("$(document).on('click', '#int_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),
                   hr(),
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
                     selected.graph.input,
                     uiOutput(outputId = 'plot_output'),
                     HTML('<br>'),
                     fluidRow(
                       align = 'center',
                       column(width = 1),
                       column(width = 1, HTML('<br>'), go.left.button),
                       column(width = 8,  uiOutput(outputId = 'description')),
                       column(width = 1, HTML('<br>'), go.right.button),
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