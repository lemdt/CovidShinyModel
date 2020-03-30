##  ............................................................................
## All UI elements separated from server.R
##  ............................................................................

# loading language strings
source('wording.R')

# loading inputs
source('inputs.R')

# libraries
library(shiny)
library(shinyjs)


about.page <- modalDialog(
  fluidPage(
    HTML(about.wording)
    
  ),
  size = 'l'
)

prior.re.page <- function(date.select){
  return.page <- fluidPage(
    fluidRow(
      r0.prior.input(date.select),
      pred.re.action(date.select)
    )
  )
  
  return(return.page) 
}

predict.re.page <- function(curr.date){
  return.page <-modalDialog(
    
    useShinyjs(),
    
    HTML(estimate.re.header),
    
    splitLayout(
      
      date.hist.input(curr.date),
      
      hist.hosp.input
      
    ),
    
    add.hist.action,
    
    dataTableOutput(
      outputId = 'input_hosp_dt'
    ), 
    
    tags$script("$(document).on('click', '#input_hosp_dt button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),
    
    HTML('<br>'),
    
    run.fit.action,
    
    div(id = "predict.ui.toggle",
        fluidPage(
          uiOutput('best.re'),
          plotOutput('fit.plot')
        )
        
    ) %>% hidden()
    
  )
}

intervention.ui <- function(curr.date, hosp.delay.time){
  return.page <- fluidPage(
    fluidRow(    
      int.date.input(curr.date, hosp.delay.time),
      uiOutput(outputId = 'int_val'),
      smooth.int.input,
      add.int.action
    )
  )
  
  return(return.page)
}

influx.ui <- function(curr.date, hosp.delay.time){
  return.page <- fluidPage(
    fluidRow(    
      influx.date.input(curr.date, hosp.delay.time),
      num.influx.input
    )
  )
  
  return(return.page)
}

cases.graph.ui <-  fluidPage(
  selected.cases.input,
  plotOutput(outputId = 'cases.plot', 
             click = "plot_click")
  )

hosp.graph.ui <- fluidPage(
  selected.hosp.input,
  plotOutput(outputId = 'hospitalization.plot', 
             click = "plot_click")
  )

res.graph.ui <- function(hosp.avail, icu.avail, vent.avail){
  return.page <- fluidPage(
    column(4, hosp.cap.input(hosp.avail)),
    column(4,icu.cap.input(icu.avail)),
    column(4,vent.cap.input(vent.avail)),
    fluidPage(selected.res.input),
    plotOutput(outputId = 'resource.plot', 
               click = "plot_click")
  )
  
  return(return.page)
}