##  ............................................................................
## Model inputs and modal screens, separated from server.R
##  ............................................................................

# loading language strings
source('wording.R')

# libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)


##  ............................................................................
##  Model Inputs
##  ............................................................................

incubation.period.input <- function(incubation.period){
  input.return <- sliderInput('incubation.period', incubation.input.wording, min = 0, max = 30, step = 1, 
                              value = incubation.period, width = '100%')
  
  return(input.return)
}

illness.length.input <- function(illness.length){
  input.return <- sliderInput('illness.length', infectious.input.wording, min = 0, max = 30, step = 1, 
                              value = illness.length, width = '100%')
  
  return(input.return)
}


hosp.rate.input <- function(hosp.rate){
  input.return <- sliderInput('hosp.rate', per.hosp.wording, min = 0, max = 1, step = 0.01, 
                              value = hosp.rate, width = '100%')
  
  return(input.return)
}


icu.rate.input <- function(icu.rate){
  input.return <- sliderInput('icu.rate', per.icu.wording, min = 0, max = 1, step = 0.01, 
                              value = icu.rate, width = '100%')
  
  return(input.return)
  
}

vent.rate.input <- function(vent.rate){
  input.return <- sliderInput('vent.rate', per.vent.wording, min = 0, max = 1, step = 0.01, 
                              value = vent.rate, width = '100%')
  
  return(input.return)
}


hosp.after.inf.input <- function(hosp.delay.time){
  input.return <- sliderInput('hosp.after.inf', inf.to.hosp.wording, min = 0, max = 30, step = 1, 
                              value = hosp.delay.time, width = '100%')
  
  return(input.return)
  
}

icu.after.hosp.input <- function(icu.delay.time){
  input.return <- sliderInput('icu.after.hosp', hosp.to.icu.wording, min = 0, max = 30, step = 1, 
                              value = icu.delay.time, width = '100%')
  
  return(input.return)
}

vent.after.icu.input <- function(vent.delay.time){
  input.return <- sliderInput('vent.after.icu', icu.to.vent.wording, min = 0, max = 30, step = 1, 
                              value = vent.delay.time, width = '100%')
  
  return(input.return)
}

hosp.los.input <- function(hosp.los){
  input.return <- sliderInput('hosp.los', hosp.los.wording, min = 1, max = 30, step = 1, 
                              value = hosp.los, width = '100%')
  
  return(input.return)
}

hosp.los.input.model0 <- function(hosp.los){
  input.return <- sliderInput('hosp.los', hosp.los.wording.model0, min = 1, max = 30, step = 1, 
                              value = hosp.los, width = '100%')
  
  return(input.return)
}


icu.los.input <- function(icu.los){
  input.return <- sliderInput('icu.los', icu.los.wording, min = 1, max = 30, step = 1,
                              value = icu.los, width = '100%')
  
  return(input.return)
}

vent.los.input <- function(vent.los){
  input.return <-sliderInput('vent.los', vent.los.wording, min = 1, max = 30, step = 1, 
                             value = vent.los, width = '100%')
  
  return(input.return)
  
}

save.parameter.action <- actionButton("save", "Save and Close")

##  ............................................................................
##  Model 2 Parameter Inputs
##  ............................................................................

trans.prob.slider <- function(inputId, label, value, min = 0, max = 1, step = 0.01){
  input.return <- sliderInput(inputId, label, min = min, max = max, 
                              value = value, step = step, width = '100%')
  return(input.return)
}


##  ............................................................................
##  Modals 
##  ............................................................................


about.page <- modalDialog(
  fluidPage(
    HTML(about.wording)
    
  ),
  size = 'l'
)


predict.re.page <- function(curr.date){
  return.page <-modalDialog(
    
    useShinyjs(),
    
    HTML("<h4> Estimate Re based on historical hospitalizations</h4>
         Provide data from past dates to estimate the Re value.<br><br>"),
    
    splitLayout(
      
      dateInput(inputId = 'date.hist', 
                label = 'Date',
                value = curr.date,
                max = curr.date,
                min = curr.date - 14),
      
      numericInput(inputId = 'num.hospitalized.hist', 
                   label = 'Number Hospitalized', 
                   value = NA)
      
      
    ),
    
    actionButton('add.hist', 'Add Data'),
    
    dataTableOutput(
      outputId = 'input_hosp_dt'
    ), 
    
    tags$script("$(document).on('click', '#input_hosp_dt button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                                             Shiny.onInputChange('lastClick', Math.random())
                                             });"),
    
    HTML('<br>'),
    
    actionButton(inputId = 'run.fit', 
                 label = 'Estimate Re'),
    
    div(id = "predict.ui.toggle",
        fluidPage(
          uiOutput('best.re'),
          plotOutput('fit.plot')
        )
        
    ) %>% hidden()
    
  )
}