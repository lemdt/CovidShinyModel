##  ............................................................................
## Compilation of all of the inputs that are used in the app
##  ............................................................................

library(shiny)
source('wording.R')

##  ............................................................................
##  Location inputs
##  ............................................................................

num.hosp.input <- numericInput(inputId = 'num_hospitalized', 
                               label = hosp.input.wording, 
                               value = 50)

num.cases.input <- numericInput(inputId = 'num_cases', 
                                label = cases.input.wording, 
                                value = 50)

double.time.input <- function(date.select){
  input.return <- sliderInput(inputId = 'doubling_time', 
                              label = sprintf(prior.double.wording, date.select), 
                              min = 1, 
                              max = 12, 
                              step = 1, 
                              value = 6)
  return(input.return)
}

r0.prior.input <- function(date.select){
  input.return <- sliderInput(inputId = 'r0_prior', 
                              label = sprintf(prior.re.wording, date.select), 
                              min = 0.1, 
                              max = 7, 
                              step = 0.1, 
                              value = 2.8)
  return(input.return)
}

pred.re.action <- function(date.select){
  input.return <- actionLink('predict_re', sprintf(estimate.re.action.wording, date.select))
  
  return(input.return)
}