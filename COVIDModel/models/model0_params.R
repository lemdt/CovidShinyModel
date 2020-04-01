##  ............................................................................
##  Model 0 Helper Code
##  ............................................................................

# libraries 
library(shiny)

default.params <- reactiveValues(
  incubation.period = 5,
  sigma = 1/5,
  illness.length = 7,
  gamma.r = 1/7,
  inf.to.hosp = 7, 
  gamma.h = 1/7,
  gamma = 1/7,
  hosp.los = 10, 
  psi = 1/10,
  hosp.rate = 0.06, 
  icu.rate = 0.5, 
  vent.rate = 0.85,
  int.new.r0 = 2.8, 
  int.new.double = 6,
  int.new.num.days = 0, 
  int.smooth.days = 0,
  hosp.avail = 1000, 
  icu.avail = 200, 
  vent.avail = 100
)

parameters.modal <- function(params){
  params.page <- modalDialog(
    fluidPage(
      incubation.period.input(params$incubation.period),
      illness.length.input(params$illness.length),
      hosp.after.inf.input(params$inf.to.hosp),
      hosp.rate.input(params$hosp.rate),
      hosp.los.input.model0(params$hosp.los),
      icu.rate.input(params$icu.rate),
      vent.rate.input(params$vent.rate)
    ),
    footer = tagList(
      save.parameter.action
    )
  )
  
  return(params.page)
}

save.params <- function(params,input){
  params$illness.length <- input$illness.length
  params$gamma.r <- 1/input$illness.length
  params$inf.to.hosp <- input$hosp.after.inf
  params$gamma.h <- 1/input$hosp.after.inf
  params$gamma <- ((1 - input$hosp.rate) * 1/input$illness.length) + 
    (input$hosp.rate * 1/input$hosp.after.inf)
  params$incubation.period <- input$incubation.period
  params$sigma <- 1/input$incubation.period
  params$hosp.los <- input$hosp.los
  params$psi <- 1 / input$hosp.los 
  
  params$hosp.rate <- input$hosp.rate
  params$icu.rate <- input$icu.rate
  params$vent.rate <- input$vent.rate
  
  return(params)
}
